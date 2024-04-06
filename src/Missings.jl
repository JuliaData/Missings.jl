module Missings

export allowmissing, disallowmissing, ismissing, missing, missings,
       Missing, MissingException, levels, coalesce, passmissing, nonmissingtype,
       skipmissings, emptymissing, missingsmallest

using Base: ismissing, missing, Missing, MissingException

@static if VERSION < v"1.3.0-alpha.121"
    nonmissingtype(::Type{S}) where {S} = Core.Compiler.typesubtract(S, Missing)
else
    using Base: nonmissingtype
end

using DataAPI: levels

# vector constructors
missings(dims::Dims) = fill(missing, dims)
missings(::Type{T}, dims::Dims) where {T >: Missing} = fill!(Array{T}(undef, dims), missing)
missings(::Type{T}, dims::Dims) where {T} = fill!(Array{Union{T, Missing}}(undef, dims), missing)
missings(dims::Integer...) = missings(dims)
missings(::Type{T}, dims::Integer...) where {T} = missings(T, dims)

"""
    allowmissing(x::AbstractArray)

Return an array equal to `x` allowing for [`missing`](@ref) values,
i.e. with an element type equal to `Union{eltype(x), Missing}`.

When possible, the result will share memory with `x` (as with [`convert`](@ref)).

See also: [`disallowmissing`](@ref)
"""
allowmissing(x::AbstractArray{T}) where {T} = convert(AbstractArray{Union{T, Missing}}, x)

"""
    disallowmissing(x::AbstractArray)

Return an array equal to `x` not allowing for [`missing`](@ref) values,
i.e. with an element type equal to `nonmissingtype(eltype(x))`.

When possible, the result will share memory with `x` (as with [`convert`](@ref)).
If `x` contains missing values, a `MethodError` is thrown.

See also: [`allowmissing`](@ref)
"""
disallowmissing(x::AbstractArray{T}) where {T} = convert(AbstractArray{nonmissingtype(T)}, x)

# Iterators
"""
    Missings.replace(itr, replacement)

Return an iterator over the elements in `itr` which replaces [`missing`](@ref) values with
`replacement`.

When applicable, the size of `itr` is preserved.
The returned object can be indexed using indices of `itr` if the latter is indexable,
and `eachindex` and `keys` return the indices of `itr`.

If the type of `replacement` differs from the element type of `itr`,
it will be converted to it.

See also: [`skipmissing`](@ref), [`Missings.fail`](@ref)

# Examples
```jldoctest
julia> collect(Missings.replace([1, missing, 2], 0))
3-element Array{Int64,1}:
 1
 0
 2

julia> collect(Missings.replace([1 missing; 2 missing], 0))
2×2 Array{Int64,2}:
 1  0
 2  0

```
"""
replace(itr, replacement) = EachReplaceMissing(itr, convert(eltype(itr), replacement))
struct EachReplaceMissing{T, U}
    x::T
    replacement::U
end
Base.IteratorSize(::Type{<:EachReplaceMissing{T}}) where {T} =
    Base.IteratorSize(T)
Base.IteratorEltype(::Type{<:EachReplaceMissing{T}}) where {T} =
    Base.IteratorEltype(T)
Base.length(itr::EachReplaceMissing) = length(itr.x)
Base.size(itr::EachReplaceMissing) = size(itr.x)
Base.axes(itr::EachReplaceMissing) = axes(itr.x)
Base.eltype(itr::EachReplaceMissing) = nonmissingtype(eltype(itr.x))
Base.eachindex(itr::EachReplaceMissing) = eachindex(itr.x)
Base.keys(itr::EachReplaceMissing) = keys(itr.x)

@inline function Base.iterate(itr::EachReplaceMissing)
    st = iterate(itr.x)
    st === nothing && return nothing
    v, s = st
    return (v isa Missing ? itr.replacement : v, s)
end

@inline function Base.iterate(itr::EachReplaceMissing, state)
    st = iterate(itr.x, state)
    st === nothing && return nothing
    v, s = st
    return (v isa Missing ? itr.replacement : v, s)
end

Base.@propagate_inbounds function Base.getindex(itr::EachReplaceMissing, I...)
    v = itr.x[I...]
    return v === missing ? itr.replacement : v
end

"""
    Missings.fail(itr)

Return an iterator over the elements in `itr` which will throw a [`MissingException`](@ref)
if a [`missing`](@ref) value is found.
The returned object can be indexed using indices of `itr` if the latter is indexable.
Indices corresponding to missing values are not valid: even though they are not skipped
by `keys` and eachindex, a `MissingException` is thrown when trying to use them.

Use [`collect`](@ref) to obtain an `Array` containing the resulting values.
If `itr` is an array, the resulting array will have the same dimensions.

See also: [`skipmissing`](@ref), [`Missings.replace`](@ref)

# Examples
```jldoctest
julia> collect(Missings.fail([1 2; 3 4]))
2×2 Array{Int64,2}:
 1  2
 3  4

julia> collect(Missings.fail([1, missing, 2]))
ERROR: MissingException: missing value encountered by Missings.fail
[...]
```
"""
fail(itr) = EachFailMissing(itr)
struct EachFailMissing{T}
    x::T
end
Base.IteratorSize(::Type{EachFailMissing{T}}) where {T} =
    Base.IteratorSize(T)
Base.IteratorEltype(::Type{EachFailMissing{T}}) where {T} =
    Base.IteratorEltype(T)
Base.length(itr::EachFailMissing) = length(itr.x)
Base.size(itr::EachFailMissing) = size(itr.x)
Base.axes(itr::EachFailMissing) = axes(itr.x)
Base.eltype(itr::EachFailMissing) = nonmissingtype(eltype(itr.x))
Base.eachindex(itr::EachFailMissing) = eachindex(itr.x)
Base.keys(itr::EachFailMissing) = keys(itr.x)

@inline function Base.iterate(itr::EachFailMissing)
    st = iterate(itr.x)
    st === nothing && return nothing
    v, s = st
    ismissing(v) && throw(MissingException("missing value encountered by Missings.fail"))
    return (v::eltype(itr), s)
end

@inline function Base.iterate(itr::EachFailMissing, state)
    st = iterate(itr.x, state)
    st === nothing && return nothing
    v, s = st
    ismissing(v) && throw(MissingException("missing value encountered by Missings.fail"))
    return (v::eltype(itr), s)
end

Base.@propagate_inbounds function Base.getindex(itr::EachFailMissing, I...)
    v = itr.x[I...]
    v === missing && throw(MissingException("the value at index $I is missing"))
    return v
end

struct PassMissing{F} <: Function
    f::F
end

function (f::PassMissing{F})(x) where {F}
    if @generated
        return x === Missing ? missing : :(f.f(x))
    else
        return x === missing ? missing : f.f(x)
    end
end

function (f::PassMissing{F})(xs...) where {F}
    if @generated
        for T in xs
            T === Missing && return missing
        end
        return :(f.f(xs...))
    else
        return any(ismissing, xs) ? missing : f.f(xs...)
    end
end

"""
    passmissing(f)

Return a function that returns `missing` if any of its positional arguments
are `missing` (even if their number or type is not consistent with any of the
methods defined for `f`) and otherwise applies `f` to these arguments.

`passmissing` does not support passing keyword arguments to the `f` function.

# Examples
```jldoctest
julia> passmissing(sqrt)(4)
2.0

julia> passmissing(sqrt)(missing)
missing

julia> passmissing(sqrt).([missing, 4])
2-element Array{Union{Missing, Float64},1}:
  missing
   2.0

julia> passmissing((x,y)->"\$x \$y")(1, 2)
"1 2"

julia> passmissing((x,y)->"\$x \$y")(missing)
missing
```
"""
passmissing(f) = PassMissing{Core.Typeof(f)}(f)

"""
   skipmissings(args...)

Return a tuple of iterators wrapping each of the iterators in `args`, but
skipping elements at positions where at least one of the iterators returns `missing`
(listwise deletion of missing values).

# Examples
```
julia> x = [1, 2, missing, 4]; y = [1, 2, 3, missing];

julia> tx, ty = skipmissings(x, y)
(Missings.SkipMissings{Array{Union{Missing, Int64},1},Tuple{Array{Union{Missing, Int64},1}}}
(Union{Missing, Int64}[1, 2, missing, 4], (Union{Missing, Int64}[1, 2, 3, missing],)), Missi
ngs.SkipMissings{Array{Union{Missing, Int64},1},Tuple{Array{Union{Missing, Int64},1}}}(Union
{Missing, Int64}[1, 2, 3, missing], (Union{Missing, Int64}[1, 2, missing, 4],)))

julia> collect(tx)
2-element Array{Int64,1}:
 1
 2

```
"""
function skipmissings(args...)
    Base.depwarn("Current design of skipmissings is deprecated. In future releases " *
                 "this function may be redesigned or removed", :skipmissings)
    if isempty(args)
        throw(ArgumentError("Must input one or more arguments"))
    end

    if args isa Tuple{Vararg{AbstractArray}}
        if !all(x -> length(x) == length(args[1]), args)
            throw(ArgumentError("All arguments must have the same length"))
        end

        if !all(x -> eachindex(x) == eachindex(args[1]), args)
            throw(ArgumentError("All arguments must have the same indices"))
        end
    end

    ntuple(length(args)) do i
        s = setdiff(1:length(args), i)
        SkipMissings(args[i], args[s])
    end
end

struct SkipMissings{V, T}
    x::V
    others::T
end

Base.@propagate_inbounds function _anymissingindex(others::Tuple{Vararg{AbstractArray}}, i)    
   for oth in others
        oth[i] === missing && return true
    end

    return false
end

@inline function _anymissingiterate(others::Tuple, state)
    for oth in others 
        y = iterate(oth, state)
        y !== nothing && first(y) === missing && return true
    end

    return false
end

const SkipMissingsofArrays = SkipMissings{V, T} where
    {V <: AbstractArray, T <: Tuple{Vararg{AbstractArray}}}

function Base.show(io::IO, mime::MIME"text/plain", itr::SkipMissings{V}) where V 
    print(io, SkipMissings, '{', V, '}', '(', itr.x, ')', " comprised of " *
          "$(length(itr.others) + 1) iterators")
end

Base.IteratorSize(::Type{<:SkipMissings}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:SkipMissings{V}}) where {V} = Base.IteratorEltype(V)
Base.eltype(::Type{<:SkipMissings{V}}) where {V} = nonmissingtype(eltype(V))
Base.IndexStyle(itr::SkipMissings) = Base.IndexStyle(itr.x)

function Base.iterate(itr::SkipMissings, state=1)
    x_itr = iterate(itr.x, state)
    x_itr === nothing && return nothing
    x_item, x_state = x_itr
    while true
        x_item === missing || _anymissingiterate(itr.others, state) || break
        x_itr = iterate(itr.x, x_state)
        x_itr === nothing && return nothing
        state = x_state
        x_item, x_state = x_itr
    end
    return x_item, x_state
end

function Base.iterate(itr::SkipMissingsofArrays, state=0)
    eix = eachindex(itr.x)
    ind_itr = iterate(eix, state)
    ind_itr === nothing && return nothing
    ind_item, ind_state = ind_itr
    @inbounds x_item = itr.x[ind_item]
    @inbounds while true
        x_item === missing || _anymissingindex(itr.others, ind_item) || break
        ind_itr = iterate(eix, ind_state)
        ind_itr === nothing && return nothing
        ind_item, ind_state = ind_itr
        x_item = itr.x[ind_item]
    end
    return x_item, ind_state
end

Base.IndexStyle(::Type{<:SkipMissings{V}}) where {V} = Base.IndexStyle(V)

function Base.eachindex(itr::SkipMissingsofArrays)
    @inbounds Iterators.filter(eachindex(itr.x)) do i
        itr.x[i] !== missing && !_anymissingindex(itr.others, i)
    end
end

function Base.keys(itr::SkipMissingsofArrays)
    @inbounds Iterators.filter(keys(itr.x)) do i
        itr.x[i] !== missing && !_anymissingindex(itr.others, i)
    end
end

@inline function Base.getindex(itr::SkipMissingsofArrays, i)
    @boundscheck checkbounds(itr.x, i)
    @inbounds xi = itr.x[i]
    if xi === missing || @inbounds _anymissingindex(itr.others, i) 
        throw(MissingException("the value at index $i is missing for some element"))
    end
    return xi
end

Base.mapreduce(f, op, itr::SkipMissingsofArrays) =
    Base._mapreduce(f, op, Base.IndexStyle(itr), itr)

function Base._mapreduce(f, op, ::IndexLinear, itr::SkipMissingsofArrays)
    A = itr.x
    local ai
    inds = LinearIndices(A)
    i = first(inds)
    ilast = last(inds)
    @inbounds while i <= ilast
        ai = A[i]
        ai === missing || _anymissingindex(itr.others, i) || break
        i += 1
    end
    i > ilast && return Base.mapreduce_empty(f, op, Base.eltype(itr))
    a1::eltype(itr.x) = ai
    i += 1
    @inbounds while i <= ilast
        ai = A[i]
        ai === missing || _anymissingindex(itr.others, i) || break
        i += 1
    end
    i > ilast && return Base.mapreduce_first(f, op, a1)
    # We know A contains at least two non-missing entries: the result cannot be nothing
    something(Base.mapreduce_impl(f, op, itr, first(inds), last(inds)))
end

Base._mapreduce(f, op, ::IndexCartesian, itr::SkipMissingsofArrays) = mapfoldl(f, op, itr)


Base.mapreduce_impl(f, op, A::SkipMissingsofArrays, ifirst::Integer, ilast::Integer) =
    Base.mapreduce_impl(f, op, A, ifirst, ilast, Base.pairwise_blocksize(f, op))

# Returns nothing when the input contains only missing values, and Some(x) otherwise
@noinline function Base.mapreduce_impl(f, op, itr::SkipMissingsofArrays,
                                       ifirst::Integer, ilast::Integer, blksize::Int)
    A = itr.x
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        if a1 === missing 
            return nothing
        elseif _anymissingindex(itr.others, ifirst) 
            return nothing
        else
            return Some(Base.mapreduce_first(f, op, a1))
        end
    elseif ifirst + blksize > ilast
        # sequential portion
        local ai
        i = ifirst
        @inbounds while i <= ilast
            ai = A[i]
            ai === missing || _anymissingindex(itr.others, i) || break
            i += 1
        end
        i > ilast && return nothing
        a1 = ai::eltype(itr)
        i += 1
        @inbounds while i <= ilast
            ai = A[i]
            ai === missing || _anymissingindex(itr.others, i) || break
            i += 1
        end
        i > ilast && return Some(Base.mapreduce_first(f, op, a1))
        a2 = ai::eltype(itr)
        i += 1
        v = op(f(a1), f(a2))
        @simd for i = i:ilast
            @inbounds ai = A[i]
            ai === missing || @inbounds _anymissingindex(itr.others, i) || (v = op(v, f(ai)))
        end
        return Some(v)
    else
        # pairwise portion
        imid = (ifirst + ilast) >> 1
        v1 = Base.mapreduce_impl(f, op, itr, ifirst, imid, blksize)
        v2 = Base.mapreduce_impl(f, op, itr, imid+1, ilast, blksize)
        if v1 === nothing && v2 === nothing
            return nothing
        elseif v1 === nothing
            return v2
        elseif v2 === nothing
            return v1
        else
            return Some(op(something(v1), something(v2)))
        end
    end
end

"""
    filter(f, itr::SkipMissings)

Return a vector similar to the array wrapped by the given `SkipMissings` iterator
but skipping all elements with a `missing` value in one of the iterators passed
to `skipmissing` and elements for which `f` returns `false`. This method
only applies when all iterators passed to `skipmissings` are arrays.

# Examples
```
julia> x = [missing; 2:9]; y = [1:9; missing];

julia> mx, my = skipmissings(x, y);

julia> filter(isodd, mx)
4-element Array{Int64,1}:
 3
 5
 7
 9

```
"""
function filter(f, itr::SkipMissingsofArrays)
    x = itr.x
    y = similar(x, eltype(itr), 0)
    for i in eachindex(x)
        @inbounds xi = x[i]
        if xi !== missing && @inbounds !_anymissingindex(itr.others, i) && f(xi)
            push!(y, xi)
        end
    end
    y
end

"""
    emptymissing(f)

Return a function that takes at least one positional argument `x` and
returns `missing` if `x` is empty (`isempty(x)` returns `true`)
and otherwise returns `f(x, args...; kwargs...)`, where `args` are following
positional arguments passed to `f` and `kwargs` are its keyword arguments.

# Examples
```
julia> emptymissing(last)([])
missing

julia> emptymissing(last)([1, 2, 3])
3

julia> emptymissing(first)([], 2)
missing

julia> emptymissing(first)([1], 2)
1-element Vector{Int64}:
1
```
"""
emptymissing(f) = (x, args...; kwargs...) -> isempty(x) ? missing : f(x, args...; kwargs...)

# Only for internal use. Allows dispatch over anonymous functions.
struct MissingSmallest{T}
    lt::T
end

"""
    missingsmallest(f)

Return a function of two arguments `x` and `y` that tests whether `x` is less
than `y` such that `missing` is always less than the other argument. In other 
words, return a modified version of the partial order function `f` such that
`missing` is the smallest possible value, and all other non-`missing` values are
compared according to `f`.

The behavior of the standard `isless` function modified to treat `missing` as
the smallest value can be obtained by calling the 2-argument `missingsmallest(x,
y)` function. This is equivalent to `missingsmallest(isless)(x, y)`.

# Examples
```
julia> isshorter = missingsmallest((s1, s2) -> isless(length(s1), length(s2)));

julia> isshorter("short", "longstring")
true

julia> isshorter("longstring", "short")
false

julia> isshorter("", missing)
false
```
"""
missingsmallest(f) = MissingSmallest(f)

"""
    missingsmallest(x, y)

The standard partial order `isless` modified so that `missing` is always the
smallest possible value:
- If neither argument is `missing`, the function behaves exactly as `isless`.
- If `y` is `missing` the result will be `false` regardless of the value of `x`.
- If `x` is `missing` the result will be `true` unless `y` is `missing`.

See also the 1-argument method which takes a partial ordering function (like
`isless`) and modifies it to treat `missing` as explained above. These functions
can be used together with sorting functions so that missing values are sorted
first. This is useful in particular so that when sorting in reverse order
missing values appear at the end.

# Examples
```jldoctest
julia> sort(v, lt=missingsmallest)
5-element Vector{Union{Missing, Int64}}:
   missing
   missing
  1
  2
 10

julia> sort(v, lt=missingsmallest, rev=true)
5-element Vector{Union{Missing, Int64}}:
 10
  2
  1
   missing
   missing

julia> missingsmallest(missing, Inf)
true

julia> missingsmallest(-Inf, missing)
false

julia> missingsmallest(missing, missing)
false
"""
missingsmallest(x, y) = missingsmallest(isless)(x, y)

(ms::MissingSmallest)(x, y) = ismissing(y) ? false : ismissing(x) ? true : ms.lt(x, y)

end # module