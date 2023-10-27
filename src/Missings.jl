module Missings

export allowmissing, disallowmissing, ismissing, missing, missings,
       Missing, MissingException, levels, coalesce, passmissing, nonmissingtype,
       skipmissings, spreadmissings

using Base: ismissing, missing, Missing, MissingException
using Base: @deprecate

@static if VERSION < v"1.3.0-alpha.121"
    nonmissingtype(::Type{S}) where {S} = Core.Compiler.typesubtract(S, Missing)
else
    using Base: nonmissingtype
end

import DataAPI

@deprecate T nonmissingtype false

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

Return an iterator wrapping iterable `itr` which replaces [`missing`](@ref) values with
`replacement`. When applicable, the size of `itr` is preserved.
If the type of `replacement` differs from the element type of `itr`,
it will be converted.

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
Base.eltype(itr::EachReplaceMissing) = nonmissingtype(eltype(itr.x))

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

"""
    Missings.fail(itr)

Return an iterator wrapping iterable `itr` which will throw a [`MissingException`](@ref)
if a [`missing`](@ref) value is found.

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
Base.eltype(itr::EachFailMissing) = nonmissingtype(eltype(itr.x))

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

const levels = DataAPI.levels

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
"""
passmissing(f) = PassMissing{Core.Typeof(f)}(f)

struct SpreadMissings{F} <: Function
    f::F
    spread::Symbol
    function SpreadMissings(f, spread)
        if !(spread in (:default, :nonmissing, :none))
            throw(ArgumentError("spread must be either :default, :nonmissing, or :none"))
        end
        new{Core.Typeof(f)}(f, spread)
    end
end

function non_spreadabble_check(t::Union{AbstractDict, NamedTuple, Tuple})
    T = typeof(t)
    s = "Spreadmissings on $T is reserved. Please wrap in Ref to be " *
        "treated as a scalar."
    throw(ArgumentError(s))
end
non_spreadabble_check(x) = nothing

"""
Given an input vector `a` where `nonmissinginds` is guaranteed
to not include any missing values, return a `SubArray` referencing
the `nonmissinginds`. The element type of the returned output
does not include `missing`.
"""
nomissing_subarray(a::AbstractVector, nonmissinginds::AbstractVector) =
    SubArray{nonmissingtype(eltype(a)), 1, typeof(a), Tuple{typeof(nonmissinginds)}, true}(a, (nonmissinginds,), 0, 1)

function maybespread(res, vecs, spread)
    res = f.f(newargs...; new_kwargs...)

    if spread == :default || spread == :nonmissinginds
        out = similar(res, Union{eltype(res), Missing}, length(vecs[1]))
        fill!(out, missing)
        out[nonmissingmask] .= res

        return out
    else
        return res
    end

    return out
end

function (f::SpreadMissings{F})(args...; kwargs...) where {F}
    kwargs_vals = values(values(kwargs))
    xs = tuple(args..., kwargs_vals...)
    foreach(non_spreadabble_check, xs)

    # Detect vector inputs which contain missing in
    # either the main arguments or keyword arguments
    if any(x -> x isa AbstractVector{>:Missing}, xs)
        # Check that all vector inputs have the
        # same indices.
        #
        # TODO: Allow users to protect vector inputs
        vecs = Base.filter(x -> x isa AbstractVector, xs)
        findex = eachindex(first(vecs))
        if !(all(x -> eachindex(x) == findex, vecs[2:end]))
            d = Dict()
            for i in 1:length(vecs)
                e = eachindex(vecs[i])
                if eachindex(e) in keys(d)
                    push!(d[i], i)
                else
                    d[e] = [i]
                end
            end
            s = "The indices of vector-input arguments are not all " *
                "the same.\n"

            for k in keys(d)
                inds = join(d[k], ", ", " and ")
                ind_msg = "Vector inputs $inds have indices $k\n"
                s = s * ind_msg
            end

            throw(ArgumentError(s))
        end

        nonmissingmask = fill(true, length(vecs[1]))
        for v in vecs
            nonmissingmask .&= .!ismissing.(v)
        end
        nonmissinginds = findall(nonmissingmask)
        newargs = ntuple(length(args)) do i
            a = args[i]
            if a isa AbstractVector
                nomissing_subarray(a, nonmissinginds)
            else
                a
            end
        end

        new_kwargs_vals = ntuple(length(kwargs_vals)) do i
            a = kwargs_vals[i]
            if a isa AbstractVector
                nomissing_subarray(a, nonmissinginds)
            else
                a
            end
        end

        new_kwargs = NamedTuple{keys(kwargs)}(new_kwargs_vals)
    else
        return f.f(xs...; kwargs...)
    end
end

"""
    spreadmissings(f; spread = :default)

Given a function `f`, wraps a function `f` but performs a transformation
on arguments before executing. Given the call

```
spreadmissings(f)(x::AbstractVector, y::Integer, z::AbstractVector)
```

will find the indices which corresond to `missing` values in *both*
`x` and `z`. Then apply `f` on the `view`s of `x` and `z` which
contain non-missing values. In essense:

```
inds = .!missing.(x) .& .!missing.(z)
sx = view(x, inds); sy = view(y, inds)
f(sx, y, sy)
```

# Examples
```
julia> x = [0, 1, 2, missing]; y = [-1, 0, missing, 2];

julia> function restricted_fun(x, y)
            map(x, y) do xi, yi
               if xi < 1 || yi < 1 # will error on missings
                   return 1
               else
                   return 2
               end
           end
       end;

julia> spreadmissings(restricted_fun)(x, y)
4-element Vector{Union{Missing, Int64}}:
 1
 1
  missing
  missing
```

`spreadmissings` allows control over how the output from `f` is "spread"
along with respect to missing values.

* `:default`:
  * If `output` is a `Vector` with the same length as the number of
    jointly non-missing elements of the inputs `output` is "spread"
    to match the non-missing elements of the inputs.
  * If the `output` is a `Vector` whose length is not the same
    as the length of number of non-missing elements of the inputs,
    a `DimensionMismatch` error is thrown.
  * If the output is not a `Vector`, `output` is simply returned directly
* `:nonmissing`:
  * If `output` is a `Vector`, behavior is the same as `:default`
  * If `output` is not a `Vector`, `output` is spread along non-missing
    elements of the inputs.
* `:none`: `output` is returned directly, whether a `Vector` or not.

To spread a vector across all non-missing indices of inputs, wrap the
result in `Ref`.
"""
spreadmissings(f; spread = :default) = SpreadMissings(f, spread)

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

end # module
