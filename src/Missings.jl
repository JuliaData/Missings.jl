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
```
"""
passmissing(f) = PassMissing{Core.Typeof(f)}(f)

abstract type AbstractSpread end
struct SpreadDefault <: AbstractSpread end
struct SpreadNonMissing <: AbstractSpread end
struct SpreadNone <: AbstractSpread end
struct SpreadAll <: AbstractSpread end

struct SpreadMissings{F, S <: AbstractSpread} <: Function
    f::F
    spread::S
    function SpreadMissings(f, spread::AbstractSpread)
        if !(spread isa AbstractSpread)
            throw(ArgumentError("spread must be either SpreadDefault(), SpreadNonMissing(), or SpreadNone()"))
        end
        new{Core.Typeof(f), typeof(spread)}(f, spread)
    end
end

"""
    nomissing_subarray(a::AbstractVector, nonmissinginds::AbstractVector)

Given an input vector `a` where `nonmissinginds` is guaranteed
to not include any missing values, return a `SubArray` referencing
the `nonmissinginds`. The element type of the returned output
does not include `missing`.
"""
function nomissing_subarray(a::AbstractVector, nonmissinginds::AbstractVector)
    T = nonmissingtype(eltype(a)) # Element type
    N = 1 # Dimension of view
    P = typeof(a) # Type of parent array
    I = Tuple{typeof(nonmissinginds)} # Type of the non-missing indices
    L = Base.IndexStyle(a) === IndexLinear # If the type supports fast linear indexing
    SubArray{T, N, P, I, L}(a, (nonmissinginds,), 0, 1)
end

function new_args_subarray(args::Tuple, nonmissinginds::AbstractVector)
    newargs = map(args) do a
        if a isa AbstractVector
            nomissing_subarray(a, nonmissinginds)
        else
            a
        end
    end
end

function spread_missing(
    res::AbstractVector{T},
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::AbstractVector{Union{Missing, T}} where {T}

    if length(res) != length(nonmissinginds)
        s = "When spreading a vector result with `spread=$(S)`, " *
            "length of output must match number of jointly non-"
            "missing values in inputs "
            "(got $(length(res)) and $(length(nonmissinginds))).".

        throw(DimensionMismatch(s))
    end
    out = similar(res, Union{eltype(res), Missing}, length(vecs[1]))
    fill!(out, missing)
    out[nonmissingmask] .= res
    out
end

function maybespread_missing(
    res::T,
    spread::SpreadDefault,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::T where{T}

    res
end

function maybespread_missing(
    res::AbstractVector{T},
    spread::SpreadDefault,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::AbstractVector{Union{Missing, T}} where {T}

    spread_missing(res, vecs, nonmissinginds, nonmissingmask)
end

function maybespread_missing(
    res::T,
    spread::SpreadNonMissing,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::AbstractVector{Union{Missing, T}} where{T}

    out = Vector{Union{typeof(res), Missing}}(undef, length(vecs[1]))
    fill!(out, missing)
    out[nonmissinginds] .= Ref(res)
    out
end

function maybespread_missing(
    res::AbstractVector{T},
    spread::SpreadNonMissing,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::AbstractVector{Union{Missing, T}} where {T}

    spread_missing(res, vecs, nonmissinginds, nonmissingmask)
end

function maybespread_missing(
    res::T,
    spread::SpreadNone,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::T where {T}

    res
end

function maybespread_missing(
    res::T,
    spread::SpreadAll,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})::AbstractVector{T} where {T}

    out = Vector{typeof(res)}(undef, length(first(vecs)))
    out .= Ref(res)
    out
end

function maybespread_missing(
    res::AbstractVector,
    spread::SpreadAll,
    vecs::Tuple,
    nonmissinginds::AbstractVector{<:Integer},
    nonmissingmask::AbstractVector{<:Bool})

    throw(ArgumentError("spreadmissings with :all on vector output is reserved"))
end

function spread_nomissing(
    res::AbstractVector{T},
    vecs::Tuple)::typeof(res) where {T}

    if length(res) != length(first(vecs))
        s = "When spreading a vector result with `spread=$(S)`, " *
            "length of output must match number of jointly non-"
            "missing values in inputs "
            "(got $(length(res)) and $(length(first(vecs)))).".
        throw(DimensionMismatch(s))
    end
    res
end

function maybespread_nomissing(
    res::T,
    spread::SpreadDefault,
    vecs::Tuple)::T where{T}

    res
end

function maybespread_nomissing(
    res::AbstractVector{T},
    spread::SpreadDefault,
    vecs::Tuple)::typeof(res) where {T}

    spread_nomissing(res, vecs)
end

function maybespread_nomissing(
    res::T,
    spread::SpreadNonMissing,
    vecs::Tuple)::Vector{T} where{T}

    out = Vector{typeof(res)}(undef, length(vecs[1]))
    fill!(out, res)
    out
end

function maybespread_nomissing(
    res::AbstractVector{T},
    spread::SpreadNonMissing,
    vecs::Tuple)::typeof(res) where {T}

    spread_nomissing(res, vecs)
end

function maybespread_nomissing(
    res::T,
    spread::SpreadNone,
    vecs::Tuple)::T where {T}

    res
end

function maybespread_nomissing(
    res::T,
    spread::SpreadAll,
    vecs::Tuple)::AbstractVector{T} where {T}

    out = Vector{typeof(res)}(undef, length(first(vecs)))
    out .= Ref(res)
    out
end

function maybespread_nomissing(
    res::AbstractVector,
    spread::SpreadAll,
    vecs::Tuple)

    throw(ArgumentError("spreadmissings with :all on vector output is reserved"))
end

function check_indices_match(vecs...)
    Base.require_one_based_indexing(vecs...)
    eachindex(vecs...)
    nothing
end

function (f::SpreadMissings{F, S})(args...; kwargs...) where {F, S}
    kwargs_vals = values(values(kwargs))
    xs = tuple(args..., kwargs_vals...)

    if any(ismissing, xs)
        s = "Using `spreadmissings` with a positional or keyword argumet" *
        " that is `missing` is reserved"
        throw(ArgumentError(s))
    end

    # Detect vector inputs which contain missing in
    # either the main arguments or keyword arguments
    if any(x -> x isa AbstractVector{>:Missing}, xs)
        # Check that all vector inputs have the
        # same indices. Collect these vector inputs
        # into a single object.
        #
        # TODO: Allow users to protect vector inputs
        vecs = Base.filter(x -> x isa AbstractVector, xs)
        check_indices_match(vecs...)
        # Determine which indices in our collection of
        # vector inputs have no missing values in
        # all our inputs.
        nonmissingmask = fill(true, length(vecs[1]))
        for v in vecs
            nonmissingmask .&= .!ismissing.(v)
        end
        nonmissinginds = findall(nonmissingmask)
        # Construct new versions of arguments
        # with SubArrays whose eltypes do not allow Missing
        newargs = new_args_subarray(args, nonmissinginds)
        new_kwargs_vals = new_args_subarray(kwargs_vals, nonmissinginds)

        new_kwargs = (k => v for (k, v) in zip(keys(kwargs), new_kwargs_vals))
        res = f.f(newargs...; new_kwargs...)
        maybespread_missing(res, f.spread, vecs, nonmissinginds, nonmissingmask)
    # There is at least one vector, but none of the vectors can contain missing
    elseif any(x -> x isa AbstractVector, xs)
        vecs = Base.filter(x -> x isa AbstractVector, xs)
        check_indices_match(vecs...)
        res = f.f(args...; kwargs...)
        maybespread_nomissing(res, f.spread, vecs)
    else
        f.f(args...; kwargs...)
    end
end

"""
    spreadmissings(f; spread = :default)

Return a function which calls function `f` after skipping entries
corresponding to missing values in `AbstractVector` arguments.

All input vectors must have the same length. Non-`AbstractVector`
arguments are left untouched.

If `spread` is `:default` or `:nonmissing` and `f` returns a vector,
its length must be equal to the number of jointly non-missing
entries in the vector inputs. A vector of the same length as
vector inputs is returned, filling positions corresponding
to missing values with `missing`.

If `spread` is `:none`, or if `f` returns a value other than a vector,
it is returned as-is.

For each vector argument, `f` is passed a `SubArray`
view with an element type equal to `nonmissingtype(T)`,
with `T` the element type of the original argument.

If none of the input arguments are vectors of any kind,
`spreadmissings(f)` behaves exactly the same as `f`. No
pre or post-processing is done.

### Examples

```julia-repl
julia> using Statistics;

julia> xmiss = [1, 2, 3, missing];

julia> ymiss = [missing, 200, 300, 400];

julia> summeans(x, y) = mean(x) + mean(y);

julia> spreadmissings(summeans)(xmiss, ymiss)
252.5

julia> xmiss = [10, 20, 30, missing];

julia> ymiss = [missing, 500, 400, 300];

julia> cor(xmiss, ymiss)
missing

julia> spreadmissings(cor)(xmiss, ymiss)
-1.0

julia> standardize(xmiss)
4-element Vector{Missing}:
 missing
 missing
 missing
 missing

julia> spreadmissings(standardize)(xmiss)
4-element Vector{Union{Missing, Float64}}:
 -10.0
   0.0
  10.0
    missing
```

# Extended help

The behavior of `spreadmissing` can be illustrated using an example. The call

```
spreadmissings(f)(x::AbstractVector, y::Integer, z::AbstractVector)
```

finds the indices which correspond to `missing` values in *both*
`x` and `z`. Then `f` is applied on the `SubArray`s of `x` and `z` which
contain non-missing values. This is essentially equivalent to:

```
inds = .!missing.(x) .& .!missing.(z)
sx = view(x, inds); sy = view(y, inds)
f(sx, y, sy)
```


`spreadmissings` does not use the default `view` behavior. Rather,
it constructs a `SubArray` directly such that the eltype of the new
inputs do not include `Missing`.

# `spread` keyword argument

The `spread` keyword argument controls whether the output from
`f` is "spread" over non-missing values.

* `:default`:
    * If output is not a vector, it is returned directly.
    * If output is a vector with the same length as the number of
      jointly non-missing elements of the inputs, it is "spread"
      to match the non-missing elements of the inputs.
    * Otherwise a `DimensionMismatch` error is thrown.

* `:nonmissing`:
    * If output is not a vector, it is is spread over non-missing
    elements of the inputs.
    * If output is a vector, behavior is the same as `:default`.
    * If `output` is not a `Vector`, `output` is spread along non-missing
    elements of the inputs.
* `:none`: output is returned directly, whether a vector or not.
* `:all`:
    * If output is not a vector, it is spread over the full
      length of the input vectors, not only the indices with
      missing values with inputs.
    * If the output is a vector, an error is thrown.

A summary of the behavior is given in the table below:

| spread \\ output type  | Vector                          | Non-vector                          |
|:---------------------- |:------------------------------- |:------------------------------------|
| :default               | spread over non-missing indices | return                              |
| :nonmissing            | spread over non-missing indices | spread over non-missing indices     |
| :none                  | return                          | return                              |
| :all                   | error                           | spread over all indices             |

If there are `AbstractVector` inputs but none of these inputs are
`AbstractVector{>:Missing}`, the returned vectors will not allow
for `missing`.

If none of the arguments are `AbstractVector`s, `spreadmissings(f)`
behaves the same as `f` regardless of `spread`.

!!! note
    `spreadmissings` has a subtly different behavior than common uses of
    `skipmissing`. Compare the two functions below

    ```julia-repl
    julia> function fillmean_skip(x)
        m = mean(skipmissing(x))
        fill(m, length(x))
    end;

    julia> fillmean(x) = fill(mean(x), length(x));

    julia> x = [2, missing];

    julia> fillmean_skip(x)
    2-element Vector{Float64}:
     2.0
     2.0

    julia> spreadmissings(fillmean)(x)
    2-element Vector{Union{Missing, Float64}}:
     2.0
      missing
    ```

    1. `fillmean_skip` fills all entries of the original vector `x` with the mean,
       excluding `missing`s. By contrast, `spreadmissings(fillmean)` only fills non-missing
       elements of the original `x`.
    2. `fillmean_skip` returns a vector which does not allow for `missing`, while
       `spreadmissings(fillmean)` does.

    Use the keyword `spread = :all` to emulate the `skipmissing` behavior.
"""
function spreadmissings(f; spread::Symbol = :default)
    if spread === :default
        SpreadMissings(f, SpreadDefault())
    elseif spread === :nonmissing
        SpreadMissings(f, SpreadNonMissing())
    elseif spread === :none
        SpreadMissings(f, SpreadNone())
    elseif spread === :all
        SpreadMissings(f, SpreadAll())
    else
       throw(ArgumentError("`spread` must be one of `:default`, `:nonmissing`, `:none`, or `:all`"))
    end
end

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
