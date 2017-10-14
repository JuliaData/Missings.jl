__precompile__(true)
module Nulls

import Base: *, <, ==, !=, <=, !, +, -, ^, /, &, |, xor
using Compat: AbstractRange

export null, nulls, Null, levels

struct Null end

const null = Null()

Base.show(io::IO, x::Null) = print(io, "null")

T(::Type{Union{T1, Null}}) where {T1} = T1
T(::Type{Null}) = Union{}
T(::Type{T1}) where {T1} = T1
T(::Type{Any}) = Any

Base.isnull(v::Null) = true

# vector constructors
nulls(dims...) = fill(null, dims)
nulls(::Type{T}, dims...) where {T >: Null} = fill!(Array{T}(dims), null)
nulls(::Type{T}, dims...) where {T} = fill!(Array{Union{T, Null}}(dims), null)

Base.promote_rule(::Type{T}, ::Type{Null}) where {T} = Union{T, Null}
Base.promote_rule(::Type{T}, ::Type{Union{S,Null}}) where {T,S} = Union{promote_type(T, S), Null}
Base.promote_rule(::Type{T}, ::Type{Any}) where {T} = Any
Base.promote_rule(::Type{Any}, ::Type{Null}) = Any
Base.promote_rule(::Type{Null}, ::Type{Any}) = Any
Base.promote_rule(::Type{Null}, ::Type{Null}) = Null
Base.convert(::Type{Union{T, Null}}, x) where {T} = convert(T, x)

# Comparison operators
==(::Null, ::Null) = null
==(::Null, b) = null
==(a, ::Null) = null
# != must be defined explicitly since fallback expects a Bool
!=(::Null, ::Null) = null
!=(::Null, b) = null
!=(a, ::Null) = null
Base.isequal(::Null, ::Null) = true
Base.isequal(::Null, b) = false
Base.isequal(a, ::Null) = false
<(::Null, ::Null) = null
<(::Null, b) = null
<(a, ::Null) = null
Base.isless(::Null, ::Null) = false
Base.isless(::Null, b) = false
Base.isless(a, ::Null) = true
if VERSION < v"0.7.0-DEV.300"
    <=(::Null, ::Null) = null
    <=(::Null, b) = null
    <=(a, ::Null) = null
end

# Unary operators/functions
for f in (:(!), :(+), :(-), :(Base.identity), :(Base.zero),
          :(Base.abs), :(Base.abs2), :(Base.sign),
          :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
          :(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
          :(Base.isinteger), :(Base.isreal), :(Base.isimag), :(Base.isnan), :(Base.isempty),
          :(Base.iszero), :(Base.transpose), :(Base.ctranspose))
    @eval $(f)(d::Null) = null
end

Base.zero(::Type{Union{T, Null}}) where {T <: Number} = zero(T)
Base.zero(::Type{Union{T, Null}}) where {T <: Base.Dates.Period} = zero(T)

# Binary operators/functions
for f in (:(+), :(-), :(*), :(/), :(^),
          :(Base.div), :(Base.mod), :(Base.fld), :(Base.rem), :(Base.min), :(Base.max))
    @eval begin
        # Scalar with null
        ($f)(::Null, ::Null) = null
        ($f)(d::Null, x::Number) = null
        ($f)(d::Number, x::Null) = null
    end
end

# Rounding and related functions
for f in (:(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc))
    @eval begin
        ($f)(::Null, digits::Integer=0, base::Integer=0) = null
        ($f)(::Type{>:Null}, ::Null) = null
        ($f)(::Type, ::Null) = throw(NullException())
    end
end

# to avoid ambiguity warnings
(^)(::Null, ::Integer) = null

# Bit operators
(&)(::Null, ::Null) = null
(&)(a::Null, b::Bool) = ifelse(b, null, false)
(&)(b::Bool, a::Null) = ifelse(b, null, false)
(&)(::Null, ::Integer) = null
(&)(::Integer, ::Null) = null
(|)(::Null, ::Null) = null
(|)(a::Null, b::Bool) = ifelse(b, true, null)
(|)(b::Bool, a::Null) = ifelse(b, true, null)
(|)(::Null, ::Integer) = null
(|)(::Integer, ::Null) = null
xor(::Null, ::Null) = null
xor(a::Null, b::Bool) = null
xor(b::Bool, a::Null) = null
xor(::Null, ::Integer) = null
xor(::Integer, ::Null) = null

# String functions
*(d::Null, x::AbstractString) = null
*(d::AbstractString, x::Null) = null

# Iterators
"""
    Nulls.replace(itr, replacement)

Return an iterator wrapping iterable `itr` which replaces [`null`](@ref) values with
`replacement`. When applicable, the size of `itr` is preserved.

See also: [`Nulls.skip`](@ref), [`Nulls.fail`](@ref)

# Examples
```jldoctest
julia> collect(Nulls.replace([1, null, 2], 0))
3-element Array{Int64,1}:
 1
 0
 2

julia> collect(Nulls.replace([1 null; 2 null], 0))
2×2 Array{Int64,2}:
 1  0
 2  0

```
"""
replace(itr, replacement) = EachReplaceNull(itr, replacement)
struct EachReplaceNull{T, U}
    x::T
    replacement::U
end
Base.iteratorsize(::Type{<:EachReplaceNull{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{<:EachReplaceNull{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachReplaceNull) = length(itr.x)
Base.size(itr::EachReplaceNull) = size(itr.x)
Base.start(itr::EachReplaceNull) = start(itr.x)
Base.done(itr::EachReplaceNull, state) = done(itr.x, state)
Base.eltype(itr::EachReplaceNull) =
    Union{Nulls.T(eltype(itr.x)), typeof(itr.replacement)}
@inline function Base.next(itr::EachReplaceNull, state)
    v, s = next(itr.x, state)
    ((isnull(v) ? itr.replacement : v)::eltype(itr), s)
end

"""
    Nulls.skip(itr)

Return an iterator wrapping iterable `itr` which skips [`null`](@ref) values.

Use [`collect`](@ref) to obtain an `Array` containing the non-`null` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove nulls while preserving dimensions
of the input.

See also: [`Nulls.replace`](@ref), [`Nulls.fail`](@ref)

# Examples
```jldoctest
julia> collect(Nulls.skip([1, null, 2]))
2-element Array{Int64,1}:
 1
 2

julia> collect(Nulls.skip([1 null; 2 null]))
2-element Array{Int64,1}:
 1
 2

```
"""
skip(itr) = EachSkipNull(itr)
struct EachSkipNull{T}
    x::T
end
Base.iteratorsize(::Type{<:EachSkipNull}) =
    Base.SizeUnknown()
Base.iteratoreltype(::Type{EachSkipNull{T}}) where {T} =
    Base.iteratoreltype(T)
Base.eltype(itr::EachSkipNull) = Nulls.T(eltype(itr.x))
# Fallback implementation for general iterables: we cannot access a value twice,
# so after finding the next non-null element in start() or next(), we have to
# pass it in the iterator state, which introduces a type instability since the value
# is null if the input does not contain any non-null element.
# As of Julia 0.6 and early 0.7, this instability kills performance.
@inline function Base.start(itr::EachSkipNull)
    s = start(itr.x)
    v = null
    @inbounds while !done(itr.x, s) && isnull(v)
        v, s = next(itr.x, s)
    end
    (v, s)
end
@inline Base.done(itr::EachSkipNull, state) = isnull(state[1]) && done(itr.x, state[2])
@inline function Base.next(itr::EachSkipNull, state)
    v1, s = state
    v2 = null
    @inbounds while !done(itr.x, s) && isnull(v2)
        v2, s = next(itr.x, s)
    end
    (v1::eltype(itr), (v2, s))
end
# Optimized implementation for AbstractArray, relying on the ability to access x[i] twice:
# once in done() to find the next non-null entry, and once in next() to return it.
# This works around the type instability problem of the generic fallback.
@inline function _next_nonnull_ind(x::AbstractArray, s)
    idx = eachindex(x)
    @inbounds while !done(idx, s)
        i, new_s = next(idx, s)
        isnull(x[i]) || break
        s = new_s
    end
    s
end
@inline Base.start(itr::EachSkipNull{<:AbstractArray}) =
    _next_nonnull_ind(itr.x, start(eachindex(itr.x)))
@inline Base.done(itr::EachSkipNull{<:AbstractArray}, state) =
    done(eachindex(itr.x), state)
@inline function Base.next(itr::EachSkipNull{<:AbstractArray}, state)
    i, state = next(eachindex(itr.x), state)
    @inbounds v = itr.x[i]::eltype(itr)
    (v, _next_nonnull_ind(itr.x, state))
end

"""
    Nulls.fail(itr)

Return an iterator wrapping iterable `itr` which will throw a [`NullException`](@ref)
if a [`null`](@ref) value is found.

Use [`collect`](@ref) to obtain an `Array` containing the resulting values.
If `itr` is an array, the resulting array will have the same dimensions.

See also: [`Nulls.skip`](@ref), [`Nulls.replace`](@ref)

# Examples
```jldoctest
julia> collect(Nulls.fail([1 2; 3 4]))
2×2 Array{Int64,2}:
 1  2
 3  4

julia> collect(Nulls.fail([1, null, 2]))
ERROR: NullException()
[...]
```
"""
fail(itr) = EachFailNull(itr)
struct EachFailNull{T}
    x::T
end
Base.iteratorsize(::Type{EachFailNull{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{EachFailNull{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachFailNull) = length(itr.x)
Base.size(itr::EachFailNull) = size(itr.x)
Base.start(itr::EachFailNull) = start(itr.x)
Base.done(itr::EachFailNull, state) = done(itr.x, state)
Base.eltype(itr::EachFailNull) = Nulls.T(eltype(itr.x))
@inline function Base.next(itr::EachFailNull, state)
    v, s = next(itr.x, state)
    isnull(v) && throw(NullException())
    (v::eltype(itr), s)
end

"""
    coalesce(x, y...)

Return the first non-`null` value in the arguments, or `null` if all arguments are `null`.

In its broadcasted form, this function can be used to replace all null values
in an array with a given value (see examples).

# Examples

```jldoctest
julia> coalesce(null, 1)
1

julia> coalesce(1, null)
1

julia> coalesce(null, null)
null

julia> coalesce.([null, 1, null], 0)
3-element Array{$Int,1}:
 0
 1
 0

julia> coalesce.([null, 1, null], [0, 10, 5])
3-element Array{$Int,1}:
 0
 1
 5

```
"""
coalesce(x) = x
coalesce(x, y...) = ifelse(x !== null, x, coalesce(y...))

"""
    levels(x)

Return a vector of unique values which occur or could occur in collection `x`,
omitting `null` even if present. Values are returned in the preferred order
for the collection, with the result of [`sort`](@ref) as a default.

Contrary to [`unique`](@ref), this function may return values which do not
actually occur in the data, and does not preserve their order of appearance in `x`.
"""
function levels(x)
    T = Nulls.T(eltype(x))
    levs = convert(AbstractArray{T}, filter!(!isnull, unique(x)))
    if method_exists(isless, Tuple{T, T})
        try; sort!(levs); end
    end
    levs
end

# AbstractArray{>:Null} functions

function ==(A::AbstractArray{>:Null}, B::AbstractArray)
    if indices(A) != indices(B)
        return false
    end
    if isa(A,AbstractRange) != isa(B,AbstractRange)
        return false
    end
    anynull = false
    @inbounds for (a, b) in zip(A, B)
        eq = (a == b)
        if eq === false
            return false
        else
            anynull |= isnull(eq)
        end
    end
    return anynull ? null : true
end

==(A::AbstractArray, B::AbstractArray{>:Null}) = (B == A)
==(A::AbstractArray{>:Null}, B::AbstractArray{>:Null}) =
    invoke(==, Tuple{AbstractArray{>:Null}, AbstractArray}, A, B)

!=(x::AbstractArray{>:Null}, y::AbstractArray) = !(x == y)
!=(x::AbstractArray, y::AbstractArray{>:Null}) = !(x == y)
!=(x::AbstractArray{>:Null}, y::AbstractArray{>:Null}) = !(x == y)

function Base.any(f, A::AbstractArray{>:Null})
    anynull = false
    @inbounds for x in A
        v = f(x)
        if v === true
            return true
        else
            anynull |= isnull(v)
        end
    end
    return anynull ? null : false
end

function Base.all(f, A::AbstractArray{>:Null})
    anynull = false
    @inbounds for x in A
        v = f(x)
        if v === false
            return false
        else
            anynull |= isnull(v)
        end
    end
    return anynull ? null : true
end

end # module
