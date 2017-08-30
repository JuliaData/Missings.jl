__precompile__(true)
module Nulls

import Base: *, <, ==, <=, +, -, ^, /, &, |, xor

export null, nulls, Null

struct Null end

const null = Null()

Base.show(io::IO, x::Null) = print(io, "null")

T(::Type{Union{T1, Null}}) where {T1} = T1
T(::Type{T1}) where {T1} = T1
T(::Type{Any}) = Any

Base.isnull(v::Null) = true

Base.length(x::Null) = 1
Base.size(x::Null) = ()
Base.size(x::Null, i::Integer) = i < 1 ? throw(BoundsError()) : 1
Base.ndims(x::Null) = 0
Base.getindex(x::Null, i) = i == 1 ? null : throw(BoundsError())

# vector constructors
nulls(dims...) = fill(null, dims)
nulls(::Type{T}, dims...) where {T >: Null} = fill!(Array{T}(dims), null)
nulls(::Type{T}, dims...) where {T} = fill!(Array{Union{T, Null}}(dims), null)

# Iteration rules modeled after that for numbers
Base.start(::Null) = false
Base.next(::Null, ::Bool) = (null, true)
Base.done(::Null, b::Bool) = b

Base.promote_rule(::Type{T}, ::Type{Null}) where {T} = Union{T, Null}
Base.convert(::Type{Union{T, Null}}, x) where {T} = convert(T, x)

# Comparison operators
<(::Null, ::Null) = false
<(::Null, b) = false
<(a, ::Null) = false
Base.isless(::Null, ::Null) = false
Base.isless(::Null, b) = false
Base.isless(a, ::Null) = true
if VERSION < v"0.7.0-DEV.300"
    <=(::Null, ::Null) = true
    <=(::Null, b) = false
    <=(a, ::Null) = false
end

# Unary operators/functions
for f in (:(+), :(-), :(Base.identity),
          :(Base.abs), :(Base.abs2), :(Base.sign),
          :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
          :(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc))
    @eval $(f)(d::Null) = null
end

for f in (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
          :(Base.isinteger), :(Base.isreal), :(Base.isimag), :(Base.isnan), :(Base.isempty),
          :(Base.iszero))
    @eval $(f)(d::Null) = false
end

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

# to avoid ambiguity warnings
(^)(::Null, ::Integer) = null

# Bit operators
(&)(::Null, ::Null) = null
(&)(a::Null, b::Bool) = ifelse(b, null, false)
(&)(b::Bool, a::Null) = ifelse(b, null, false)
(|)(::Null, ::Null) = null
(|)(a::Null, b::Bool) = ifelse(b, true, null)
(|)(b::Bool, a::Null) = ifelse(b, true, null)
xor(::Null, ::Null) = null
xor(a::Null, b::Bool) = null
xor(b::Bool, a::Null) = null

replace(itr, x) = (ifelse(v !== null, v, x) for v in itr)
skip(itr) = (v for v in itr if v !== null)

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

end # module
