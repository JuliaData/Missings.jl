__precompile__()
module Nulls

using Compat
importall Base.Operators
import Compat: xor, iszero

export null, Null, ?

if VERSION < v"0.6.0-dev.2746"
    immutable Null end
else
    include_string("struct Null end")
end

const null = Null()

Base.show(io::IO, x::Null) = print(io, "null")

?{T}(::Type{T}) = Union{T, Null}
*(::typeof(?), x) = ?(x)
Base.isnull(v::Null) = true

Base.length(x::Null) = 1
Base.size(x::Null) = ()
Base.size(x::Null, i::Integer) = i < 1 ? throw(BoundsError()) : 1
Base.ndims(x::Null) = 0
Base.getindex(x::Null, i) = i == 1 ? null : throw(BoundsError())

# Iteration rules modeled after that for numbers
Base.start(::Null) = false
Base.next(::Null, ::Bool) = (null, true)
Base.done(::Null, b::Bool) = b

Base.promote_rule{T}(::Type{T}, ::Type{Null}) = Union{T, Null}
Base.convert(::Type{Null}, x) = null
Base.convert(::Type{Null}, ::Null) = null

# Comparison operators
==(::Null, ::Null) = true
==(::Null, b) = false
==(a, ::Null) = false
<(::Null, ::Null) = false
<(::Null, b) = false
<(a, ::Null) = true
Base.isless(::Null, ::Null) = false
Base.isless(::Null, b) = false
Base.isless(a, ::Null) = true

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
          :(Compat.iszero))
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

replace(itr, a, b) = (ifelse(v == a, b, v) for v in itr)
replace(itr, b) = replace(itr, b, null)
skip(itr, a=null) = (v for v in itr if v != a)

end # module
