module Nulls

importall Base.Operators

export null, ?

struct Null end

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

Base.promote_rule{T}(::Type{T}, ::Type{Null} ) = Union{T, Null}

# Comparison operators
==(::Null, ::Null) = true
==(::Null, b) = false
==(a, ::Null) = false
<(::Null, ::Null) = null
<(::Null, b) = null
<(a, ::Null) = null
Base.isless(::Null, ::Null) = false
Base.isless(::Null, b) = false
Base.isless(a, ::Null) = true

# Unary operators/functions
for f in (:(Base.:+), :(Base.:-), :(Base.:*), :(Base.:/),
          :(Base.abs), :(Base.abs2), :(Base.sign),
          :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
          :(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc)
         )
    @eval $(f)(d::Null) = null
end

for f in (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
          :(Base.isinteger), :(Base.isreal), :(Base.isempty))
    @eval $(f)(d::Null) = false
end

# Binary operators/functions
for f in (:(Base.:+), :(Base.:-), :(Base.:*), :(Base.:/), :(Base.:^),
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
(&)(a::Null, b::Bool) = b ? null : false
(&)(b::Bool, a::Null) = b ? null : false
(|)(::Null, ::Null) = null
(|)(a::Null, b::Bool) = b ? true : null
(|)(b::Bool, a::Null) = b ? true : null
Base.xor(::Null, ::Null) = null
Base.xor(a::Null, b::Bool) = null
Base.xor(b::Bool, a::Null) = null

# for f in (:(&), :(|), :(Base.xor))
#     @eval begin
#         # Scalar with null
#         ($f)(::Null, ::Null) = null
#         ($f)(::Null, b::Integer) = null
#     end
# end

replace(itr, a, b) = (ifelse(v == a, b, v) for v in itr)
skip(itr, a=null) = (v for v in itr if v != a)

# TODO
 # create a series of benchmarks comparing DataArrays, NullableArrays, Vector{Union{Null, T}}, and Vector{T}
 # create use-case list for DataFrames: sql-like operations, pivot operations, by, dplyr

end # module
