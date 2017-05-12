module Null

importall Base.Operators

export null, ?

struct NullType end

const null = NullType()

Base.show(io::IO, x::NullType) = print(io, "null")

?{T}(::Type{T}) = Union{T, NullType}
*(::typeof(?), x) = ?(x)
Base.isnull(v::NullType) = true

Base.length(x::NullType) = 1
Base.size(x::NullType) = ()
Base.size(x::NullType, i::Integer) = i < 1 ? throw(BoundsError()) : 1
Base.ndims(x::NullType) = 0
Base.getindex(x::NullType, i) = i == 1 ? null : throw(BoundsError())

Base.promote_rule{T}(::Type{T}, ::Type{NullType} ) = Union{T, NullType}

# Comparison operators
==(::NullType, ::NullType) = true
==(::NullType, b) = false
==(a, ::NullType) = false
<(::NullType, ::NullType) = false
<(::NullType, b) = false
<(a, ::NullType) = true

# Unary operators/functions
for f in (:(Base.:+), :(Base.:-), :(Base.:*), :(Base.:/),
          :(Base.abs), :(Base.abs2), :(Base.sign),
          :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
          :(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc)
         )
    @eval $(f)(d::NullType) = null
end

for f in (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
          :(Base.isinteger), :(Base.isreal), :(Base.isempty))
    @eval $(f)(d::NullType) = false
end

# Binary operators/functions
for f in (:(Base.:+), :(Base.:-), :(Base.:*), :(Base.:/), :(Base.:^),
          :(Base.div), :(Base.mod), :(Base.fld), :(Base.rem), :(Base.min), :(Base.max))
    @eval begin
        # Scalar with null
        ($f)(::NullType, ::NullType) = null
        ($f)(d::NullType, x::Number) = null
        ($f)(d::Number, x::NullType) = null
    end
end

# to avoid ambiguity warnings
(^)(::NullType, ::Integer) = null

# Bit operators
(&)(::NullType, ::NullType) = null
(&)(a::NullType, b::Bool) = b ? null : false
(&)(b::Bool, a::NullType) = b ? null : false
(|)(::NullType, ::NullType) = null
(|)(a::NullType, b::Bool) = b ? true : null
(|)(b::Bool, a::NullType) = b ? true : null
Base.xor(::NullType, ::NullType) = null
Base.xor(a::NullType, b::Bool) = null
Base.xor(b::Bool, a::NullType) = null

# for f in (:(&), :(|), :(Base.xor))
#     @eval begin
#         # Scalar with null
#         ($f)(::NullType, ::NullType) = null
#         ($f)(::NullType, b::Integer) = null
#     end
# end

replace(itr, a, b) = (ifelse(v == a, b, v) for v in itr)
skip(itr, a=null) = (v for v in itr if v != a)

# TODO
 # create a series of benchmarks comparing DataArrays, NullableArrays, Vector{Union{NullType, T}}, and Vector{T}
 # create use-case list for DataFrames: sql-like operations, pivot operations, by, dplyr

end # module
