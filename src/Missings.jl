__precompile__(true)
module Missings

using Compat

export allowmissing, coalesce, disallowmissing, ismissing, missing, missings,
       Missing, MissingException, levels, skipmissing

if VERSION < v"0.7.0-DEV.2762"
    """
        Missing

    A type with no fields whose singleton instance [`missing`](@ref) is used
    to represent missing values.
    """
    struct Missing end

    """
        MissingException(msg)

    Exception thrown when a [`missing`](@ref) value is encountered in a situation
    where it is not supported. The error message, in the `msg` field
    may provide more specific details.
    """
    struct MissingException <: Exception
        msg::AbstractString
    end
end

import Base: ==, !=, <, *, <=, !, +, -, ^, /, &, |, xor

if VERSION >= v"0.7.0-DEV.2762"
    using Base: coalesce, ismissing, missing, Missing, MissingException
else
    """
        missing

    The singleton instance of type [`Missing`](@ref) representing a missing value.
    """
    const missing = Missing()

    Base.show(io::IO, x::Missing) = print(io, "missing")

    Base.showerror(io::IO, ex::MissingException) =
        print(io, "MissingException: ", ex.msg)

    ismissing(::Any) = false
    ismissing(::Missing) = true

    Base.promote_rule(::Type{T}, ::Type{Missing}) where {T} = Union{T, Missing}
    Base.promote_rule(::Type{T}, ::Type{Union{S,Missing}}) where {T,S} = Union{promote_type(T, S), Missing}
    Base.promote_rule(::Type{T}, ::Type{Any}) where {T} = Any
    Base.promote_rule(::Type{Any}, ::Type{Missing}) = Any
    Base.promote_rule(::Type{Missing}, ::Type{Any}) = Any
    Base.promote_rule(::Type{Missing}, ::Type{Missing}) = Missing

    Base.convert(::Type{Union{T, Missing}}, x) where {T} = convert(T, x)

    # Comparison operators
    ==(::Missing, ::Missing) = missing
    ==(::Missing, b) = missing
    ==(a, ::Missing) = missing
    # != must be defined explicitly since fallback expects a Bool
    !=(::Missing, ::Missing) = missing
    !=(::Missing, b) = missing
    !=(a, ::Missing) = missing
    Base.isequal(::Missing, ::Missing) = true
    Base.isequal(::Missing, b) = false
    Base.isequal(a, ::Missing) = false
    <(::Missing, ::Missing) = missing
    <(::Missing, b) = missing
    <(a, ::Missing) = missing
    Base.isless(::Missing, ::Missing) = false
    Base.isless(::Missing, b) = false
    Base.isless(a, ::Missing) = true
    if VERSION < v"0.7.0-DEV.300"
        <=(::Missing, ::Missing) = missing
        <=(::Missing, b) = missing
        <=(a, ::Missing) = missing
    end

    # Unary operators/functions
    for f in (:(!), :(+), :(-), :(Base.identity), :(Base.zero), :(Base.one), :(Base.oneunit),
            :(Base.abs), :(Base.abs2), :(Base.sign),
            :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
            :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
            :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
            :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
            :(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
            :(Base.isinteger), :(Base.isreal), :(Base.isimag), :(Base.isnan), :(Base.isempty),
            :(Base.iszero), :(Base.transpose), :(Base.ctranspose), :(Base.float))
        @eval $(f)(d::Missing) = missing
    end

    for f in (:(Base.zero), :(Base.one), :(Base.oneunit))
        @eval function $(f)(::Type{Union{T, Missing}}) where T
            T === Any && throw(MethodError($f, (Any,)))
            $f(T)
        end
    end

    # Binary operators/functions
    for f in (:(+), :(-), :(*), :(/), :(^),
            :(Base.div), :(Base.mod), :(Base.fld), :(Base.rem), :(Base.min), :(Base.max))
        @eval begin
            # Scalar with missing
            ($f)(::Missing, ::Missing) = missing
            ($f)(d::Missing, x::Number) = missing
            ($f)(d::Number, x::Missing) = missing
        end
    end

    # Rounding and related functions
    for f in (:(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc))
        @eval begin
            ($f)(::Missing, digits::Integer=0, base::Integer=0) = missing
            ($f)(::Type{>:Missing}, ::Missing) = missing
            ($f)(::Type{T}, ::Missing) where {T} =
                throw(MissingException("cannot convert a missing value to type $T"))
        end
    end

    # to avoid ambiguity warnings
    (^)(::Missing, ::Integer) = missing

    # Bit operators
    (&)(::Missing, ::Missing) = missing
    (&)(a::Missing, b::Bool) = ifelse(b, missing, false)
    (&)(b::Bool, a::Missing) = ifelse(b, missing, false)
    (&)(::Missing, ::Integer) = missing
    (&)(::Integer, ::Missing) = missing
    (|)(::Missing, ::Missing) = missing
    (|)(a::Missing, b::Bool) = ifelse(b, true, missing)
    (|)(b::Bool, a::Missing) = ifelse(b, true, missing)
    (|)(::Missing, ::Integer) = missing
    (|)(::Integer, ::Missing) = missing
    xor(::Missing, ::Missing) = missing
    xor(a::Missing, b::Bool) = missing
    xor(b::Bool, a::Missing) = missing
    xor(::Missing, ::Integer) = missing
    xor(::Integer, ::Missing) = missing

    # String functions
    *(d::Missing, x::AbstractString) = missing
    *(d::AbstractString, x::Missing) = missing

    # AbstractArray{>:Missing} functions

    function ==(A::AbstractArray{>:Missing}, B::AbstractArray)
        if indices(A) != indices(B)
            return false
        end
        if isa(A,AbstractRange) != isa(B,AbstractRange)
            return false
        end
        anymissing = false
        @inbounds for (a, b) in zip(A, B)
            eq = (a == b)
            if eq === false
                return false
            else
                anymissing |= ismissing(eq)
            end
        end
        return anymissing ? missing : true
    end

    ==(A::AbstractArray, B::AbstractArray{>:Missing}) = (B == A)
    ==(A::AbstractArray{>:Missing}, B::AbstractArray{>:Missing}) =
        invoke(==, Tuple{AbstractArray{>:Missing}, AbstractArray}, A, B)

    !=(x::AbstractArray{>:Missing}, y::AbstractArray) = !(x == y)
    !=(x::AbstractArray, y::AbstractArray{>:Missing}) = !(x == y)
    !=(x::AbstractArray{>:Missing}, y::AbstractArray{>:Missing}) = !(x == y)

    function Base.any(f, A::AbstractArray{>:Missing})
        anymissing = false
        @inbounds for x in A
            v = f(x)
            if v === true
                return true
            else
                anymissing |= ismissing(v)
            end
        end
        return anymissing ? missing : false
    end

    function Base.all(f, A::AbstractArray{>:Missing})
        anymissing = false
        @inbounds for x in A
            v = f(x)
            if v === false
                return false
            else
                anymissing |= ismissing(v)
            end
        end
        return anymissing ? missing : true
    end

    function Base.float(A::AbstractArray{Union{T, Missing}}) where {T}
        U = typeof(float(zero(T)))
        convert(AbstractArray{Union{U, Missing}}, A)
    end
    Base.float(A::AbstractArray{Missing}) = A

    """
        coalesce(x, y...)

    Return the first non-`missing` value in the arguments, or `missing` if all arguments are `missing`.

    In its broadcasted form, this function can be used to replace all missing values
    in an array with a given value (see examples).

    # Examples

    ```jldoctest
    julia> coalesce(missing, 1)
    1

    julia> coalesce(1, missing)
    1

    julia> coalesce(missing, missing)
    missing

    julia> coalesce.([missing, 1, missing], 0)
    3-element Array{$Int,1}:
     0
     1
     0

    julia> coalesce.([missing, 1, missing], [0, 10, 5])
    3-element Array{$Int,1}:
     0
     1
     5

    ```
    """
    coalesce(x) = x
    coalesce(x, y...) = ifelse(x !== missing, x, coalesce(y...))
end

T(::Type{Union{T1, Missing}}) where {T1} = T1
T(::Type{Missing}) = Union{}
T(::Type{T1}) where {T1} = T1
T(::Type{Any}) = Any

# vector constructors
missings(dims...) = fill(missing, dims)
missings(::Type{T}, dims...) where {T >: Missing} = fill!(Array{T}(dims), missing)
missings(::Type{T}, dims...) where {T} = fill!(Array{Union{T, Missing}}(dims), missing)

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
i.e. with an element type equal to `Missings.T(eltype(x))`.

When possible, the result will share memory with `x` (as with [`convert`](@ref)).
If `x` contains missing values, a `MethodError` is thrown.

See also: [`allowmissing`](@ref)
"""
disallowmissing(x::AbstractArray{T}) where {T} = convert(AbstractArray{Missings.T(T)}, x)

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
Base.iteratorsize(::Type{<:EachReplaceMissing{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{<:EachReplaceMissing{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachReplaceMissing) = length(itr.x)
Base.size(itr::EachReplaceMissing) = size(itr.x)
Base.start(itr::EachReplaceMissing) = start(itr.x)
Base.done(itr::EachReplaceMissing, state) = done(itr.x, state)
Base.eltype(itr::EachReplaceMissing) = Missings.T(eltype(itr.x))
@inline function Base.next(itr::EachReplaceMissing, state)
    v, s = next(itr.x, state)
    (v isa Missing ? itr.replacement : v, s)
end

"""
    skipmissing(itr)

Return an iterator wrapping iterable `itr` which skips [`missing`](@ref) values.

Use [`collect`](@ref) to obtain an `Array` containing the non-`missing` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove missings while preserving dimensions
of the input.

# Examples
```jldoctest
julia> collect(skipmissing([1, missing, 2]))
2-element Array{Int64,1}:
 1
 2

julia> collect(skipmissing([1 missing; 2 missing]))
2-element Array{Int64,1}:
 1
 2

```
"""
skipmissing(itr) = EachSkipMissing(itr)

struct EachSkipMissing{T}
    x::T
end
Base.iteratorsize(::Type{<:EachSkipMissing}) =
    Base.SizeUnknown()
Base.iteratoreltype(::Type{EachSkipMissing{T}}) where {T} =
    Base.iteratoreltype(T)
Base.eltype(itr::EachSkipMissing) = Missings.T(eltype(itr.x))
# Fallback implementation for general iterables: we cannot access a value twice,
# so after finding the next non-missing element in start() or next(), we have to
# pass it in the iterator state, which introduces a type instability since the value
# is missing if the input does not contain any non-missing element.
# As of Julia 0.6 and early 0.7, this instability kills performance.
@inline function Base.start(itr::EachSkipMissing)
    s = start(itr.x)
    v = missing
    @inbounds while !done(itr.x, s) && v isa Missing
        v, s = next(itr.x, s)
    end
    (v, s)
end
@inline Base.done(itr::EachSkipMissing, state) = ismissing(state[1]) && done(itr.x, state[2])
@inline function Base.next(itr::EachSkipMissing, state)
    v1, s = state
    v2 = missing
    @inbounds while !done(itr.x, s) && v2 isa Missing
        v2, s = next(itr.x, s)
    end
    (v1, (v2, s))
end
# Optimized implementation for AbstractArray, relying on the ability to access x[i] twice:
# once in done() to find the next non-missing entry, and once in next() to return it.
# This works around the type instability problem of the generic fallback.
@inline function _next_nonmissing_ind(x::AbstractArray, s)
    idx = eachindex(x)
    @inbounds while !done(idx, s)
        i, new_s = next(idx, s)
        x[i] isa Missing || break
        s = new_s
    end
    s
end
@inline Base.start(itr::EachSkipMissing{<:AbstractArray}) =
    _next_nonmissing_ind(itr.x, start(eachindex(itr.x)))
@inline Base.done(itr::EachSkipMissing{<:AbstractArray}, state) =
    done(eachindex(itr.x), state)
@inline function Base.next(itr::EachSkipMissing{<:AbstractArray}, state)
    i, state = next(eachindex(itr.x), state)
    @inbounds v = itr.x[i]::eltype(itr)
    (v, _next_nonmissing_ind(itr.x, state))
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
Base.iteratorsize(::Type{EachFailMissing{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{EachFailMissing{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachFailMissing) = length(itr.x)
Base.size(itr::EachFailMissing) = size(itr.x)
Base.start(itr::EachFailMissing) = start(itr.x)
Base.done(itr::EachFailMissing, state) = done(itr.x, state)
Base.eltype(itr::EachFailMissing) = Missings.T(eltype(itr.x))
@inline function Base.next(itr::EachFailMissing, state)
    v, s = next(itr.x, state)
    # NOTE: v isa Missing currently gives incorrect code, cf. JuliaLang/julia#24177
    ismissing(v) && throw(MissingException("missing value encountered by Missings.fail"))
    (v::eltype(itr), s)
end

"""
    levels(x)

Return a vector of unique values which occur or could occur in collection `x`,
omitting `missing` even if present. Values are returned in the preferred order
for the collection, with the result of [`sort`](@ref) as a default.

Contrary to [`unique`](@ref), this function may return values which do not
actually occur in the data, and does not preserve their order of appearance in `x`.
"""
function levels(x)
    T = Missings.T(eltype(x))
    levs = convert(AbstractArray{T}, filter!(!ismissing, unique(x)))
    if method_exists(isless, Tuple{T, T})
        try; sort!(levs); end
    end
    levs
end

# Deprecations
@deprecate skip(itr) skipmissing(itr) false

end # module
