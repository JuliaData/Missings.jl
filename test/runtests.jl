using Base.Test, Nulls

@testset "Nulls" begin

    bit_operators = [&, |, ⊻]

    arithmetic_operators = [+, -, *, /, ^, Base.div, Base.mod, Base.fld, Base.rem]

    elementary_functions = [abs, abs2, sign,
                            acos, acosh, asin, asinh, atan, atanh, sin, sinh,
                            conj, cos, cosh, tan, tanh,
                            ceil, floor, round, trunc,
                            exp, exp2, expm1, log, log10, log1p, log2,
                            exponent, sqrt, gamma, lgamma,
                            identity]

    boolean_functions = [iseven, isodd, ispow2,
                         isfinite, isinf, isnan, iszero,
                         isinteger, isreal, isimag,
                         isempty]

    # All unary operators return null when evaluating null
    for f in [+, -]
        @test isnull(f(null))
    end

    # All elementary functions return null when evaluating null
    for f in elementary_functions
        @test isnull(f(null))
    end

    # All boolean functions return false when evaluating null
    for f in boolean_functions
        @test !f(null)
    end

    # Comparison operators
    @test null == null
    @test !(1 == null)
    @test !(null == 1)
    @test !(null != null)
    @test 1 != null
    @test null != 1
    @test !(null < null)
    @test !(null < 1)
    @test !(1 < null)
    @test null <= null
    @test !(null <= 1)
    @test !(1 <= null)
    @test !isless(null, null)
    @test !isless(null, 1)
    @test isless(1, null)

    # All arithmetic operators return null when operating on two null's
    # All arithmetic operators return null when operating on a scalar and an null
    # All arithmetic operators return null when operating on an null and a scalar
    for f in arithmetic_operators
        @test isnull(f(null, null))
        @test isnull(f(1, null))
        @test isnull(f(null, 1))
    end

    # All bit operators return null when operating on two null's
    for f in bit_operators
        @test isnull(f(null, null))
    end

    @test isnull(null & true)
    @test isnull(true & null)
    @test !(null & false)
    @test !(false & null)
    @test isnull(null | false)
    @test isnull(false | null)
    @test null | true
    @test true | null
    @test isnull(xor(null, true))
    @test isnull(xor(true, null))
    @test isnull(xor(null, false))
    @test isnull(xor(false, null))

    @test length(null) == 1
    @test size(null) == ()
    @test size(null, 1) == 1
    @test_throws BoundsError size(null, 0)
    @test ndims(null) == 0
    @test null[1] == null
    @test_throws BoundsError null[2]

    @test eltype([1, 2, null]) == Union{Int, Null}

    @test sprint(show, null) == "null"

    # Iteration over scalars works as with numbers
    for i in null
        @test isnull(i)
    end
    @test !start(null)
    let (a, b) = next(null, true)
        @test isnull(a) && b
    end
    let (a, b) = next(null, false)
        @test isnull(a) && b
    end
    @test done(null, true)
    @test !done(null, false)

    @test collect(Nulls.replace([1, 2, null, 4], 3)) == collect(1:4)
    @test collect(Nulls.skip([1, 2, null, 4])) == [1, 2, 4]
    @test collect(Nulls.fail([1, 2, 3, 4])) == [1, 2, 3, 4]
    @test_throws NullException collect(Nulls.fail([1, 2, null, 4]))

    @test Nulls.coalesce(null, 1) === 1
    @test Nulls.coalesce(1, null) === 1
    @test Nulls.coalesce(null, null) === null
    @test Nulls.coalesce.([null, 1, null], 0) == [0, 1, 0]
    @test Nulls.coalesce.([null, 1, null], 0) isa Vector{Int}
    @test Nulls.coalesce.([null, 1, null], [0, 10, 5]) == [0, 1, 5]
    @test Nulls.coalesce.([null, 1, null], [0, 10, 5]) isa Vector{Int}
    @test Nulls.coalesce.([null, 1, null], [0, null, null]) == [0, 1, null]
    # Fails in Julia 0.6 and 0.7.0-DEV.1556
    @test_broken Nulls.coalesce.([null, 1, null], [0, null, null]) isa Vector{Union{Null, Int}}

    x = convert(Vector{Union{Int, Null}}, [1.0, null])
    @test isa(x, Vector{Union{Int, Null}})
    @test x == [1, null]
    x = convert(Vector{Union{Int, Null}}, [1.0])
    @test isa(x, Vector{Union{Int, Null}})
    @test x == [1]
    x = convert(Vector{Union{Int, Null}}, [null])
    @test isa(x, Vector{Union{Int, Null}})
    @test x == [null]

    @test Nulls.T(Union{Int, Null}) == Int

    @test nulls(1) == [null]
    @test nulls(Int, 1) == (Union{Int, Null})[null]
    @test nulls(Union{Int, Null}, 1, 2) == (Union{Int, Null})[null null]
    @test Union{Int, Null}[1,2,3] == (Union{Int, Null})[1,2,3]

    @test convert(Union{Int, Null}, 1.0) == 1

    @test Nulls.T(Any) == Any
end
