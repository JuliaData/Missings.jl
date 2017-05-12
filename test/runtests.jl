using Base.Test, Nulls
using Compat

@testset "Nulls" begin

    const bit_operators = [&, |, ⊻]

    const arithmetic_operators = [+, -, *, /, ^, Base.div, Base.mod, Base.fld, Base.rem]

    const elementary_functions = [abs, abs2, sign,
                                  acos, acosh, asin, asinh, atan, atanh, sin, sinh,
                                  conj, cos, cosh, tan, tanh,
                                  ceil, floor, round, trunc,
                                  exp, exp2, expm1, log, log10, log1p, log2,
                                  exponent, sqrt, gamma, lgamma]

    # All unary operators return null when evaluating null
    for f in [+, -]
        @test isnull(f(null))
    end

    # All elementary functions return null when evaluating null
    for f in elementary_functions
        @test isnull(f(null))
    end

    # Comparison operators
    @test null == null
    @test !(1 == null)
    @test !(null == 1)
    @test !(null != null)
    @test 1 != null
    @test null != 1
    @test isnull(null < null)
    @test isnull(null < 1)
    @test isnull(1 < null)
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

    @test eltype([1, 2, null]) == ?Int
end
