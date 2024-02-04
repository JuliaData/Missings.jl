module SpreadMissingTests

using Test, Missings, CategoricalArrays

const ≈ = isequal

function small_vec(args...; kwargs...)
    [1, 2]
end

function right_vec(args...; kwargs...)
    kwargs_vals = values(values(kwargs))
    xs = tuple(args..., kwargs_vals...)
    vecs = Base.filter(x -> x isa AbstractVector, xs)
    if !isempty(vecs)
        collect(1:length(first(vecs)))
    else
        [-1]
    end
end

function scalar(args...; kwargs...)
    1
end

xmiss = [1, 2, 3, missing]
x = [1, 2, 3, 4]

ymiss = [missing, 200, 300, 400]
y = [100, 200, 300, 400]

s = [1000, 2000]

@testset "vector, :default" begin
    ### missings in main arg only
    t = spreadmissings(right_vec)(xmiss)
    @test t ≈ [1, 2, 3, missing]
    ### missing in keyword arg only
    t = spreadmissings(right_vec)(; z = ymiss)
    @test t ≈ [missing, 1, 2, 3]
    ### missing in both main arg and keyword arg
    t = spreadmissings(right_vec)(x, xmiss; z = ymiss)
    @test t ≈ [missing, 1, 2, missing]
    ### missings nowhere
    t = spreadmissings(right_vec)(x; z = y)
    @test t ≈ [1, 2, 3, 4]
    @test t isa Vector{Int}
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(right_vec)(x, s)
    ### no vectors
    t = spreadmissings(right_vec)(1, 2; z = 9)
    @test t == [-1]
end

@testset "vector, :nonmissing" begin
    ### missings in main arg only
    t = spreadmissings(right_vec; spread = :nonmissing)(xmiss)
    @test t ≈ [1, 2, 3, missing]
    ### missing in keyword arg only
    t = spreadmissings(right_vec; spread = :nonmissing)(; z = ymiss)
    @test t ≈ [missing, 1, 2, 3]
    ### missing in both main arg and keyword arg
    t = spreadmissings(right_vec; spread = :nonmissing)(x, xmiss; z = ymiss)
    @test t ≈ [missing, 1, 2, missing]
    ### missings nowhere
    t = spreadmissings(right_vec; spread = :nonmissing)(x; z = y)
    @test t ≈ [1, 2, 3, 4]
    @test t isa Vector{Int}
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(right_vec; spread = :nonmissing)(x, s)
    ### no vectors
    t = spreadmissings(right_vec)(1, 2; z = 9)
    @test t == [-1]
end

@testset "vector, :none" begin
    t = spreadmissings(right_vec; spread = :none)(xmiss)
    @test t ≈ [1, 2, 3]
    ### missing in keyword arg only
    t = spreadmissings(right_vec; spread = :none)(; z = ymiss)
    @test t ≈ [1, 2, 3]
    ### missing in both main arg and keyword arg
    t = spreadmissings(right_vec; spread = :none)(x, xmiss; z = ymiss)
    @test t ≈ [1, 2]
    ### missings nowhere
    t = spreadmissings(right_vec; spread = :none)(x; z = y)
    @test t ≈ [1, 2, 3, 4]
    @test t isa Vector{Int}
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(right_vec; spread = :none)(x, s)
end

@testset "vector, :all" begin
    ### missings in main arg only
    @test_throws ArgumentError spreadmissings(right_vec; spread = :all)(xmiss)

    ### missing in keyword arg only
    @test_throws ArgumentError spreadmissings(right_vec; spread = :all)(; z = ymiss)

    ### missing in both main arg and keyword arg
    @test_throws ArgumentError spreadmissings(right_vec; spread = :all)(x, xmiss; z = ymiss)

    ### missings nowhere
    @test_throws ArgumentError spreadmissings(right_vec; spread = :all)(x; z = y)

    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(right_vec)(x, s)
    ### no vectors
    t = spreadmissings(right_vec)(1, 2; z = 9)
    @test t == [-1]
end

@testset "non-vector, :default" begin
    ### missings in main arg only
    t = spreadmissings(scalar; spread = :default)(xmiss)
    @test t ≈ 1
    ### missing in keyword arg only
    t = spreadmissings(scalar; spread = :default)(; z = ymiss)
    @test t ≈ 1
    ### missing in both main arg and keyword arg
    t = spreadmissings(scalar; spread = :default)(x, xmiss; z = ymiss)
    @test t ≈ 1
    ### missings nowhere
    t = spreadmissings(scalar; spread = :default)(x; z = y)
    @test t ≈ 1
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(scalar; spread = :default)(x, s)
    ### no vectors
    t = spreadmissings(scalar; spread = :default)(1, 2; z = 9)
    @test t == 1
end

@testset "non-vector, :nonmissing" begin
    ### missings in main arg only
    t = spreadmissings(scalar; spread = :nonmissing)(xmiss)
    @test t ≈ [1, 1, 1, missing]
    ### missing in keyword arg only
    t = spreadmissings(scalar; spread = :nonmissing)(; z = ymiss)
    @test t ≈ [missing, 1, 1, 1]
    ### missing in both main arg and keyword arg
    t = spreadmissings(scalar; spread = :nonmissing)(x, xmiss; z = ymiss)
    @test t ≈ [missing, 1, 1, missing]
    ### missings nowhere
    t = spreadmissings(scalar; spread = :nonmissing)(x; z = y)
    @test t ≈ [1, 1, 1, 1]
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(scalar; spread = :nonmissing)(x, s)
    ### no vectors
    t = spreadmissings(scalar; spread = :nonmissing)(1, 2; z = 9)
    @test t == 1
end

@testset "non-vector, :none" begin
    ### missings in main arg only
    t = spreadmissings(scalar; spread = :none)(xmiss)
    @test t ≈ 1
    ### missing in keyword arg only
    t = spreadmissings(scalar; spread = :none)(; z = ymiss)
    @test t ≈ 1
    ### missing in both main arg and keyword arg
    t = spreadmissings(scalar; spread = :none)(x, xmiss; z = ymiss)
    @test t ≈ 1
    ### missings nowhere
    t = spreadmissings(scalar; spread = :none)(x; z = y)
    @test t ≈ 1
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(scalar; spread = :none)(x, s)
    ### no vectors
    t = spreadmissings(scalar; spread = :none)(1, 2; z = 9)
    @test t == 1
end

@testset "non-vector, :all" begin
    ### missings in main arg only
    t = spreadmissings(scalar; spread = :all)(xmiss)
    @test t ≈ [1, 1, 1, 1]
    ### missing in keyword arg only
    t = spreadmissings(scalar; spread = :all)(; z = ymiss)
    @test t ≈ [1, 1, 1, 1]
    ### missing in both main arg and keyword arg
    t = spreadmissings(scalar; spread = :all)(x, xmiss; z = ymiss)
    @test t ≈ [1, 1, 1, 1]
    ### missings nowhere
    t = spreadmissings(scalar; spread = :all)(x; z = y)
    @test t ≈ [1, 1, 1, 1]
    ### Mis-matched vector lengths
    @test_throws DimensionMismatch spreadmissings(scalar; spread = :all)(x, s)
    ### no vectors
    t = spreadmissings(scalar; spread = :all)(1, 2; z = 9)
    @test t == 1
end

@testset "categorical" begin
    x = [1, 2, 3]
    t = spreadmissings(categorical)(x)
    @test t == categorical([1, 2, 3])
    @test typeof(t) == typeof(categorical([1, 2, 3]))

    x = [1, 2, 3, missing]
    t = spreadmissings(categorical)(x)
    @test t ≈ categorical([1, 2, 3, missing])
    @test typeof(t) == typeof(categorical([1, 2, 3, missing]))
end

# Error on missing
alwaysone(args...; kwargs...) = 1
@test_throws ArgumentError spreadmissings(alwaysone)(missing)
@test_throws ArgumentError spreadmissings(alwaysone)(; a = missing)
@test_throws ArgumentError spreadmissings(alwaysone)([1, 2], missing)
@test_throws ArgumentError spreadmissings(alwaysone)([1, 2]; a = missing)

end # module