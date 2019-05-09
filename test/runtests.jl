using Test, SparseArrays, Missings

@testset "Missings" begin
    x = Missings.replace([1, 2, missing, 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == collect(1:4)
    @test collect(x) isa Vector{Int}
    x = Missings.replace([1, 2, missing, 4], 3.0)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == collect(1:4)
    @test collect(x) isa Vector{Int}
    x = Missings.replace([1 2; missing 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test collect(x) == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    x = Missings.replace((v for v in [missing, 1, missing, 2, 4]), 0)
    @test length(x) == 5
    @test size(x) == (5,)
    @test eltype(x) === Any
    @test collect(x) == [0, 1, 0, 2, 4]
    @test collect(x) isa Vector{Int}

    x = Missings.fail([1, 2, 3, 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == [1, 2, 3, 4]
    @test collect(x) isa Vector{Int}
    x = Missings.fail([1 2; 3 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test collect(x) == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    @test_throws MissingException collect(Missings.fail([1, 2, missing, 4]))
    x = Missings.fail(v for v in [1, 2, 4])
    @test eltype(x) === Any
    @test length(x) == 3
    @test size(x) == (3,)
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = skipmissing([1, 2, missing, 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = skipmissing([1  2; missing 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = collect(skipmissing([missing]))
    @test eltype(x) === Union{}
    @test isempty(collect(x))
    @test collect(x) isa Vector{Union{}}
    x = collect(skipmissing(Union{Int, Missing}[]))
    @test eltype(x) === Int
    @test isempty(collect(x))
    @test collect(x) isa Vector{Int}
    x = skipmissing([missing, missing, 1, 2, missing, 4, missing, missing])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = skipmissing(v for v in [missing, 1, missing, 2, 4])
    @test eltype(x) === Any
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    @test levels(1:1) == levels([1]) == levels([1, missing]) == levels([missing, 1]) == [1]
    @test levels(2:-1:1) == levels([2, 1]) == levels([2, missing, 1]) == [1, 2]
    @test levels([missing, "a", "c", missing, "b"]) == ["a", "b", "c"]
    @test levels([Complex(0, 1), Complex(1, 0), missing]) == [Complex(0, 1), Complex(1, 0)]
    @test levels(sparse([0 3 2])) == [0, 2, 3]
    @test typeof(levels([1])) === typeof(levels([1, missing])) === Vector{Int}
    @test typeof(levels(["a"])) === typeof(levels(["a", missing])) === Vector{String}
    @test typeof(levels(sparse([1]))) === Vector{Int}
    @test isempty(levels([missing]))
    @test isempty(levels([]))

    @test Missings.T(Union{Int, Missing}) == Int
    @test Missings.T(Any) == Any
    @test Missings.T(Missing) == Union{}
    @test Missings.T(Union{Array{Int}, Missing}) == Array{Int}

    @test isequal(missings(1), [missing])
    @test isequal(missings(Int, 1), [missing])
    @test missings(Int, 1) isa Vector{Union{Int, Missing}}
    @test isequal(missings(Union{Int, Missing}, 1, 2), [missing missing])
    @test missings(Union{Int, Missing}, 1, 2) isa Matrix{Union{Int, Missing}}
    @test Union{Int, Missing}[1,2,3] == (Union{Int, Missing})[1,2,3]
    x = missings(Int, (1, 2))
    @test isa(x, Matrix{Union{Int, Missing}})
    @test isequal(x, [missing missing])
    x = missings((1, 2))
    @test isa(x, Matrix{Missing})
    @test isequal(x, [missing missing])

    @test allowmissing([1]) == [1]
    @test allowmissing([1]) isa AbstractVector{Union{Int, Missing}}
    @test allowmissing(Any[:a]) == [:a]
    @test allowmissing(Any[:a]) isa AbstractVector{Any}
    @test isequal(allowmissing([1, missing]), [1, missing])
    @test allowmissing([1, missing]) isa AbstractVector{Union{Int, Missing}}
    @test isequal(allowmissing([missing]), [missing])
    @test allowmissing([missing]) isa AbstractVector{Missing}

    @test allowmissing([1 1]) == [1 1]
    @test allowmissing([1 1]) isa AbstractArray{Union{Int, Missing}, 2}
    @test allowmissing([:a 1]) == [:a 1]
    @test allowmissing([:a 1]) isa AbstractArray{Any, 2}
    @test isequal(allowmissing([1 missing]), [1 missing])
    @test allowmissing([1 missing]) isa AbstractArray{Union{Int, Missing}, 2}
    @test isequal(allowmissing([missing missing]), [missing missing])
    @test allowmissing([missing missing]) isa AbstractArray{Missing, 2}

    @test disallowmissing(Union{Int, Missing}[1]) == [1]
    @test disallowmissing(Union{Int, Missing}[1]) isa AbstractVector{Int}
    @test disallowmissing([1]) == [1]
    @test disallowmissing([1]) isa AbstractVector{Int}
    @test disallowmissing(Any[:a]) == [:a]
    @test disallowmissing(Any[:a]) isa AbstractVector{Any}
    @test_throws MethodError disallowmissing([1, missing])
    @test_throws MethodError disallowmissing([missing])

    @test disallowmissing(Union{Int, Missing}[1 1]) == [1 1]
    @test disallowmissing(Union{Int, Missing}[1 1]) isa AbstractArray{Int, 2}
    @test disallowmissing([1 1]) == [1 1]
    @test disallowmissing([1 1]) isa AbstractArray{Int, 2}
    @test disallowmissing([:a 1]) == [:a 1]
    @test disallowmissing([:a 1]) isa AbstractArray{Any, 2}
    @test_throws MethodError disallowmissing([1 missing])
    @test_throws MethodError disallowmissing([missing missing])

    # Lifting
    @test passmissing(sqrt)(4) == 2.0
    @test isequal(passmissing(sqrt)(missing), missing)
    @test isequal(passmissing(sqrt).([missing, 4]), [missing, 2.0])
    @test passmissing((x,y)->"$x $y")(1, 2) == "1 2"
    @test isequal(passmissing((x,y)->"$x $y")(missing), missing)
    @test_throws ErrorException passmissing(string)(missing, base=2)
end
