using Test, SparseArrays, Missings

# Must be defined outside testset on v1.0
struct CubeRooter end
(::CubeRooter)(x) = cbrt(x)

@testset "Missings" begin
    x = Missings.replace([1, 2, missing, 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test axes(x) == (1:4,)
    @test keys(x) == eachindex(x) == 1:4
    @test collect(x) == [x[i] for i in keys(x)] == 1:4
    @test collect(x) isa Vector{Int}
    x = Missings.replace([1, 2, missing, 4], 3.0)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test axes(x) == (1:4,)
    @test keys(x) == eachindex(x) == 1:4
    @test collect(x) == [x[i] for i in keys(x)] == 1:4
    @test collect(x) isa Vector{Int}
    x = Missings.replace([1 2; missing 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test axes(x) == (1:2, 1:2)
    @test keys(x) == CartesianIndices((1:2, 1:2))
    @test eachindex(x) == 1:4
    @test collect(x) == [x[i] for i in keys(x)] == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    x = Missings.replace((v for v in [missing, 1, missing, 2, 4]), 0)
    @test length(x) == 5
    @test size(x) == (5,)
    if VERSION >= v"1.7.0-DEV"
        @test eachindex(x) == keys(x) == 1:5
    else
        @test_throws MethodError keys(x)
        @test_throws MethodError eachindex(x)
    end
    @test_throws MethodError x[1]
    @test eltype(x) === Any
    @test collect(x) == [0, 1, 0, 2, 4]
    @test collect(x) isa Vector{Int}

    x = Missings.fail([1, 2, 3, 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test axes(x) == (1:4,)
    @test keys(x) == eachindex(x) == 1:4
    @test collect(x) == [x[i] for i in keys(x)] == [1, 2, 3, 4]
    @test collect(x) isa Vector{Int}
    x = Missings.fail([1 2; 3 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test axes(x) == (1:2, 1:2)
    @test keys(x) == CartesianIndices((1:2, 1:2))
    @test eachindex(x) == 1:4
    @test collect(x) == [x[i] for i in keys(x)] == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    @test_throws MissingException collect(Missings.fail([1, 2, missing, 4]))
    x = Missings.fail(v for v in [1, 2, 4])
    @test eltype(x) === Any
    @test length(x) == 3
    @test size(x) == (3,)
    @test axes(x) == (1:3,)
    if VERSION >= v"1.7.0-DEV"
        @test eachindex(x) == keys(x) == 1:3
    else
        @test_throws MethodError keys(x)
        @test_throws MethodError eachindex(x)
    end
    @test_throws MethodError x[1]
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

    @test nonmissingtype(Union{Int, Missing}) == Int
    @test nonmissingtype(Any) == Any
    @test nonmissingtype(Missing) == Union{}
    @test nonmissingtype(Union{Array{Int}, Missing}) == Array{Int}

    @test isequal(missings(1), [missing])
    @test isequal(missings(Any, 1), [missing])
    @test isequal(missings(Int, 1), [missing])
    @test missings(Int, 1) isa Vector{Union{Int, Missing}}
    @test missings(Any, 1) isa Vector{Union{Any, Missing}}
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
    @test_throws Union{MethodError, ArgumentError} disallowmissing([missing])

    @test disallowmissing(Union{Int, Missing}[1 1]) == [1 1]
    @test disallowmissing(Union{Int, Missing}[1 1]) isa AbstractArray{Int, 2}
    @test disallowmissing([1 1]) == [1 1]
    @test disallowmissing([1 1]) isa AbstractArray{Int, 2}
    @test disallowmissing([:a 1]) == [:a 1]
    @test disallowmissing([:a 1]) isa AbstractArray{Any, 2}
    @test_throws MethodError disallowmissing([1 missing])
    @test_throws Union{MethodError, ArgumentError} disallowmissing([missing missing])

    # Lifting
    ## functor
    cuberoot = CubeRooter()  # defined at top of file
    @test passmissing(cuberoot)(27) == 3.0
    @test isequal(passmissing(cuberoot)(missing), missing)
    ## type
    @test passmissing(Int)(1.0) == 1
    @test isequal(passmissing(Int)(missing), missing)
    ## function
    @test passmissing(sqrt)(4) == 2.0
    @test isequal(passmissing(sqrt)(missing), missing)
    @test isequal(passmissing(sqrt).([missing, 4]), [missing, 2.0])
    @test passmissing((x,y)->"$x $y")(1, 2) == "1 2"
    @test isequal(passmissing((x,y)->"$x $y")(missing), missing)
    if VERSION >= v"1.4.0-DEV"
        @test_throws MethodError passmissing(string)(missing, base=2)
    else
        @test_throws ErrorException passmissing(string)(missing, base=2)
    end

    @test passmissing(sin) === Missings.PassMissing{typeof(sin)}(sin)
    @test passmissing(Int) === Missings.PassMissing{Type{Int}}(Int)
    @test passmissing(cuberoot) === Missings.PassMissing{CubeRooter}(cuberoot)

    @testset "deprecated" begin
        x = [1, 2, missing, 4]
        y = ["a", "b", "c", missing]
        z = [missing, missing, 3.1, 4.5]
        l = [1, 2, 3, 4, 5]
        @test_throws ArgumentError skipmissings(x, l)
        mx, my = skipmissings(x, y)
        iobuf = IOBuffer()
        show(iobuf, MIME("text/plain"), mx)
        s = String(take!(iobuf))
        @test s == "Missings.SkipMissings{$(Vector{Union{Missing, Int}})}(" *
            "Union{Missing, $Int}[1, 2, missing, 4]) comprised of 2 iterators"
        @test collect(mx) == [1, 2]
        @test collect(mx) isa Vector{Int}
        @test reduce(+, mx) === reduce(+, collect(mx)) === sum(mx) ===
            mapreduce(identity, +, mx) === 3
        @test mapreduce(x -> x^2, +, mx) === mapreduce(x -> x^2, +, collect(mx)) === 5
        mx, my, mz = skipmissings(x, y, z)
        @test eltype(mx) == Int
        @test eltype(my) == String
        @test eltype(mz) == Float64
        @test isempty(collect(mx))
        @test sum(mx) === 0
        x = [missing 4; 2 5; 3 6]
        y = [1 4; missing 5; 3 6]
        mx, my = skipmissings(x, y)
        @test collect(mx) == [3, 4, 5, 6]
        @test mx[3] == 3
        @test_throws MissingException mx[1]
        @test reduce(+, mx) === 18
        @test isapprox(mapreduce(cos, *, collect(mx)),  mapreduce(cos, *, mx))
        @static if VERSION >= v"1.4.0-DEV"
            @inferred Union{Float64, Missing} mapreduce(cos, *, mx)
            @inferred Union{Float64, Missing} sum(mx)
            @inferred Union{Float64, Missing} reduce(+, mx)
        end

        x = [missing missing missing]
        y = [1, 2, 3]
        mx, my = skipmissings(x, y)
        @test_throws Union{ArgumentError,MethodError} reduce(x -> x/2, mx)
        @test_throws Union{ArgumentError,MethodError} mapreduce(x -> x/2, +, mx)
        @test_throws MethodError length(mx)
        @test IndexStyle(typeof(mx)) == IndexStyle(typeof(x))
        x = [isodd(i) ? missing : i for i in 1:64]
        y = [isodd(i) ? missing : i for i in 65:128]
        mx, my = skipmissings(x, y)
        @test sum(mx) === 1056
        @static if VERSION >= v"1.4.0-DEV"
            @inferred Union{Missing, Int} sum(mx)
            @inferred Union{Missing, Int} reduce(+, mx)
        end
    end

@testset "emptymissing" begin
    @test emptymissing(last)([]) === missing
    @test emptymissing(last)([1, 2, 3]) === 3
    @test emptymissing(sum)(skipmissing(missings(Int, 3))) === missing
    @test emptymissing(sum)(skipmissing([1, 2, 3])) === 6
    fun(a, b; c) = (b, c)
    @test emptymissing(fun)([], 1, c=2) === missing
    @test emptymissing(fun)(3, 1, c=2) == (1, 2)
end

@testset "missingsmallest" begin
    @test missingsmallest(missing, Inf) == true
    @test missingsmallest(-Inf, missing) == false
    @test missingsmallest(missing, missing) == false
    @test missingsmallest(3, 4) == true
    @test missingsmallest(-Inf, Inf) == true 

    @test missingsmallest("a", "b") == true
    @test missingsmallest("short", missing) == false
    @test missingsmallest(missing, "") == true

    @test missingsmallest((1, 2), (3, 4)) == true
    @test missingsmallest((3, 4), (1, 2)) == false
    @test missingsmallest(missing, (1e3, 1e4)) == true
    
    # Compare strings by length, not lexicographically
    isshorter = missingsmallest((s1, s2) -> isless(length(s1), length(s2)))
    @test isshorter("short", "longstring") == true
    @test isshorter("longstring", "short") == false
    @test isshorter(missing, "short") == true
    @test isshorter("", missing) == false

    @test_throws MethodError missingsmallest(isless)(isless)
    @test missingsmallest !== missingsmallest(isless)
end

end
