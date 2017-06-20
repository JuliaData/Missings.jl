
# Nulls

*A null value representation for Julia for databases and statistics*

| **PackageEvaluator**                                            | **Build Status**                                                                                |
|:---------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
|[![][pkg-0.5-img]][pkg-0.5-url] | [![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] [![][codecov-img]][codecov-url] |


## Installation

The package is registered in `METADATA.jl` and so can be installed with `Pkg.add`.

```julia
julia> Pkg.add("Nulls")
```

## Project Status

The package is tested against the current Julia `0.6` release and nightly on Linux, OS X, and Windows.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.


[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: https://JuliaData.github.io/Nulls.jl/latest

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://JuliaData.github.io/Nulls.jl/stable

[travis-img]: https://travis-ci.org/JuliaData/Nulls.jl.svg?branch=master
[travis-url]: https://travis-ci.org/JuliaData/Nulls.jl

[appveyor-img]: https://ci.appveyor.com/api/projects/status/8jvl7wf1droa9h91?svg=true
[appveyor-url]: https://ci.appveyor.com/project/quinnj/nulls-jl

[codecov-img]: https://codecov.io/gh/JuliaData/Nulls.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaData/Nulls.jl

[issues-url]: https://github.com/JuliaData/Nulls.jl/issues

## Documentation

Nulls.jl is a very simple package: it provides a single type `Null` with a single instance `null`. It also defines basic operators on `null` so that it essentially becomes a `NaN` for any type. e.g. `null < 1 == false` and `null == 1 == false`. It is also not dissimilar to the type `Void` with single instance `nothing` in Julia. The difference is where `nothing` is used as, for example, the return type of `print`, `null` has more operations defined and conceptually is used to represent missing values in data.

For convenience, Nulls.jl also defines the `?` operator so that it can be used like: `?Int => Union{Int, Null}`. This is convenient for annotating method signatures (`f(x::(?Int))`), or type-tagging container structures like arrays, (`A = Vector{?Int}(n)`).