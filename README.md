
# Missings

*Convenience functions to work with missing values in Julia*

[![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] [![][codecov-img]][codecov-url]


## Installation

The package is registered in `METADATA.jl` and so can be installed with `Pkg.add`.

```julia
julia> Pkg.add("Missings")
```

## Project Status

Starting from Julia `0.7` the `Missing` type and basic functionalities related to it have been merged to core Julia.
See [the Julia manual](https://docs.julialang.org/en/v1/manual/missing/index.html) for general documentation on missing values in Julia.
This package still provides additional features to handle `missing` values:
- `Missings.T` to extract `T` from a `Union{T, Missing}` type
- `allowmissing` and `disallowmissing` to convert between `Vector{T}` and `Vector{Union{T, Missing}}`
- `passmissing` to wrap a function so that it returns `missing` if any of its positional arguments is `missing`
- `levels` to get the unique values in a vector excluding `missing` and in their preferred order

Other functions should not be used in newly-written code as they have equivalents in Julia Base.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.


[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: https://JuliaData.github.io/Missings.jl/latest

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://JuliaData.github.io/Missings.jl/stable

[travis-img]: https://travis-ci.org/JuliaData/Missings.jl.svg?branch=master
[travis-url]: https://travis-ci.org/JuliaData/Missings.jl

[appveyor-img]: https://ci.appveyor.com/api/projects/status/8jvl7wf1droa9h91?svg=true
[appveyor-url]: https://ci.appveyor.com/project/quinnj/missings-jl

[codecov-img]: https://codecov.io/gh/JuliaData/Missings.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaData/Missings.jl

[issues-url]: https://github.com/JuliaData/Missings.jl/issues
