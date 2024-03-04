# Missings.jl

[![CI](https://github.com/JuliaData/Missings.jl/workflows/CI/badge.svg)](https://github.com/JuliaData/Missings.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/JuliaData/Missings.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaData/Missings.jl)
[![deps](https://juliahub.com/docs/Missings/deps.svg)](https://juliahub.com/ui/Packages/Missings/wLfgT?t=2)
[![version](https://juliahub.com/docs/Missings/version.svg)](https://juliahub.com/ui/Packages/Missings/wLfgT)
[![pkgeval](https://juliahub.com/docs/Missings/pkgeval.svg)](https://juliahub.com/ui/Packages/Missings/wLfgT)


*Convenience functions for working with missing values in Julia*


**Installation**: at the Julia REPL, `import Pkg; Pkg.add("Missings")`

**Maintenance**: Missings is maintained collectively by the [JuliaData collaborators](https://github.com/orgs/JuliaData/people).
Responsiveness to pull requests and issues can vary, depending on the availability of key collaborators.

## Project Status

Starting from Julia 1.0, the `Missing` type and basic related functionality are part of the language.
For documentation see [the Julia manual section on missing values](https://docs.julialang.org/en/v1/manual/missing/index.html).

This package provides additional functionality for working with `missing` values:
- `nonmissingtype` to extract `T` from a `Union{T, Missing}` type
- `allowmissing` and `disallowmissing` to convert between `Vector{T}` and `Vector{Union{T, Missing}}`
- `passmissing` to wrap a function so that it returns `missing` if any of its positional arguments is `missing`
- `levels` to get the unique values in a vector excluding `missing` and in their preferred order
- `Missings.replace` to wrap a collection in a (possibly indexable) iterator replacing `missing` with another value
- `Missings.fail` to wrap a collection in a (possibly indexable) iterator throwing an error if `missing` is encountered
- `skipmissings` to loop through a collection of iterators excluding indices where any iterators are `missing`
- `missingsmallest(f)` to create a partial order function that treats `missing` as the smallest value and otherwise behaves like `f`
- `missingsmallest`: the standard `isless` function modified to treat `missing` as the smallest value rather than the largest one

## Contributing and Questions

Contributions are welcome, as are feature requests and suggestions.
Please open an [issue][issues-url] if you encounter any problems or would just like to ask a question.

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
