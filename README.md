
# Missings

*A missing value representation for Julia for databases and statistics*

| **PackageEvaluator**                                            | **Build Status**                                                                                |
|:---------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
|[![][pkg-0.6-img]][pkg-0.6-url] | [![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] [![][codecov-img]][codecov-url] |


## Installation

The package is registered in `METADATA.jl` and so can be installed with `Pkg.add`.

```julia
julia> Pkg.add("Missings")
```

## Project Status

The package is tested against the current Julia `0.6` release and nightly on Linux, OS X, and Windows.

Starting from Julia `0.7` the `Missing` type and basic functionalities related to it have been merged to core Julia.
This package still provides additional features to handle `missing` values.

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

[pkg-0.6-img]: http://pkg.julialang.org/badges/Missings_0.6.svg
[pkg-0.6-url]: http://pkg.julialang.org/?pkg=Missings

## Documentation

Missings.jl provides a single type `Missing` with a single instance `missing` which represents a missing value in data. `missing` values behave essentially like [`NULL` in SQL](https://en.wikipedia.org/wiki/NULL_(SQL)) or [`NA` in R](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#NA-handling). `missing` differs from `nothing` (the object returned by Julia functions and blocks which do not return any value) in that it can be passed to many operators and functions, prompting them to return `missing`. Where appropriate, packages should provide methods propagating `missing` for the functions they define.

The package defines standard operators and functions which propagate `missing` values: for example `1 + missing` and `cos(missing)` both return `missing`. In particular, note that comparison operators `==`, `<`, `>`, `<=` and `=>` (but not `isequal` nor `isless`) also propagate `missing`, so that `1 == missing` and `missing == missing` both return `missing`. Use `ismissing` to test whether a value is `missing`. Logical operators `&`, `|`, `⊻`/`xor` implement [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic): they return `missing` only when the result cannot be determined. For example, `true & missing` returns `missing` but `true | missing` returns `true`.

In many cases, `missing` values will have to be skipped or replaced with a valid value. For example, `sum([1, missing])` returns `missing` due to the behavior of `+`. Use `sum(Missings.skip([1, missing])` to ignore `missing` values. `sum(Missings.replace([1, missing], 0))` would have the same effect. `Missings.fail` throws an error if any value is found while iterating over the data. These three functions return an iterator and therefore do not need to allocate a copy of the data. Finally, the `Missings.coalesce` function is a more complex and powerful version of `Missings.replace`.
