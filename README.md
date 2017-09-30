
# Nulls

*A null value representation for Julia for databases and statistics*

| **PackageEvaluator**                                            | **Build Status**                                                                                |
|:---------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
|[![][pkg-0.6-img]][pkg-0.6-url] | [![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] [![][codecov-img]][codecov-url] |


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

[pkg-0.6-img]: http://pkg.julialang.org/badges/Nulls_0.6.svg
[pkg-0.6-url]: http://pkg.julialang.org/?pkg=Nulls

## Documentation

Nulls.jl provides a single type `Null` with a single instance `null` which represents a missing value in data. `null` values behave essentially like [`NULL` in SQL](https://en.wikipedia.org/wiki/Null_(SQL)) or [`NA` in R](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#NA-handling). `null` differs from `nothing` (the object returned by Julia functions and blocks which do not return any value) in that it can be passed to many operators and functions, prompting them to return `null`. Where appropriate, packages should provide methods propagating `null` for the functions they define.

The package defines standard operators and functions which propagate `null` values: for example `1 + null` and `cos(null)` both return `null`. In particular, note that comparison operators `==`, `<`, `>`, `<=` and `=>` (but not `isequal` nor `isless`) also propagate `null`, so that `1 == null` and `null == null` both return `null`. Use `isnull` to test whether a value is `null`. Logical operators `&`, `|`, `⊻`/`xor` implement [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic): they return `null` only when the result cannot be determined. For example, `true & null` returns `null` but `true | null` returns `true`.

In many cases, `null` values will have to be skipped or replaced with a valid value. For example, `sum([1, null])` returns `null` due to the behavior of `+`. Use `sum(Nulls.skip([1, null])` to ignore `null` values. `sum(Nulls.replace([1, null], 0))` would have the same effect. `Nulls.fail` throws an error if any value is found while iterating over the data. These three functions return an iterator and therefore do not need to allocate a copy of the data. Finally, the `Nulls.coalesce` function is a more complex and powerful version of `Nulls.replace`.
