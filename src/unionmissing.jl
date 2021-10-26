"""
    @?(ex)

Wrap an expression `ex` into `Union{Missing, ex}`.

Note that Julia will pass a whole expression `ex` following `@? to it`.
Therefore using parantheses to correctly specify the `ex` is sometimes required.

```jldoctest
julia> Vector{@?Int}
Vector{Union{Missing, Int64}} (alias for Array{Union{Missing, Int64}, 1})

julia> Union{String, @?Int}
Union{Missing, Int64, String}

julia> Union{@?(Int), String}
Union{Missing, Int64, String}

julia> @?(Int)[1, 2, 3]
3-element Vector{Union{Missing, Int64}}:
 1
 2
 3
```
"""
macro var"?"(typ)
    :(Union{$(esc(typ)), Missing})
end
