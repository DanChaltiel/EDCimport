# Load a list in an environment

Load a list in an environment

## Usage

``` r
load_list(x, env = parent.frame(), remove = TRUE)
```

## Arguments

- x:

  a list

- env:

  the environment onto which the list should be loaded

- remove:

  if `TRUE`, `x` will be removed from the environment afterward

## Value

nothing, called for its side-effect

## Examples

``` r
x=list(a=1, b=mtcars)
load_list(x, remove=FALSE)
print(a)
#> [1] 1
print(nrow(b))
#> [1] 32
```
