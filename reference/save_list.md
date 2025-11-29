# Save a list as `.RData` file

Save a list as `.RData` file

## Usage

``` r
save_list(x, filename)
```

## Arguments

- x:

  a list

- filename:

  the filename, with the `.RData` extension.

## Value

nothing, called for its side-effect

## Examples

``` r
x=list(a=1, b=mtcars)
save_list(x, "test.RData")
load("test.RData")
file.remove("test.RData")
#> [1] TRUE
print(a)
#> [1] 1
print(nrow(b))
#> [1] 32
```
