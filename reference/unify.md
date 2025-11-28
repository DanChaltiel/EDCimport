# Unify a vector

Turn a vector of length N to a vector of length 1 after checking that
there is only one unique value. Useful to safely flatten a duplicated
table. Preserves the `label` attribute if set.

## Usage

``` r
unify(x, collapse_chr = FALSE, warn = TRUE)
```

## Arguments

- x:

  a vector

- collapse_chr:

  whether to collapse non-unique character values

- warn:

  whether to warn if non-unique values were found

## Value

a vector of length 1

## Examples

``` r
unify(c(1,1,1,1))
#> [1] 1
#unify(c(1,1,2,1)) #warning

library(dplyr)
set.seed(42)
x=tibble(id=rep(letters[1:5],10), value=rep(1:5,10), 
         value2=sample(letters[6:10], 50, replace=TRUE))
x %>% summarise(value=unify(value), .by=id) #safer than `value=value[1]`
#> # A tibble: 5 × 2
#>   id    value
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3
#> 4 d         4
#> 5 e         5
x %>% summarise(value2=unify(value2, collapse_chr=TRUE, warn=FALSE), .by=id)
#> # A tibble: 5 × 2
#>   id    value2                      
#>   <chr> <chr>                       
#> 1 a     f, i, f, h, j, i, h, j, g, g
#> 2 b     j, g, j, f, j, h, g, i, h, g
#> 3 c     f, g, i, f, j, g, i, j, f, i
#> 4 d     f, f, g, h, i, f, i, i, j, h
#> 5 e     g, i, g, i, g, g, g, g, g, j
x$value[2]=1
x %>% summarise(value2=unify(value2), .by=id) #warning about that non-unique value
#> Warning: There were 5 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `value2 = unify(value2)`.
#> ℹ In group 1: `id = "a"`.
#> Caused by warning:
#> ! Unifying multiple values in "value2", returning the first one ("f)"
#> ℹ Unique values: "f", "i", "h", "j", and "g"
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
#> # A tibble: 5 × 2
#>   id    value2
#>   <chr> <chr> 
#> 1 a     f     
#> 2 b     j     
#> 3 c     f     
#> 4 d     f     
#> 5 e     g     
```
