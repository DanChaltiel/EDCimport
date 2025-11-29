# Unify a vector

Turn a vector of length N to a vector of length 1 after checking that
there is only one unique value. Useful to safely flatten a duplicated
table. This preserves the `label` attribute if set.

## Usage

``` r
unify(x)
```

## Arguments

- x:

  a vector

## Value

a vector of length 1

## Examples

``` r
unify(c(1,1,1,1))
#> [1] 1
#unify(c(1,1,2,1)) #warning

library(dplyr)
x=tibble(id=rep(letters[1:5],10), value=rep(1:5,10))
x %>% group_by(id) %>% summarise(value=unify(value)) #safer than `value=value[1]`
#> # A tibble: 5 × 2
#>   id    value
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3
#> 4 d         4
#> 5 e         5
x$value[2]=1
#x %>% group_by(id) %>% summarise(value=unify(value)) #warning about that non-unique value
```
