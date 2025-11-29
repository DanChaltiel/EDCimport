# Format factor levels as Yes/No

Format factor levels as arbitrary values of Yes/No (with Yes always
first) while **leaving untouched** all vectors that contain other
information.

## Usage

``` r
fct_yesno(
  x,
  input = list(yes = c("Yes", "Oui"), no = c("No", "Non"), na = c("NA", "")),
  output = c("Yes", "No"),
  strict = FALSE,
  mutate_character = TRUE,
  fail = TRUE
)
```

## Arguments

- x:

  a vector of any type/class.

- input:

  list of values to be considered as "yes", "no", and `NA`.

- output:

  the output factor levels.

- strict:

  whether to match the input strictly or use
  [stringr::str_detect](https://stringr.tidyverse.org/reference/str_detect.html)
  to find them. Can also be "ignore_case" to just ignore the case.

- mutate_character:

  whether to turn characters into factor.

- fail:

  whether to fail if some levels cannot be recoded to yes/no.

## Value

a factor, or `x` untouched.

## Examples

``` r
fct_yesno(c("No", "Yes")) #levels are in order
#> [1] No  Yes
#> Levels: Yes No

set.seed(42)
N=6
x = tibble(
  a=sample(c("Yes", "No"), size=N, replace=TRUE),
  b=sample(c("Oui", "Non"), size=N, replace=TRUE),
  c=sample(0:1, size=N, replace=TRUE),
  d=sample(c(TRUE, FALSE), size=N, replace=TRUE),
  e=sample(c("1-Yes", "0-No", "2-NA"), size=N, replace=TRUE),
  
  y=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
  z=1:N,
)
 
x          
#> # A tibble: 6 × 7
#>   a     b         c d     e     y         z
#>   <chr> <chr> <int> <lgl> <chr> <chr> <int>
#> 1 Yes   Non       0 FALSE 2-NA  ccc       1
#> 2 Yes   Non       1 FALSE 1-Yes bbb       2
#> 3 Yes   Oui       0 TRUE  2-NA  aaa       3
#> 4 Yes   Non       0 TRUE  1-Yes bbb       4
#> 5 No    Oui       1 TRUE  1-Yes bbb       5
#> 6 No    Non       1 TRUE  0-No  ccc       6
#y and z are left untouched (or throw an error if fail=TRUE)   
sapply(x, fct_yesno, fail=FALSE, simplify=FALSE)
#> $a
#> [1] Yes Yes Yes Yes No  No 
#> Levels: Yes No
#> 
#> $b
#> [1] No  No  Yes No  Yes No 
#> Levels: Yes No
#> 
#> $c
#> [1] No  Yes No  No  Yes Yes
#> Levels: Yes No
#> 
#> $d
#> [1] No  No  Yes Yes Yes Yes
#> Levels: Yes No
#> 
#> $e
#> [1] <NA> Yes  <NA> Yes  Yes  No  
#> Levels: Yes No
#> 
#> $y
#> [1] "ccc" "bbb" "aaa" "bbb" "bbb" "ccc"
#> 
#> $z
#> [1] 1 2 3 4 5 6
#> 

# as "1-Yes" is not in `input`, x$e is untouched/fails if strict=TRUE
fct_yesno(x$e)
#> [1] <NA> Yes  <NA> Yes  Yes  No  
#> Levels: Yes No
fct_yesno(x$e, strict=TRUE, fail=FALSE) 
#> [1] "2-NA"  "1-Yes" "2-NA"  "1-Yes" "1-Yes" "0-No" 
fct_yesno(x$e, output=c("Ja", "Nein"))
#> [1] <NA> Ja   <NA> Ja   Ja   Nein
#> Levels: Ja Nein
```
