# Format factor levels as Yes/No

Format factor levels as arbitrary values of Yes/No (with Yes always
first) while **leaving untouched** all vectors that contain other
information.

## Usage

``` r
fct_yesno(
  x,
  input = list(yes = c("Yes", "Oui"), no = c("No", "Non")),
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

  list of values to be considered as "yes" and "no".

- output:

  the output factor levels.

- strict:

  whether to match the input strictly or use
  [stringr::str_detect](https://stringr.tidyverse.org/reference/str_detect.html)
  to find them.

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
  e=sample(c("1-Yes", "0-No"), size=N, replace=TRUE),
  
  y=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
  z=1:N,
)
 
x          
#> # A tibble: 6 × 7
#>   a     b         c d     e     y         z
#>   <chr> <chr> <int> <lgl> <chr> <chr> <int>
#> 1 Yes   Non       0 FALSE 1-Yes bbb       1
#> 2 Yes   Non       1 FALSE 0-No  ccc       2
#> 3 Yes   Oui       0 TRUE  1-Yes bbb       3
#> 4 Yes   Non       0 TRUE  1-Yes aaa       4
#> 5 No    Oui       1 TRUE  1-Yes bbb       5
#> 6 No    Non       1 TRUE  1-Yes bbb       6
#y and z are left untouched (or throw an error if fail=TRUE)   
sapply(x, fct_yesno, fail=FALSE)
#>      a   b   c   d   e   y     z  
#> [1,] "1" "2" "2" "2" "1" "bbb" "1"
#> [2,] "1" "2" "1" "2" "2" "ccc" "2"
#> [3,] "1" "1" "2" "1" "1" "bbb" "3"
#> [4,] "1" "2" "2" "1" "1" "aaa" "4"
#> [5,] "2" "1" "1" "1" "1" "bbb" "5"
#> [6,] "2" "2" "1" "1" "1" "bbb" "6"

# as "1-Yes" is not in `input`, x$e is untouched/fails if strict=TRUE
fct_yesno(x$e)
#> [1] Yes No  Yes Yes Yes Yes
#> Levels: Yes No
fct_yesno(x$e, strict=TRUE, fail=FALSE) 
#> [1] "1-Yes" "0-No"  "1-Yes" "1-Yes" "1-Yes" "1-Yes"
fct_yesno(x$e, output=c("Ja", "Nein"))
#> [1] Ja   Nein Ja   Ja   Ja   Ja  
#> Levels: Ja Nein
```
