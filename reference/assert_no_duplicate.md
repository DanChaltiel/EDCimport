# Assert that a dataframe has one row per patient

Check that there is no duplicate on the column holding patient ID in a
pipeable style.  
Mostly useful after joining two datasets.

## Usage

``` r
assert_no_duplicate(df, by = NULL, id_col = get_subjid_cols())
```

## Arguments

- df:

  a dataframe

- by:

  *(optional)* grouping columns

- id_col:

  the name of the columns holding patient ID

## Value

the `df` dataset, unchanged

## Examples

``` r
if (FALSE) { # \dontrun{
#without duplicate => no error, continue the pipeline
tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% nrow()

#with duplicate => throws an error
tibble(subjid=c(1:10, 1:2)) %>% assert_no_duplicate() %>% nrow()

#By groups
df = tibble(subjid=rep(1:10, 4), visit=rep(c("V1", "V2"), 2, each=10), 
            group=rep(c("A", "B"), each=20))
df %>% assert_no_duplicate() #error
df %>% assert_no_duplicate(by=c(visit, group)) #no error
} # }
```
