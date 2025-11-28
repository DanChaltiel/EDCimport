# 2 - Post processing

## Introduction

> **Caution**
>
> Before reading this, you should read
> [`vignette("reading")`](https://danchaltiel.github.io/EDCimport/articles/reading.md)
> to learn how to import your database.

After importing your database with EDCimport, you end up with an
`edc_database` object, which can be loaded to the global environment
using
[`load_database()`](https://danchaltiel.github.io/EDCimport/reference/load_database.md).

However, EDCimport provides a few functions to improve the database
before loading it.

## Harmonize Subject ID across the database

The Subject ID column, usually `SUBJID` for CDISC data, is the primary
key, shared by almost all your datasets.

Using
[`edc_unify_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_unify_subjid.md),
you can harmonize this column across the whole database, so that it
becomes a `factor`, consistent for all datasets. With `preprocess`, you
can even customize it.

This is especially convenient for joining your data and checking for
missing patients.

``` r
library(EDCimport)
db1 = edc_example()
load_database(db1)
enrol$subjid %>% class()
#> [1] "integer"
enrol$subjid %>% head()
#> [1] 1 2 3 4 5 6

db2 = edc_example() %>% 
  edc_unify_subjid(preprocess=~paste0("#", .x))
load_database(db2)
enrol$subjid %>% class()
#> [1] "factor"
enrol$subjid %>% head()
#> [1] #1 #2 #3 #4 #5 #6
#> 50 Levels: #1 #2 #3 #4 #5 #6 #7 #8 #9 #10 #11 #12 #13 #14 #15 #16 #17 ... #50
#missing patients in table `ae`
ae$subjid %>% 
  forcats::fct_count() %>% 
  dplyr::filter(n==0) 
#> # A tibble: 2 × 2
#>   f         n
#>   <fct> <int>
#> 1 #35       0
#> 2 #37       0
```

> **Note**
>
> If your SUBJID column is numeric and `preprocess` is empty, SUBJID
> will be cast to numeric.

## Clean dataset names

Is your database from a messy EDC software, filled with special
characters or camelCase column names?

Fear not! With
[`edc_clean_names()`](https://danchaltiel.github.io/EDCimport/reference/edc_clean_names.md)
you can clean all dataset names at once.

By default, it converts names to lowercase letters, numbers, and
underscores only. For this example, since
[`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md)
already provides clean column names, let’s convert all columns to
**uppercase**:

``` r
library(EDCimport)
db = edc_example() %>% 
  edc_clean_names(toupper)
load_database(db)
names(enrol)
#> [1] "SUBJID"     "AGE"        "ENROL_DATE" "ARM"        "CRFNAME"   
#> [6] "CRFSTAT"
```

## Split some dataset to short+long

> **Note**
>
> This one is a bit more complex, but bear with me, I’ll try to make is
> understandable.

When a CRF form contains both repeated and non-repeated measures, the
export usually duplicates the non-repeated measure.

This results in a “mixed” data format, combining both “long” and “short”
structures. (You usually call the latter “wide”, but in this case it is
not really.)

For example, in the dataset `long_mixed` from
[`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md),
you have two long-format variables (one value per observation) and one
wide-format variable (one value per subject).

``` r
head(long_mixed)
#> # A tibble: 6 × 6
#>   SUBJID CRFNAME                    LONG1 LONG2 SHORT CRFSTAT   
#>    <int> <chr>                      <dbl> <dbl> <chr> <chr>     
#> 1      1 both short and long data  1.33   11.0  B     Complete  
#> 2      1 both short and long data -0.869  10.9  B     Complete  
#> 3      2 both short and long data  0.0555 10.00 C     Complete  
#> 4      2 both short and long data  0.0491 10.1  C     Incomplete
#> 5      3 both short and long data -0.578   9.28 D     Complete  
#> 6      3 both short and long data -0.999   9.80 D     Complete
```

With complex CRFs and lengthy forms, this mixed structure can complicate
analysis, as repeated and non-repeated data may be unrelated.

With
[`edc_split_mixed()`](https://danchaltiel.github.io/EDCimport/reference/edc_split_mixed.md),
you can split this dataset into two, one `short` and one `long`:

``` r
db = edc_example() %>% 
  edc_split_mixed(long_mixed)
load_database(db)
head(long_mixed_short) #one row per subject
#> # A tibble: 6 × 3
#>   subjid crfname                  short
#>    <int> <chr>                    <chr>
#> 1      1 both short and long data B    
#> 2      2 both short and long data C    
#> 3      3 both short and long data D    
#> 4      4 both short and long data E    
#> 5      5 both short and long data F    
#> 6      6 both short and long data G
head(long_mixed_long)  #one row per observation
#> # A tibble: 6 × 4
#>   subjid   long1 long2 crfstat   
#>    <int>   <dbl> <dbl> <chr>     
#> 1      1  1.33   11.0  Complete  
#> 2      1 -0.869  10.9  Complete  
#> 3      2  0.0555 10.00 Complete  
#> 4      2  0.0491 10.1  Incomplete
#> 5      3 -0.578   9.28 Complete  
#> 6      3 -0.999   9.80 Complete
```

## You can combine!

Obviously, these functions can be piped to one another:

``` r
db = edc_example() %>% 
  edc_split_mixed(long_mixed)  %>% 
  edc_unify_subjid(preprocess=~paste0("#", .x))%>% 
  edc_clean_names(toupper)

load_database(db)
```

Don’t hesitate to [submit a feature
request](https://github.com/DanChaltiel/EDCimport/issues/new?template=feature_request.md)
if you think another function can be useful to others!
