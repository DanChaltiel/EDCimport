# 3 - Data checking

## Introduction

You imported your database, but now you need to check it for errors and
inconsistencies.

There are a lot of ways to do so, so EDCimport provides functions for a
few concepts.

As in previous vignettes, we will be using
[`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md),
but in the real world you should use EDC reading functions. See
[`vignette("reading")`](https://danchaltiel.github.io/EDCimport/articles/reading.md)
to see how.

``` r
library(EDCimport)
library(dplyr)
db = edc_example(N=200) %>% 
  edc_unify_subjid() %>% 
  edc_clean_names()
db
#> ── EDCimport database ──────────────────────────────────────────────────────────
load_database(db)
```

## Data warning

### Data errors

The primary and most valuable data-checking tool in EDCimport is
[`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md).

Simply use
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
to identify problematic or inconsistent rows and pipe them into the
function.

For example, let’s say that in our study:

- Patients should be older than 25

- Adverse event grades should be between 1 and 5.

- Patients in the treatment arm should not have `data1$date1` before
  2010-04-10

Here’s how you check these conditions:

``` r
enrol %>% 
  filter(age<25) %>% 
  edc_data_warn("Patients should be >25yo", issue_n=1)
#> Warning: Issue #01: Patients should be >25yo (2 patients: #18 and #59)

ae %>% 
  filter(aegr<1 | aegr>5) %>% 
  edc_data_warn("Incorrect adverse event grade", issue_n=2)

data1 %>% 
  edc_left_join(enrol) %>% 
  filter(arm=="Trt") %>% 
  filter(date1<"2010-04-10") %>% 
  edc_data_warn("Treated patients should have been seen later", issue_n=3)
#> Warning: Issue #03: Treated patients should have been seen later (9 patients: #1, #45,
#> #64, #69, #82, …)
```

You can implement these checks according to your Data Validation Plan,
ensuring they run after every export. Any failed check will produce a
warning in your R console. Once the database is corrected, the warnings
will no longer appear (e.g. `issue_n=2`).

After running all your checks, you can use
[`edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
to get a summary of all detected issues.

``` r
edc_data_warnings()
#> # A tibble: 3 × 5
#>   issue_n message                                      subjid    data     type 
#>   <chr>   <chr>                                        <list>    <list>   <chr>
#> 1 01      Patients should be >25yo                     <chr [2]> <tibble> WARN 
#> 2 02      Incorrect adverse event grade                <NULL>    <tibble> WARN 
#> 3 03      Treated patients should have been seen later <chr [9]> <tibble> WARN
```

### Fatal error

If one check is so mandatory that you cannot work in a database where it
fails, use
[`edc_data_stop()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
instead.

For example, you can use it to check that some variable construction
didn’t go wrong:

``` r
df = mtcars %>% 
  mutate(
    type = case_when(
      cyl==4 ~ "4 cylinders", 
      cyl==6 ~ "6 cylinders", 
      cyl==8 ~ "8 cylinders", 
      .default="ERROR"
    ),
  )

df %>% 
  filter(type=="ERROR") %>% 
  edc_data_stop("Error on type construction")
```

## Duplicate-free dataset assertion

If you work with multiple datasets, your code probably include a lot of
joins. As you may have painfully discovered, joining data carries a high
risk of altering the data layout and resulting in multiple rows per
patient.

This is why you should always include
[`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
in your pipeline if you expect only one row per patient.

``` r
enrol %>% 
  assert_no_duplicate() %>% 
  count(arm)
#> # A tibble: 2 × 2
#>   arm       n
#>   <chr> <int>
#> 1 Ctl      99
#> 2 Trt     101

enrol %>% 
  edc_left_join(data1) %>% #oopsie
  assert_no_duplicate() %>% 
  count(arm)
#> Error in `assert_no_duplicate()`:
#> ! Duplicate on column "subjid" for values 1, 2, 3, 4, 5, 6, 7, 8, 9, and
#>   10.
```

> **Tip**
>
> This is the [Fail Fast
> principle](https://www.codereliant.io/fail-fast-pattern/): you’d
> better have an error in your R script than in your analysis report.

## Last-news table

If your analysis involves a survival endpoint, you likely have a
follow-up dataset that includes the vital status as of the last visit
date.

However, in real-world scenarios, this dataset might not be accurately
filled, and some patient can have other data after the date of last
visit.

The
[`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
function calculates the actual date of the last recorded information for
each patient (`SUBJID`), based on all Date/Datetime columns across all
datasets.

Currently,
[`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md)
does not include an explicit table for this scenario, so let’s consider
the following example:

- `data3$date10` is your last visit date in your followup dataset. It is
  the origin you should `prefer` if there is a tie and the reference to
  identify any inconsistency.

- `data1` is a dataset containing scheduled protocol dates, such as
  planned medical visits. You should ignore all columns in this dataset,
  as they do not pertain to the patient’s last known

- `date8` and `date9` are dates when treatments were administered. They
  imply that the patient was alive at that time. If a date is after
  `date10`, it means that the survival time is underestimated.

Here is how to parameterize
[`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
to fit this scenario:

``` r
lastnews_table(prefer="date10", except="data1", show_delta=TRUE) %>% 
  mutate(delta=round(delta)) %>% 
  arrange(desc(delta))
#> # A tibble: 200 × 8
#>    subjid last_date  origin_data origin_col origin_label    preferred_last_date
#>     <dbl> <date>     <chr>       <chr>      <chr>           <date>             
#>  1      9 2010-08-01 data3       date9      Date at visit 9 2010-07-14         
#>  2     41 2010-07-28 data3       date9      Date at visit 9 2010-07-09         
#>  3     71 2010-07-16 data3       date8      Date at visit 8 2010-06-28         
#>  4    186 2010-07-12 data3       date9      Date at visit 9 2010-06-23         
#>  5     14 2010-07-30 data3       date9      Date at visit 9 2010-07-13         
#>  6     61 2010-07-30 data3       date9      Date at visit 9 2010-07-12         
#>  7     68 2010-08-06 data3       date8      Date at visit 8 2010-07-20         
#>  8    120 2010-07-27 data3       date9      Date at visit 9 2010-07-09         
#>  9     58 2010-07-31 data3       date9      Date at visit 9 2010-07-15         
#> 10     93 2010-07-30 data3       date9      Date at visit 9 2010-07-13         
#> # ℹ 190 more rows
#> # ℹ 2 more variables: preferred_origin <chr>, delta <drtn>
```

> **Warning**
>
> This table is useful for Overall Survival and data checking. However,
> you should be very careful when using it for Event-Free Survival: a
> patient can be alive at that point without having experienced an
> event.
