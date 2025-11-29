# Check the validity of the subject ID column

Compare a subject ID vector to the study's reference subject ID (usually
something like `enrolres$subjid`), and warn if any patient is missing or
extra.  
`check_subjid()` is the old, deprecated name.

## Usage

``` r
edc_warn_patient_diffs(
  x,
  ref = getOption("edc_subjid_ref"),
  issue_n = "xx",
  data_name = NULL,
  col_subjid = get_subjid_cols()
)
```

## Arguments

- x:

  the subject ID vector to check, or a dataframe which ID column will be
  guessed

- ref:

  the reference for subject ID. Should usually be set through
  `edc_options(edc_subjid_ref=xxx)`. See example.

- issue_n:

  identifying row number

- data_name:

  the name of the data (for the warning message)

- col_subjid:

  name of the subject ID column if `x` is a dataframe.

## Value

nothing, called for errors/warnings

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
options(edc_subjid_ref=enrol$subjid)
#usually, you set something like:
#options(edc_subjid_ref=enrolres$subjid)
edc_warn_patient_diffs(data1)
data1 %>% dplyr::filter(subjid>1) %>% edc_warn_patient_diffs(issue_n=NULL)
#> Warning: `.` has patient discrepancies:
#> ℹ Missing: 1 patient: #1
edc_warn_patient_diffs(c(data1$subjid, 99, 999))
#> Warning: Issue #xx: `c(data1$subjid, 99, 999)` has patient discrepancies:
#> ℹ Extra: 2 patients: #99 and #999
```
