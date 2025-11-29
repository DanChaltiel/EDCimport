# Set global options for `EDCimport`

Use this function to manage your `EDCimport` parameters globally while
taking advantage of autocompletion.  
Use
[`edc_peek_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_peek_options.md)
to see which option is currently set and
[`edc_reset_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_reset_options.md)
to set all options back to default.

## Usage

``` r
edc_options(
  ...,
  trialmaster_pw,
  path_7zip,
  edc_lookup,
  edc_subjid_ref,
  edc_plotly,
  edc_fct_yesno,
  edc_cols_subjid,
  edc_cols_meta,
  edc_cols_id,
  edc_cols_crfname,
  edc_meta_cols_pct,
  edc_warn_max_subjid,
  edc_read_verbose,
  edc_correction_verbose,
  edc_get_key_cols_verbose,
  edc_lookup_overwrite_warn,
  .local = FALSE
)
```

## Arguments

- ...:

  unused

- trialmaster_pw:

  the password of the trialmaster zip archive. For instance, you can use
  `edc_options(trialmaster_pw="my_pwd")` in the console once per
  session, so that you don't have to write the password in clear in your
  R code

- path_7zip:

  the path to the 7zip executable. Default to
  `"C:/Program Files/7-Zip/"`.

- edc_lookup:

  **(Internal)** a reference to the lookup table (usually `.lookup`).
  Should usually not be changed manually.

- edc_subjid_ref:

  **used in
  [edc_warn_patient_diffs](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md)**
  the vector of the reference subject IDs. You should usually write
  `edc_options(edc_subjid_ref=enrolres$subjid)`.

- edc_plotly:

  **used in
  [edc_swimmerplot](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)**
  whether to use plotly to visualize the plot.

- edc_fct_yesno:

  **used in
  [fct_yesno](https://danchaltiel.github.io/EDCimport/reference/fct_yesno.md)**
  list of values to be considered as Yes/No values. Defaults to
  `get_yesno_lvl()`.

- edc_cols_subjid, edc_cols_meta:

  the name of the columns holding the subject id (default to
  `c("ptno", "subjid")`) and the CRF form name (default to
  `c("crfname")`). It is case-insensitive.

- edc_cols_id, edc_cols_crfname:

  deprecated

- edc_meta_cols_pct:

  The minimal proportion of datasets a column has to reach to be
  considered "meta"

- edc_warn_max_subjid:

  The max number of subject IDs to show in
  [edc_data_warn](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)

- edc_read_verbose, edc_correction_verbose, edc_get_key_cols_verbose:

  the verbosity of the output of functions
  [read_trialmaster](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  and
  [read_all_xpt](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md),
  and
  [manual_correction](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md).
  For example, set `edc_options(edc_read_verbose=0)` to silence the
  first 2.

- edc_lookup_overwrite_warn:

  default to TRUE. Whether there should be warning when overwriting
  `.lookup` (like when reading 2 databases successively)

- .local:

  if TRUE, the effect will only apply to the local frame (internally
  using
  [`rlang::local_options()`](https://rlang.r-lib.org/reference/local_options.html))

## Value

Nothing, called for its side effects
