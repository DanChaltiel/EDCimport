# Save `sessionInfo()` output

Save [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) output
into a text file.

## Usage

``` r
save_sessioninfo(path = "check/session_info.txt", with_date = TRUE)
```

## Arguments

- path:

  target path to write the file

- with_date:

  whether to insert the date before the file extension

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{
   save_sessioninfo()
} # }
```
