# Search for newer data

Search in some folders if a TrialMaster database more recent than the
current extraction is present. By default, it will search the "data"
folder and the OS usual "Downloads" folder. If a newer database is
found, user will be asked if they want to move it to the "data" folder.

## Usage

``` r
search_for_newer_data(
  archive,
  ...,
  source = path_home("Downloads"),
  target = "data",
  ask = TRUE,
  advice = TRUE
)
```

## Arguments

- archive:

  TM archive path, giving the project name and date

- ...:

  unused

- source:

  the path vector to be searched, default to both "data" and the usual
  "Downloads" folder

- target:

  the path where files should be copied

- ask:

  whether to ask the user to move the file to "data"

- advice:

  whether to advice how to move it instead, if `ask==FALSE`

## Value

the path to the newer file, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
  archive = "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip"
  #tm = read_trialmaster(archive)
  search_for_newer_data(archive)
} # }
```
