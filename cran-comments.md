## R CMD check results

0 errors | 0 warnings | 0 notes

* Checked using local check, GH Actions and `check_win_devel()`.

## Comments

* This packages uses an external tool (7-zip) to unzip archives protected by a password. 
Therefore, most tests can unfortunately only be ran locally. Looking forward to https://github.com/r-lib/archive/pull/73!

* An example data archive has been packed in `inst/exdata`. However, as this example relies on 7-zip to run, it would fail on most automatic testing platforms so I left it in a `\dontrun`. 
