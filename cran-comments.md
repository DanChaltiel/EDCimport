## Local R CMD check results

0 errors | 0 warnings | 0 notes

## NOTES (RHub)

3 Notes are sometimes thrown on RHub.

They are most likely false positives, or build errors that are out of my reach:

 - Package unavailable to check Rd xrefs: 'installr' (which is cited in one of my help files)

 - checking for non-standard things in the check directory -> Found the following files/directories: ''NULL''
 
 - checking for detritus in the temp directory -> Found the following files/directories: 'lastMiKTeXException'
 
## Comments

 - This packages uses an external tool (7-zip) to unzip archives protected by a password. 
Therefore, most tests can unfortunately only be ran locally. Looking forward to https://github.com/r-lib/archive/pull/73!

 - An example data archive has been packed in `inst/exdata`. However, as this example relies on 7-zip to run, it would fail on most automatic testing platforms so I left it in a `\dontrun`. 
      
