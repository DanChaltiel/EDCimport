## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.



## Comments

 - `rm(list=ls())` were removed from tests, although this was NOT in any example or vignette.
 
 - DESCRIPTION has been modified to be clearer. I took inspiration from `dplyr`'s description and I think I achieve the same level of detail. This packages doesn't do many things so I'm not sure how to be more specific. Note that features related to Macro are still WIP.
 
 - An example data archive has been packed in `inst/exdata`. However, as this example relies on 7-zip to run, it would fail on most automatic testing platforms so I left it in a `\dontrun`. I plan on supporting other platforms one day so this should be temporary.
 
 - Thanks for noticing that ugly `print()` in `extract_7z()`, it was a debugging leftover.
 
 - This packages uses an external tool (7-zip) to unzip archives protected by a password. 
Therefore, tests will unfortunately only be ran locally until I find another way.

 - Sometimes the folowing NOTE is raised, although words are in inst/WORDLIST:
      Possibly misspelled words in DESCRIPTION:
      EDC (2:25)
      TrialMaster (10:68)
      
