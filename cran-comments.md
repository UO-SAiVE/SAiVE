## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. I have addressed reviewer comment re: tar.gz file size and further refined tests.

## General comment

Many tests and examples are preceded by skip_on_cran() and \dontrun. This is out of necessicity: many functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate), require large data sets to run, or take excessive time to run (several minutes).
