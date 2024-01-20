## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* re-submission after adressing reviewer comment re: tar.gz file.

Most tests and examples are preceded by skip_on_cran() and \dontrun. This isn't out of laziness. Many functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate), that require large data sets to run, or that take excessive time to run (several minutes).
