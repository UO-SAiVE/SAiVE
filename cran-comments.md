## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

You may note that most tests and examples are preceeded by skip_on_cran() and \dontrun. This isn't out of laziness. Many functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate), that require large data sets to run, or that take excessive time to run (several minutes).
