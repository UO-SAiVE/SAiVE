## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. I have:
- Fixed grammatical error in DESCRIPTION and changed GitHub to 'GitHub'.
- Added a link to GitHub repository to description field of DESCRIPTION.
- Reduced tar.gz file.
- Improved test coverage.
- Improved examples, especially reducing run times.
- Removed all \dontrun calls before examples. I've use examplesIf for those that will cause problems on CRAN or CI environments.
- Made more targeted use of \donttest, limiting it to only long run time examples.
- Replaced print()/cat() function calls in favor of message() for easy suppression.
- Removed calls to set.seed()

## General comment

Some tests are preceded by skip_on_cran(). This is out of necessity: these functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate) and take a long time to run. Other tests run conditionally if the WhiteboxTools software is installed, and thus won't be run on CRAN. 

