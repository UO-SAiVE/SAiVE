## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. I have:
- Modified the description to add a space before URL linking
- Removed calls to internal package data (SAiVE:::data): all data is now visible and called normally.
- Found a way to document spatial data that loads as {terra} objects and to use it as exported package data.

## General comment

Some tests are preceded by skip_on_cran(). This is out of necessity: these functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate) and take a long time to run. Other tests run conditionally if the WhiteboxTools software is installed, and thus won't be run on CRAN. 

