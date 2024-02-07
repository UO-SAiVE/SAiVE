## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is yet another resubmission. I have:
- Removed function defaults that set save paths/directories.
- Added options to limit number of cores.
- Made examples run on 2 cores at most.
- Found and fixed a few typos not previously caught.

## General comment

Some tests are preceded by skip_on_cran(). This is out of necessity: these functions have outputs that differ slightly between tests (and thus always fail but might be determined by a human to be accurate) and take a long time to run. Other tests run conditionally if the WhiteboxTools software is installed, and thus won't be run on CRAN. 

