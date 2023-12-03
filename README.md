# SAiVE
Holds functions useful for SAiVE group research, collaboration, and publication in an R package format.

# Collaboration guidelines:
This R package is intended for publication to CRAN, which requires strict adherence to R package-writing orthodoxy. If you're new to building packages, start here : https://r-pkgs.org/. Once you've familiarized yourself with the basics of package writing (or if you're already familiar), refer to existing functions to make sure you're doing things properly.

The following 
- DO NOT push changes directly to the "main" branch. Use your own development branch, then merge it into main once your changes are 100% tested (and the points below are also completed). Avoid merge conflicts by limiting the scope of work for each merge to main.
- Take the time to build thorough tests for each of your functions, unless there's a good reason why these won't work or can't be done. Again, refer to https://r-pkgs.org/ for details on how to do this.
- Please make sure that every merge commit to the "main" branch passes devtools::check() with no errors, notes, or warnings. 
