# SAiVE 1.0.7
Changes to some function documentation for clarity and to address issues with headings.

# SAiVE 1.0.6
Changes to hydrology related functions for ease of use and to address issues with large rasters.
### hydroProcess, createStreams, drainageBasins:
Add option to silence Whitebox tools messages.
### createStreams
-   Add option to not return vector object or file, as terra::as.lines fails on too large a raster. A vector can be created by the user is desired by splitting the raster.

# SAiVE 1.0.5
Several minor bugs and sub-optimal behavior were fixed in this release. The most notable changes are:
### spatPredict:
-   Improved error and warning logging, and runs to completion on all methods (when running multiple) unless one throws an error. Warnings are logged and printed to the console as they occur, allowing the user to make better decisions about which methods to retain/discard.
-   Much improved model testing and cross-comparison. Previously raster values were sampled at the user-defined number of points, and the resultant data.frame was partitioned 70/30 into training and testing data sets. When working with a large number of points, this meant that the model testing was performed on increasing similar data sets. This precluded any meaningful evaluation of the model and was especially problematic in identifying model over fitting. Now, data is partitioned spatially by using polygons which are themselves randomly split 70/30 into training and testing categories; points within these polygons are then assigned to one of the two data sets depending on their intersection with the polygons, keeping them spatially distinct.
-   Addition of parameter `fastFraction` which allows the user to specify the fraction of the data to use for fast model testing. This provides more fine-grained control of rapid testing if so desired.
-   Added additional fault catching for when the user improperly specifies a function parameter.
-   Clarified the documentation in several areas.

# SAiVE 1.0.4
-   Initial CRAN submission.
