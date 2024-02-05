#' Permafrost data
#'
#' A small data set of permafrost type with correlated terrain attributes
#'
#' @format ## `permafrost`
#' A data frame with 400 rows and 8 columns
#' @source Permafrost type classification from central Yukon, with corresponding values derived from the Canadian Digital Elevation Model.
"permafrost"

#' Polygons of permafrost occurrence and type
#'
#' @format ## `permafrost_polygons`
#' A geopackage file loaded as a terra spatVector.
#' @source Permafrost classification polygons created from ground observations and geophysics in central Yukon.
"permafrost_polygons"

#' Points for basin delineation
#'
#' @format ## `basin_pts`
#' A geopackage file loaded as a terra spatVector
#' @source Created by package developer in ArcGIS Pro
"basin_pts"

#' Lines representing streams
#'
#' @format ## `streams`
#' A geopackage file loaded as a terra spatVector
#' @source A subset of the Yukon CANVEC water flow lines found [here](https://open.yukon.ca/data/datasets/water-flow-50k-canvec)
"streams"

#' Raster of aspect
#'
#' @format ## `aspect`
#' A tif file loaded as a terra spatRaster
#' @source Derived from the [Canadian Digital Elevation Model DEM](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
"aspect"

#' Raster of elevation for basin delineation testing
#'
#' @format ## `basin_dem`
#' A tif file loaded as a terra spatRaster
#' @source Small subset of the [Canadian Digital Elevation Model DEM](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
"basin_dem"

#' Raster of elevation
#'
#' @format ## `elev`
#' A tif file loaded as a terra spatRaster
#' @source Small subset of the [Canadian Digital Elevation Model DEM](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
"elev"

#' Raster of solar radiation
#'
#' @format ## `solrad`
#' A tif file loaded as a terra spatRaster
#' @source Derived from the [Canadian Digital Elevation Model DEM](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333) using the [ArcGIS Area Solar Radiation tool](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-analyst/area-solar-radiation.htm)
"solrad"

#' Raster of slope angle
#'
#' @format ## `slope`
#' A tif file loaded as a terra spatRaster
#' @source Derived from the [Canadian Digital Elevation Model DEM](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
"slope"

#' Raster of vegetation types
#'
#' @format ## `veg`
#' A tif file loaded as a terra spatRaster
#' @source A small subset of the North American Land Cover dataset produced by the [Commission for Environmental Cooperation](http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/), resampled to match cell size of other rasters in this package.
"veg"
