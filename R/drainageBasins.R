#' Watershed/basin delineation
#'
#' @author Ghislain de Laplante (gdela069@uottawa.ca or ghislain.delaplante@yukon.ca)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Hydro-processes a DEM, creating flow accumulation, direction, and streams rasters, and (optionally) delineates watersheds above one or more points using [Whitebox Tools](https://www.whiteboxgeo.com/). To facilitate this task in areas with poor quality/low resolution DEMs, can "burn-in" a stream network to the DEM to ensure proper stream placement (see details). Many time-consuming raster operations are performed, so the function will attempt to use existing rasters if they are present in the same path as the base DEM and named according to the function's naming conventions. In practice, this means that only the first run of the function needs to be very time consuming. See details for more information.
#'
#' NOTE 1: This tool can be slow to execute and will use a lot of memory. Be patient, it might take several hours with a large DEM.
#'
#' NOTE 2: ESRI shapefiles, on which the Whitebox Tools functions depend, truncate column names to 10 characters. You may want to save and re-assign column names to the output terra object after this function has run.
#'
#' NOTE 3: If you are have already run this tool and are using a DEM in the same directory as last time, you only need to specify the DEM and the points (and, optionally, a projection for the points output). Operations using the optional streams shapefile and generating flow accumulation direction, and the artificial streams raster do not need to be repeated unless you want to use a different DEM or streams shapefile.
#'
#' NOTE 4: This function is very memory (RAM) intensive. You'll want at least 16GB of RAM, and to ensure that most of it is free. If you get an error such as 'cannot allocate xxxxx bytes', you probably don't have the resources to run the tool. All rasters are un-compressed and converted to 64-bit float type before starting work, and there needs to be room to store more than twice that uncompressed raster size in memory.
#'
#' @details
#' This function uses software from the Whitebox geospatial analysis package, built by Prof. John Lindsay. Refer to [this link](https://www.whiteboxgeo.com/manual/wbt_book/intro.html) for more information.
#'
#' ## Creating derived raster layers without defining watersheds
#' This function can be run without having any specific point above which to define a watershed. This can come in handy if you need to know where the synthetic streams raster will end up to ensure that your defined watershed pour points do not end up on the wrong stream branch, or if you simply want to front-load work while you work on defining the watershed pour points. To do this, leave the parameter `points` and associated parameters as `NULL`.
#'
#' ## Explanation of process:
#' Starting from a supplied DEM, the function will fill single-cell pits, burn-in a stream network depression if requested (ensuring that flow accumulations happen in the correct location), breach depressions in the digital elevation model using a least-cost algorithm (i.e. using the pathway resulting in minimal changes to the DEM considering distance and elevation) then calculate flow accumulation and direction rasters. Then, a raster of streams is created where flow accumulation is greatest. The points provided by the user are then snapped to the derived streams raster and watersheds are computed using the flow direction rasters. Finally, the watershed/drainage basin polygons are saved to the specified save path along with the provided points and the snapped pour points.
#'
#' ## Using a streams shapefile to burn-in depressions to the DEM:
#' Be aware that this part of the function should ideally be used with a "simplified" streams shapefile. In particular, avoid or pre-process stream shapefiles that represent side-channels, as these will burn-in several parallel tracks to the DEM. ESRI has a tool called "simplify hydrology lines" which is great if you can ever get it to work, and WhiteboxTools has functions [whitebox::wbt_remove_short_streams()] to trim the streams raster, and [whitebox::wbt_repair_stream_vector_topology()] to help in converting a corrected streams vector to raster in the first place.
#'
#' @param DEM The path to a DEM including extension from which to delineate watersheds/catchments. Must be in .tif format. Derived layers such as flow accumulation, flow direction, and streams will inherit the DEM coordinate reference system.
#' @param streams Optionally, the path to the polylines shapefile/geopackage containing lines, which can be used to improve accuracy when using poor quality DEMs. If this shapefile is the only input parameter being modified from previous runs (i.e. you've found a new/better streams shapefile but the DEM is unchanged) then specify a shapefile or geopackage lines file here and overwrite = TRUE.
#' @param breach_dist The max radius (in raster cells) for which to search for a path to breach depressions, passed to [whitebox::wbt_breach_depressions_least_cost()]. This value should be high to ensure all depressions are breached. Note that the DEM is *not* breached in order of lowest elevation to greatest, nor is it breached sequentially (order is unknown, but the raster is presumably searched in some grid pattern for depressions). This means that flow paths may need to cross multiple depressions, especially in low relief areas.
#' @param threshold The accumulation threshold in DEM cells necessary to start defining a stream. This streams raster is necessary to snap pout points to, so make sure not to make this number too great!
#' @param overwrite If applicable, should rasters present in the same directory as the DEM be overwritten? This will also force the recalculation of derived layers.
#' @param projection Optionally, a projection string in the form "epsg:3579" (find them [here](https://epsg.io/)). The derived watersheds and point output layers will use this projection. If NULL the projection of the points will be used.
#' @param points The path to the points shapefile (extension .shp) containing the points from which to build watersheds. The attribute of each point will be attached to the newly-created drainage polygons. Leave NULL (along with related parameters) to only process the DEM without defining watersheds.
#' @param points_name_col The name of the column in the points shapefile containing names to assign to the watersheds. Duplicates *are* allowed, and are labelled with the suffix _duplicate and a number for duplicates 2 +.
#' @param save_path The directory where you want the output shapefiles saved.
#' @param snap Snap to the "nearest" derived (calculated) stream, or to the "greatest" flow accumulation cell within the snap distance? Beware that "greatest" will move the point downstream by up to the 'snap_dist' specified, while nearest might snap to the wrong stream.
#' @param snap_dist The search radius within which to snap points to streams. Snapping method depends on 'snap' parameter. Note that distance units will match the projection, so probably best to work on a meter grid.
#' @param burn_dist If specifying a streams layer polyline layer, the number of DEM units to depress the DEM along the stream trace.
#' @param n.cores The maximum number of cores to use. Leave NULL to use all cores minus 1.
#' @param force_update_wbt Whitebox Tools is by default only downloaded if it cannot be found on the computer, and no check are performed to ensure the local version is current. Set to TRUE if you know that there is a new version and you would like to use it.
#' @param silent_wbt Should Whitebox tools messages be suppressed? This function prints messages to the console already but these messages can be useful if you need to do some debugging.
#'
#' @return A list of terra objects. If points are specified: delineated drainages, pour points as provided, snapped pour points, and the derived streams network. If no points: flow accumulation and direction rasters, and the derived streams network. If points specified, also saved to disk: an ESRI shapefile for each drainage basin, plus the associated snapped pour point and the point as provided and a shapefiles for all basins/points together. In all cases the created or discovered rasters will be in the same folder as the DEM.
#'
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#'
#' \donttest{
#' # Must be run with file paths as well as a save_path
#'
#' # Interim raster are created in the same path as the DEM
#'
#' file.copy(system.file("extdata/basin_rast.tif", package = "SAiVE"),
#'   paste0(tempdir(), "/basin_rast.tif"))
#'
#' basins <- drainageBasins(save_path = tempdir(),
#'   DEM = paste0(tempdir(), "/basin_rast.tif"),
#'   streams = system.file("extdata/streams.gpkg", package = "SAiVE"),
#'   points = system.file("extdata/basin_pts.gpkg", package = "SAiVE"),
#'   points_name_col = "ID",
#'   breach_dist = 500,
#'   n.cores = 2)
#'
#' terra::plot(basins$delineated_basins)
#' }


drainageBasins <- function(DEM, streams = NULL, breach_dist = 10000, threshold = 500, overwrite = FALSE, projection = NULL, points = NULL, points_name_col = NULL, save_path = NULL, snap = "nearest", snap_dist = 200, burn_dist = 10, n.cores = NULL, force_update_wbt = FALSE, silent_wbt = TRUE) {

  # Initial setup #####################################################
  rlang::check_installed("whitebox", reason = "required to use function drainageBasins") #This is here because whitebox is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
  invisible(utils::capture.output(wbtCheck(force = force_update_wbt, silent = TRUE)))  #Check whitebox binaries existence and version, install if necessary or if force_update_wbt = TRUE.

  if (silent_wbt) {
    old_option <- whitebox::wbt_verbose()
    whitebox::wbt_verbose(FALSE)
    if (!is.null(old_option)) {
      if (old_option) {
        on.exit(options("whitebox.verbose_mode" = TRUE))
      }
    } else {
      on.exit(whitebox::wbt_verbose(TRUE))
    }
  }

  # Set warn options to 1 so the user sees warnings as they happen (better context and could enable user to cancell lengthy operation without waiting till the end).
  old_warn <- options("warn")
  # See if you can set option without modifying it. If not, don't modify it.
  try({
    options(warn = old_warn$warn) #If this succeeds then the option can be modified. If it fails the two lines below are not run and the option is not modified.
    options(warn = 1)
    on.exit(options(warn = old_warn$warn))
  })

  if (!(snap %in% c("nearest", "greatest"))) {
    stop("The parameter 'snap' must be one of 'nearest' or 'greatest'.")
  }

  if (!file.exists(DEM)) {
    stop("The DEM you pointed to does not exist. Perhaps your file path is wrong?")
  }
  directory <- dirname(DEM)
  input_DEM <- DEM #at this point DEM is a path, not an object
  if (!is.null(streams)) {
    if (!file.exists(streams)) {
      stop("The streams vector file you pointed to does not exist. Perhaps your file path is wrong or is missing the extension?")
    }
  }

  # Change whitebox max core options to user request
  cores <- parallel::detectCores()
  if (!is.null(n.cores)) {
    if (cores < n.cores) {
      n.cores <- cores - 1
    }
    old.wbt.opts <- as.integer(Sys.getenv("R_WHITEBOX_MAX_PROCS", unset = NA))
    Sys.setenv("R_WHITEBOX_MAX_PROCS" = n.cores)
    on.exit(if (is.na(old.wbt.opts)) Sys.unsetenv("R_WHITEBOX_MAX_PROCS") else Sys.setenv("R_WHITEBOX_MAX_PROCS" = old.wbt.opts), add = TRUE)
  }

  #change terra options to allow greater RAM fraction use
  old <- terra::terraOptions(print = FALSE)
  terra::terraOptions(memfrac = 0.9)
  on.exit(terra::terraOptions(memfrac = old$memfrac), add = TRUE)

  if (!is.null(points)) {
    if (!dir.exists(save_path)) {
      stop("The save_path you provided does not exist. Ceate it and try again.")
    }
    points <- suppressWarnings(terra::vect(points)) #load the points
    if (!terra::is.points(points)) {
      stop("The points vector file you specified is not, in fact, of point geometry type.")
    }
    if (!(points_name_col %in% names(points))) {
      stop("The column name you passed to parameter points_name_col does not appear to be in the points shapefile. If the column name had spaces, slashes, or other problematic characters, it might have been modified upon reading it in. To see what R thinks the column names are you could load the layer using names(terra::vect('path_to_your_shp')).")
    }
    if (is.na(terra::crs(points))) {
      stop("The points shapefile does not have a coordinate reference system specified. Please fix this issue and try again.")
    }

    original_projection <- paste0("epsg:", terra::crs(points, describe = TRUE)$code)
    DEM <- terra::rast(DEM) #load the DEM to R environment
    points <- terra::project(points, DEM)
    suppressWarnings(dir.create(paste0(tempdir(), "/temp_inputs")))
    terra::writeVector(points, paste0(tempdir(), "/temp_inputs/points.shp"), overwrite = TRUE)
  }

  #Check for existence of final layers and that their extents match the provided DEM. Create them if not exist or different extent.
  d8pntr_exists <- FALSE
  streams_derived_exists <- FALSE
  d8fac_exists <- FALSE
  if (!is.null(streams) | !overwrite) { #no point in checking the derived rasters if a new streams layer is specified or if we're overwriting anyways
    message("Checking if the right layers already exist...")
    if (file.exists(paste0(directory, "/D8pointer.tif")) & !overwrite) {
      d8pntr <- terra::rast(paste0(directory, "/D8pointer.tif"))
      if (terra::compareGeom(d8pntr, DEM)) {
        d8pntr_exists <- TRUE
      }
    }
    if (file.exists(paste0(directory, "/streams_derived.tif")) & d8pntr_exists & !overwrite) {
      streams_derived <- terra::rast(paste0(directory, "/streams_derived.tif"))
      if (terra::compareGeom(streams_derived, DEM)) {
        streams_derived_exists <- TRUE
      }
    }
    if (snap == "greatest") {
      if (file.exists(paste0(directory, "/D8fac.tif"))) {
        d8fac <- terra::rast(paste0(directory, "/D8fac.tif"))
        if (terra::compareGeom(d8fac, DEM)) {
          d8fac_exists <- TRUE
        }
      }
    } else { #if snap == "nearest" then d8fac is not needed
      d8fac_exists <- TRUE
    }
  }

  if (!streams_derived_exists | !d8pntr_exists | !d8fac_exists | overwrite) {
    message("Caculating layers derived from the DEM as they are either missing, have different extents as the provided DEM, you've requested an overwrite of calculated layers, or you specified a streams shapefile.")

    hydroProcess_output <- hydroProcess(DEM = input_DEM, breach_dist = breach_dist, streams = streams, burn_dist = burn_dist, save_path = directory)
    stream_outputs <- createStreams(DEM = hydroProcess_output, threshold = threshold, save_path = directory)
    d8fac <-  stream_outputs$flow_accum
    d8pntr <- stream_outputs$flow_dir
    streams_derived = stream_outputs$streams_derived
    terra::writeRaster(streams_derived, paste0(directory, "/streams_derived.tif"), overwrite = TRUE)
    terra::writeRaster(d8pntr, paste0(directory, "/D8pointer.tif"), overwrite = TRUE)
    terra::writeRaster(d8fac, paste0(directory, "/D8fac.tif"), overwrite = TRUE)
  } else {
    if (!is.null(streams)) {
      message("Using pre-calculated derived layers for basin delineation. NOTE: you specified a streams shapefile which won't be used. If you want to incorporate it run this function again with overwrite = TRUE.")
    } else {
      message("Using pre-calculated derived layers for basin delineation. If you are trying to change the streams threshold you must specify overwrite = TRUE for this to take effect.")
    }
  }

  #Now snap the points and delineate watersheds, returning polygons.
  if (!is.null(points)) {
    message("Snapping points according to the parameters selected...")
    suppressWarnings(dir.create(paste0(tempdir(), "/shapefiles")))
    unlink(list.files(paste0(tempdir(), "/shapefiles"), full.names = TRUE), recursive = TRUE, force = TRUE)
    if (snap == "nearest") {
      whitebox::wbt_jenson_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                            streams = paste0(directory, "/streams_derived.tif"),
                                            output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                            snap_dist = snap_dist)
    } else if (snap == "greatest") {
      whitebox::wbt_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                     flow_accum = paste0(directory, "/D8fac.tif"),
                                     output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                     snap_dist = snap_dist)
    }

    snapped_points <- suppressWarnings(terra::vect(paste0(tempdir(), "/shapefiles/snapped_points.shp"))) #load to memory so as to iterate over each point, allowing for looping. Otherwise the tool creates non-overlapping rasters.
    suppressWarnings(dir.create(paste0(tempdir(), "/rasters"))) #watershed tool outputs rasters (it works off a grid), but polygons are desired output. These are not saved.
    unlink(list.files(paste0(tempdir(), "/rasters"), full.names = TRUE), recursive = TRUE, force = TRUE) #ensure clear dir for repeat runs in same session
    suppressWarnings(dir.create(paste0(save_path, "/watersheds_", Sys.Date()))) #The desired outputs will go here
    unlink(list.files(paste0(save_path, "/watersheds_", Sys.Date()), full.names = TRUE), recursive = TRUE, force = TRUE)
    count <- 0 #For 'together' shapefiles. Need a feature to create the R object, then features can be appended.
    failed <- character()
    message(crayon::blue$bold("\n  Starting watershed delineation. This can take a long time so get yourself a tea/coffee.  \n"))
    for (i in 1:nrow(snapped_points)) {
      message("Delineating drainage basin for point ", as.data.frame(snapped_points[i, points_name_col]), " (", i, " of ", nrow(snapped_points), ")")
      tryCatch({
        terra::writeVector(snapped_points[i, ], paste0(tempdir(), "/shapefiles/", i,".shp"), overwrite = TRUE)
        invisible(utils::capture.output(whitebox::wbt_watershed(d8_pntr = paste0(directory, "/D8pointer.tif"),
                                                                pour_pts = paste0(tempdir(), "/shapefiles/", i, ".shp"),
                                                                output = paste0(tempdir(), "/rasters/", i, ".tif")
        )))

        rast <- terra::rast(paste0(tempdir(), "/rasters/", i, ".tif"))
        poly <- terra::as.polygons(rast)
        if (!is.null(projection)) { #project if called for, otherwise restore original projection
          poly <- terra::project(poly, projection)
          snapped_pt <- snapped_points[i, ]
          snapped_pt <- terra::project(snapped_pt, projection)
          point <- points[i, ]
          point <- terra::project(point, projection)
        } else {
          poly <- terra::project(poly, original_projection)
          snapped_pt <- snapped_points[i, ]
          snapped_pt <- terra::project(snapped_pt, original_projection)
          point <- points[i, ]
          point <- terra::project(point, original_projection)
        }
        poly <- cbind(poly, as.data.frame(point)) #attach the point attributes to the polygon
        poly <- poly[, -1] #Remove a pesky identifier column that is now superfluous

        #and now that everything worked, create the directories and populate them.
        folder_name <- as.data.frame(snapped_pt[1, points_name_col])
        tryCatch({ #sometimes the identifier row is repeated (example duplicates, identified in another col). These are identified with a suffix.
          save_watershed <- paste0(save_path, "/watersheds_", Sys.Date(), "/", folder_name)
          dir.create(save_watershed)
        }, warning = function(w) {
          files <- list.files(paste0(save_path, "/watersheds_", Sys.Date(), "/"), pattern = paste0(folder_name, "*_duplicate"))
          nums <- integer()
          for (j in files) {
            num <- sub(".*[^0-9]([0-9]{1,5})$", "\\1", j)
            if (grepl("[0-9]$", num)) {
              nums <- c(nums, as.integer(num))
            }
          }
          dir.create(paste0(save_path, "/watersheds_", Sys.Date(), "/", folder_name, "_duplicate", if (length(nums) > 0) max(nums) + 1 else ""))
          save_watershed <<- paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1, points_name_col]), "_duplicate", if (length(nums) > 0) max(nums) + 1 else "")
          folder_name <<- paste0(folder_name, "_duplicate", if (length(nums) > 0) max(nums) + 1 else "")
        })

        terra::writeVector(snapped_pt, paste0(save_watershed, "/", folder_name, "_snapped_pour_point.shp"), overwrite = TRUE)
        terra::writeVector(point, paste0(save_watershed, "/", folder_name, "_input_point.shp"), overwrite = TRUE)
        terra::writeVector(poly, paste0(save_watershed, "/", folder_name, "_drainage_basin.shp"), overwrite = TRUE)

        #now create/append to the larger shapefiles
        if (count == 0) {
          output_basins <- poly
          input_points <- point
          snapped_pts <- snapped_pt
          count <- 1
        } else {
          output_basins <- rbind(output_basins, poly)
          input_points <- rbind(input_points, point)
          snapped_pts <- rbind(snapped_pts, snapped_pt)
        }
        message(crayon::blue("Success!  \n"))
      }, error = function(e) {
        warning(crayon::red(paste0("Failed to delineate watershed for point named ", as.data.frame(snapped_points[i, points_name_col]), "  \n")))
        failed <- c(failed, as.data.frame(snapped_points[i, points_name_col]))
      })
    }
    #Save the larger shapefiles to disc
    terra::writeVector(output_basins, paste0(save_path, "/watersheds_", Sys.Date(), "/drainage_basins.shp"), overwrite = TRUE)
    terra::writeVector(input_points, paste0(save_path, "/watersheds_", Sys.Date(), "/input_points.shp"), overwrite = TRUE)
    terra::writeVector(snapped_points, paste0(save_path, "/watersheds_", Sys.Date(), "/snapped_points.shp"), overwrite = TRUE)

    if (length(failed) == nrow(snapped_points)) {
      warning(crayon::red$bold("Failed to delineate all points. Re-check your inputs carefully, and if the issue persists troubleshoot by running the function line by line."))
    } else if (length(failed) > 0) {
      warning(crayon::red$bold(paste0("Failed to delineate points ", paste(failed, collapse = ", "), ".")))
    }

    if (exists("d8fac")) {
      result <- list(delineated_basins = output_basins, input_points = input_points, snapped_points = snapped_points, streams_derived = streams_derived, d8_flow_accumulation = d8fac, d8_flow_dir = d8pntr)
      message(crayon::blue$bold("Function complete: drainage basins, points, and derived streams are returned and are saved to disk."))
    } else {
      result <- list(delineated_basins = output_basins, input_points = input_points, snapped_points = snapped_points, streams_derived = streams_derived, d8_flow_dir = d8pntr)
      message(crayon::blue$bold("Function complete: drainage basins, points, and derived streams are returned and are saved to disk."))
    }
  } else {
    if (exists("d8fac")) {
      result <- list(streams_derived = streams_derived, d8_flow_accumulation = d8fac, d8_flow_dir = d8pntr)
      message(crayon::blue$bold("Function complete: derived flow accumulation, direction, and streams rasters are returned and saved to disk."))
    } else {
      result <- list(streams_derived = streams_derived, d8_flow_dir = d8pntr)
      message(crayon::blue$bold("Function complete: derived flow direction, and streams rasters are returned and saved to disk."))
    }
  }

  return(result)

} #End of function
