# A function could be used here to handle the loading and renaming for both NEAMAP files.
# For example, `process_neamap_data(filepath)`.
process_neamap_data <- function(filepath) {
  read_csv(here(filepath)) %>%
    mutate(vessel = "NEAMAP") %>%
    rename(
      id = station,
      sumbluepywt = sumbluepreywt,
      nbluepysp = nbfpreyspp,
      npiscsp = npiscspp,
      nstomtot = nstomtot,
      meanbluepywt = meanbluepreywt,
      meanpisclen = meanpisclen.simple,
      season_ng = season,
      declat = lat,
      declon = lon,
      bottemp = bWT,
      setdepth = depthm
    )
}

#' @title Download and Process OISST Data to a Tidy Data Frame
#'
#' @description This function downloads OISST data for specified years,
#'   processes it into a raster, and then converts the raster into a
#'   tidy data frame, saving it as an RDS file. It includes checks to
#'   skip steps if files already exist.
#'
#' @param years A numeric vector of years to download and process.
#' @param varname A character string specifying the variable name to extract
#'   from the NetCDF file (e.g., "sst").
#' @param nc_to_raster The function to convert NetCDF files to rasters.
#' @param raster_to_sstdf The function to convert rasters to tidy data frames.
#'
#' @return The function does not return a value, but it saves processed data
#'   frames to the "data-raw/gridded/sst_data" directory.
#'
download_and_process_oisst <- function(
  years,
  varname,
  nc_to_raster,
  raster_to_sstdf
) {
  # Create the target directory if it doesn't exist.
  dir.create(
    here::here("data-raw", "gridded", "sst_data"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Increase the download timeout to prevent errors on large files.
  options(timeout = max(300, getOption("timeout")))

  for (i in years) {
    # Define file paths and URLs for the current year.
    nc_filename <- paste0(i, ".nc")
    grd_filename <- here::here(
      "data-raw",
      "gridded",
      "sst_data",
      paste0("test_", i, ".grd")
    )
    rds_filename <- here::here(
      "data-raw",
      "gridded",
      "sst_data",
      paste0("sst", i, ".rds")
    )

    url <- paste0(
      "https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.",
      i,
      ".nc"
    )

    # Step 1: Download and process the NetCDF to a raster file.
    if (!file.exists(grd_filename)) {
      message(paste("Downloading and processing data for year:", i))
      download.file(url, destfile = nc_filename)
      temp_raster <- nc_to_raster(nc = nc_filename, varname = varname)
      raster::writeRaster(
        temp_raster,
        filename = grd_filename,
        overwrite = TRUE
      )
      unlink(nc_filename)
      message(paste("Finished processing and saving raster for", i))
    } else {
      message(paste("Raster file for", i, "already exists. Skipping download."))
    }

    # Step 2: Convert the raster to a tidy data frame and save as an RDS file.
    if (file.exists(grd_filename) && !file.exists(rds_filename)) {
      message(paste("Converting raster to data frame for year:", i))
      temp_raster <- raster::brick(grd_filename)
      sst_df <- raster_to_sstdf(brick = temp_raster)
      saveRDS(sst_df, rds_filename)
      message(paste("Converted raster to data frame and saved RDS for", i))
    } else {
      message(paste(
        "RDS file for",
        i,
        "already exists or raster not found. Skipping conversion."
      ))
    }
  }
}

# Bastille function from https://github.com/kimberly-bastille/ecopull/blob/main/R/utils.R

nc_to_raster <- function(
  nc,
  varname,
  extent = c(0, 360, -90, 90),
  crop = raster::extent(280, 300, 30, 50),
  show_images = FALSE
) {
  message("Reading .nc as brick...")

  r <- raster::brick(nc, varname = varname)

  message("Setting CRS...")
  raster::crs(
    r
  ) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)

  if (show_images) {
    par(mfrow = c(1, 2))
    raster::plot(r, 1, sub = "Full dataset")
  }

  message("Cropping data...")
  ne_data <- raster::crop(r, crop)
  #ne_data <- raster::rotate(ne_data) add here for future pulls

  if (show_images) {
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1, 1))
  }

  message("Done!")

  return(ne_data)
}

# function to convert to dataframe based on
# https://towardsdatascience.com/transforming-spatial-data-to-tabular-data-in-r-4dab139f311f

raster_to_sstdf <- function(brick, rotate = TRUE) {
  if (rotate) {
    brick_r <- raster::rotate(brick)
  }
  brick_r <- raster::crop(brick_r, raster::extent(-77, -65, 35, 45))
  sstdf <- as.data.frame(raster::rasterToPoints(brick_r, spatial = TRUE))
  sstdf <- sstdf %>%
    dplyr::rename(Lon = x, Lat = y) %>%
    tidyr::pivot_longer(
      cols = starts_with("X"),
      names_to = c("year", "month", "day"),
      names_prefix = "X",
      names_sep = "\\.",
      values_to = "sst",
    )
  return(sstdf)
}

#' @title Join OISST Data to Survey Station Locations
#'
#' @description This function performs a spatial join between a data frame
#'   of survey station locations and OISST sea surface temperature (SST) data.
#'   It iterates through OISST data files and finds the nearest SST observation
#'   for each station on the same date.
#'
#' @param stations An `sf` data frame of survey stations with `year`, `yrmody`,
#'   and geometry columns.
#' @param oisst_files A character vector of file paths to the OISST data frames.
#'
#' @return An `sf` data frame with the joined OISST data for all stations,
#'   or an empty tibble if no data is found.
#'
join_oisst_to_stations <- function(stations, oisst_files) {
  # Initialize an empty list to store the results from each year.
  yearly_results <- list()

  # Loop through each OISST data file.
  for (df_path in oisst_files) {
    message(paste("Processing file:", basename(df_path)))

    # Read the OISST data for the current year.
    sstdf <- readRDS(df_path)

    # Filter stations to match the current OISST year.
    stations_yr <- stations %>%
      dplyr::filter(year == unique(sstdf$year))

    # Check if there are stations for this year before proceeding.
    if (nrow(stations_yr) > 0) {
      message("Prepping OISST data for spatial join")

      # Prepare the OISST data for the spatial join.
      sstdf_survdays <- sstdf %>%
        dplyr::mutate(
          yrmody = as.numeric(paste0(year, month, day)),
          declon = Lon,
          declat = Lat
        ) %>%
        dplyr::filter(yrmody %in% unique(stations_yr$yrmody)) %>%
        dplyr::select(-Lon, -Lat) %>%
        sf::st_as_sf(coords = c("declon", "declat"), crs = 4326, remove = FALSE)

      message("Performing spatial join for nearest-neighbor SST values")

      # Use purrr::map_dfr to perform joins and bind rows in one step.
      yr_combined <- unique(stations_yr$yrmody) %>%
        purrr::map_dfr(function(date) {
          stations_on_date <- stations_yr %>% dplyr::filter(yrmody == date)
          sst_on_date <- sstdf_survdays %>% dplyr::filter(yrmody == date)

          if (nrow(sst_on_date) > 0) {
            sf::st_join(
              stations_on_date,
              sst_on_date,
              join = nngeo::st_nn,
              k = 1,
              progress = FALSE
            )
          } else {
            return(NULL)
          }
        })

      # Add the combined results for the current year to the list.
      yearly_results[[length(yearly_results) + 1]] <- yr_combined
    }
  }

  # After the loop, combine all the yearly results into one final data frame.
  joined_data <- dplyr::bind_rows(yearly_results)

  return(joined_data)
}
