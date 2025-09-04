# --- Script Header ---
# Title: Create VAST Inputs for Forage Index Indicators
# Author: AST
# Date: September 2025
# Description: This script processes and integrates Northeast Fisheries Science Center (NEFSC) and
# Northeast Marine Area Monitoring and Assessment Program (NEAMAP) fish stomach content data with OISST
# sea surface temperature (SST) data. The primary output is a combined dataset
# aggregated at the station level, suitable for VAST modeling or other analyses of
# forage index indicators for the State of the Ecosystem report.

# Libraries & functions ----
# Load core tidyverse packages for data manipulation.
# 'here' is used for consistent file path management across different machines.
# 'dendextend' is used for hierarchical clustering visualization (though only its data output is used here).
library(tidyverse)
library(here)
library(dendextend)
library(sf) # For spatial data manipulation
library(raster) # For handling raster data
library(terra) # Another option for raster data (modern alternative)
library(nngeo) # For nearest-neighbor spatial joins

# load custom utility functions
source(here::here("test_SOEpyindex", "utils.R"))


#' @title Get a Cleaned Prey List from a Data Source
#'
#' @description This function filters and cleans a raw prey dataset
#'   from either NEFSC or NEAMAP, standardizing the column names
#'   and removing non-prey items.
#'
#' @param data A data frame containing the raw prey data.
#' @param source_type A character string, either "NEFSC" or "NEAMAP",
#'   to specify the data source.
#'
#' @return A tidy data frame with cleaned prey data, including a column
#'   for the prey count from the specified source.
#'
#' @details The function handles the unique column names and filtering criteria
#'   for each data source.
get_prey_list <- function(data, source_type) {
  # Common list of non-prey items to filter out
  non_prey_nefsc <- c(
    "EMPTY",
    "BLOWN",
    "FISH",
    "OSTEICHTHYES",
    "ANIMAL REMAINS",
    "FISH SCALES",
    "ARTHROPODA",
    "ANNELIDA",
    "CNIDARIA",
    "UROCHORDATA",
    "ECHINODERMATA",
    "WORMS",
    "BRACHIOPODA",
    "COMB JELLIES",
    "BRYOZOA",
    "SPONGES",
    "MISCELLANEOUS",
    "OTHER"
  )

  non_prey_neamap <- c(
    "Actinopterygii",
    "fish scales",
    "Decapoda (megalope)",
    "unidentified material",
    "Plantae",
    "unidentified animal"
  )

  if (source_type == "NEFSC") {
    # Process NEFSC data
    prey_list <- data %>%
      # Ensure the prey common names are available for joining
      left_join(
        allfh %>%
          tibble::as_tibble() |>
          dplyr::select(pynam, pycomnam2, gencom2) %>%
          dplyr::distinct(),
        by = "pynam"
      ) %>%
      dplyr::filter(!pynam %in% non_prey_nefsc) %>%
      dplyr::filter(!gencom2 %in% non_prey_nefsc) %>% # Use the same filter for general categories
      dplyr::select(pycomnam2, pynam, BLUEFISH) %>%
      dplyr::filter(!is.na(BLUEFISH)) %>%
      dplyr::mutate(pynam2 = tolower(pynam), NEFSC = BLUEFISH) %>%
      dplyr::select(-BLUEFISH)
  } else if (source_type == "NEAMAP") {
    # Process NEAMAP data
    prey_list <- data %>%
      dplyr::filter(!SCIENTIFIC.NAME %in% non_prey_neamap) %>%
      dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, BLUEFISH) %>%
      dplyr::filter(!is.na(BLUEFISH)) %>%
      dplyr::mutate(
        pynam2 = tolower(SCIENTIFIC.NAME),
        pynam2 = stringr::str_replace(pynam2, "spp.", "sp"),
        NEAMAP = BLUEFISH
      ) %>%
      dplyr::select(-BLUEFISH)
  } else {
    stop("Invalid `source_type`. Please use 'NEFSC' or 'NEAMAP'.")
  }
  return(prey_list)
}


# Data ----

# load data
load(here("fhdat/allfh.rmd.epu.Rdata"))

# csvs

# Need NEAMAP SST update! This is the old file
NEAMAPstationSST22 <- read.csv(here("fhdat/NEAMAP SST_2007_2022.csv"))
NEAMAPstationSST23 <- read.csv(here("fhdat/NEAMAP SST_2023.csv"))

## read in piscivore predator list
pisccompletedf <- read.csv(here::here("test_SOEpyindex/pisccomplete.csv"))

# NEAMAP data
NEAMAPblueprey <- read.csv(here::here("fhdat/Full Prey List_Common Names.csv"))

# Analyses ----

## Update prey list ----

# This section identifies the key predators (piscivores) and prey species
# based on diet overlap and observation counts.

# Get prey lists from both NEFSC and NEAMAP data.
# this analysis uses a static prey list from NEAMAP
# but theoretically could change for NEFSC each year because it uses the new allfh data

# Filter the raw food habits data to include only the defined piscivore predators.
fh.nefsc.pisc.pisccomplete <- allfh %>%
  left_join(
    pisccompletedf,
    by = c("pdcomnam" = "COMNAME", "sizecat" = "SizeCat")
  ) %>%
  filter(!is.na(feedguild))

# NEFSC data
preycount <- fh.nefsc.pisc.pisccomplete %>%
  group_by(pdcomnam, pynam) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = pdcomnam, values_from = count, values_fill = 0)

# Use the function to get the cleaned NEFSC prey list
NEFSCprey <- get_prey_list(data = preycount, source_type = "NEFSC")

# Use the function to get the cleaned NEAMAP prey list
NEAMAPprey <- get_prey_list(data = NEAMAPblueprey, source_type = "NEAMAP")

# Combine NEFSC and NEAMAP prey lists and apply a final filter.
# This identifies the full prey list relevant to bluefish diet analysis.
blueprey <- NEFSCprey %>%
  dplyr::full_join(NEAMAPprey) %>%
  dplyr::mutate(
    NEAMAP = ifelse(is.na(NEAMAP), 0, NEAMAP),
    NEFSC = ifelse(is.na(NEFSC), 0, NEFSC),
    total = NEFSC + NEAMAP,
    PREY = ifelse(is.na(SCIENTIFIC.NAME), pynam, SCIENTIFIC.NAME),
    COMMON = ifelse(is.na(COMMON.NAME), pycomnam2, COMMON.NAME),
    pynam = ifelse(is.na(pynam), toupper(pynam2), pynam)
  ) %>%
  dplyr::arrange(desc(total)) %>%
  dplyr::filter(total > 20 | pynam == "SCOMBER SCOMBRUS") %>% # >20 leaves out mackerel
  dplyr::mutate(
    COMMON = case_when(
      pynam == "ILLEX SP" ~ "Shortfin squids",
      pynam2 == "teuthida" ~ "Unidentified squids",
      TRUE ~ COMMON
    )
  ) %>%
  dplyr::mutate(
    PREY = stringr::str_to_sentence(PREY),
    COMMON = stringr::str_to_sentence(COMMON)
  )


### QA: compare to original prey list ----
write.csv(blueprey, here::here("test_SOEpyindex", "new_blueprey.csv"))
new_blueprey <- read.csv(here::here("test_SOEpyindex", "new_blueprey.csv")) |>
  dplyr::select(colnames(blueprey))

orig_blueprey <- read.csv(here::here("test_SOEpyindex", "orig_blueprey.csv"))
head(blueprey)
head(orig_blueprey)

orig_NEFSCprey <- read.csv(here::here("test_SOEpyindex", "orig_NEFSCprey.csv"))
head(NEFSCprey)
head(orig_NEFSCprey)

orig_NEAMAPprey <- read.csv(here::here(
  "test_SOEpyindex",
  "orig_NEAMAPprey.csv"
))
head(NEAMAPprey)
head(orig_NEAMAPprey)

dplyr::anti_join(orig_blueprey, new_blueprey)
dplyr::anti_join(new_blueprey, orig_blueprey)

test <- dplyr::bind_rows(
  orig_blueprey |> dplyr::mutate(source = "orig", row = dplyr::row_number()),
  new_blueprey |> dplyr::mutate(source = "new", row = dplyr::row_number())
)

## Merge prey list into NEFSC data ----
# Tag prey in the main data frame based on the combined prey list.
# this is the data frame that is carried forward in the analysis
fh.nefsc.pisc.pisccomplete.blueprey <- fh.nefsc.pisc.pisccomplete %>%
  dplyr::mutate(
    blueprey = if_else(pynam %in% blueprey$pynam, "blueprey", "othprey")
  )

## Aggregate NEFSC Data at the Station Level ----
# This section aggregates the detailed stomach content data to create a single row
# per survey station, calculating various summary statistics.

# Calculate prey weight and name at the individual level.
bluepyall_stn <- fh.nefsc.pisc.pisccomplete.blueprey %>%
  #create id linking cruise6_station
  #create season_ng spring and fall Spring=Jan-May, Fall=June-Dec
  mutate(
    id = paste0(cruise6, "_", station),
    year = as.numeric(year),
    month = as.numeric(month),
    season_ng = case_when(
      month <= 6 ~ "SPRING",
      month >= 7 ~ "FALL",
      TRUE ~ as.character(NA)
    )
  ) %>%
  dplyr::select(
    year,
    season_ng,
    id,
    stratum,
    pynam,
    pyamtw,
    pywgti,
    pyvoli,
    blueprey,
    pdcomnam,
    pdid,
    pdlen,
    pdsvol,
    pdswgt,
    beglat,
    beglon,
    declat,
    declon,
    bottemp,
    surftemp,
    setdepth
  ) %>%
  group_by(id) %>%
  #mean blueprey g per stomach per tow: sum all blueprey g/n stomachs in tow
  mutate(
    bluepywt = case_when(blueprey == "blueprey" ~ pyamtw, TRUE ~ 0.0),
    bluepynam = case_when(blueprey == "blueprey" ~ pynam, TRUE ~ NA_character_)
  )

# Now get station data in one line
stndat <- bluepyall_stn %>%
  dplyr::select(
    year,
    season_ng,
    id,
    beglat,
    beglon,
    declat,
    declon,
    bottemp,
    surftemp,
    setdepth
  ) %>%
  distinct()

#pisc stomachs in tow count pdid for each pred and sum
piscstom <- bluepyall_stn %>%
  group_by(id, pdcomnam) %>%
  summarise(nstompd = n_distinct(pdid)) %>%
  group_by(id) %>%
  summarise(nstomtot = sum(nstompd))

#mean and var pred length per tow
pisclen <- bluepyall_stn %>%
  summarise(meanpisclen = mean(pdlen), varpisclen = var(pdlen))

# Aggregated prey at station level with predator covariates
bluepyagg_stn <- bluepyall_stn %>%
  summarise(
    sumbluepywt = sum(bluepywt),
    nbluepysp = n_distinct(bluepynam, na.rm = T),
    npreysp = n_distinct(pynam),
    npiscsp = n_distinct(pdcomnam)
  ) %>%
  left_join(piscstom) %>%
  mutate(meanbluepywt = sumbluepywt / nstomtot) %>%
  left_join(pisclen) %>%
  left_join(stndat)

# save at same stage as before, writing over old file
#saveRDS(bluepyagg_stn, here("fhdat/bluepyagg_stn.rds"))

# current dataset, fix declon, add vessel, rename NEFSC
#nefsc_bluepyagg_stn <- readRDS(here("fhdat/bluepyagg_stn.rds")) %>%
nefsc_bluepyagg_stn <- bluepyagg_stn %>%
  mutate(
    declon = -declon,
    vessel = case_when(
      year < 2009 ~ "AL",
      year >= 2009 ~ "HB",
      TRUE ~ as.character(NA)
    )
  )

### QA: compare to original NEFSC aggregated data ----
write.csv(
  nefsc_bluepyagg_stn,
  here::here(
    "test_SOEpyindex",
    "new_nefsc_bluepyagg_stn.csv"
  )
)
new_nefsc_bluepyagg_stn <- read.csv(here::here(
  "test_SOEpyindex",
  "new_nefsc_bluepyagg_stn.csv"
)) |>
  dplyr::select(colnames(nefsc_bluepyagg_stn))

orig_nefsc_bluepyagg_stn <- read.csv(here::here(
  "test_SOEpyindex",
  "orig_nefsc_bluepyagg_stn.csv"
)) |>
  dplyr::select(colnames(nefsc_bluepyagg_stn))

head(orig_nefsc_bluepyagg_stn)
head(new_nefsc_bluepyagg_stn)

dplyr::anti_join(new_nefsc_bluepyagg_stn, orig_nefsc_bluepyagg_stn)
dplyr::anti_join(head(new_nefsc_bluepyagg_stn), head(orig_nefsc_bluepyagg_stn))
dplyr::anti_join(head(orig_nefsc_bluepyagg_stn), head(new_nefsc_bluepyagg_stn))

## Combine NEFSC and NEAMAP Datasets ----
# This section reads in the NEAMAP data and combines it with the processed NEFSC data.

neamap_bluepreyagg_stn <- process_neamap_data(
  "fhdat/NEAMAP_Mean stomach weights_Bluefish Prey_Oct2023.csv"
)
neamap_bluepreyagg_stn23 <- process_neamap_data(
  "fhdat/NEAMAP_Mean stomach weights_Bluefish Prey_Oct2024.csv"
)

# Combine the two NEAMAP datasets.
neamap_bluepreyagg_stn <- dplyr::bind_rows(
  neamap_bluepreyagg_stn,
  neamap_bluepreyagg_stn23
)

# Combine NEFSC and NEAMAP datasets.
bluepyagg_stn_all <- dplyr::bind_rows(
  nefsc_bluepyagg_stn,
  neamap_bluepreyagg_stn
)

## Add month and day to NEFSC data ----
# year and month were dropped earlier in the analysis, not sure why
# but they are added back in here

# get NEFSC station and date data
NEFSCstations <- allfh %>%
  dplyr::mutate(
    id = paste0(cruise6, "_", station),
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  dplyr::select(id, year, month, day) %>%
  dplyr::distinct()

# NEAMAP station data comes from SST data
NEAMAPstationSST <- dplyr::bind_rows(NEAMAPstationSST22, NEAMAPstationSST23)

NEAMAPstations <- NEAMAPstationSST %>%
  dplyr::mutate(
    id = station,
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  dplyr::select(id, year, month, day) %>%
  dplyr::distinct()

Allstations <- bind_rows(NEFSCstations, NEAMAPstations)

#station id, lat lon, year month day

# remake diethauls
# id, lat, long from bluepyagg_stn_all
diethauls <- bluepyagg_stn_all %>%
  dplyr::select(id, declat, declon) |>
  # add year month day from Allstations
  dplyr::left_join(Allstations) |>
  dplyr::distinct()

# add year month day to diet data
bluepyagg_stn_all <- left_join(bluepyagg_stn_all, diethauls)

## Fill missing NEFSC SSTs with NEAMAP temperature data ----
# Add SST into NEAMAP and reintegrate into full dataset

# add NEAMAP SST to surftemp field
NEAMAPidSST <- NEAMAPstationSST %>%
  mutate(id = station) %>%
  dplyr::select(id, SST)

bluepyagg_stn_all <- left_join(bluepyagg_stn_all, NEAMAPidSST, by = "id") %>%
  mutate(surftemp = coalesce(surftemp, SST)) %>%
  dplyr::select(-SST)

## Integrate OISST Sea Surface Temperature Data ----
# This section adds OISST data to the combined dataset by finding the nearest
# SST measurement in time and space for each station.

# Download and process OISST data for the specified years.
download_and_process_oisst(
  years = 2023,
  varname = "sst",
  nc_to_raster = nc_to_raster,
  raster_to_sstdf = raster_to_sstdf
)

# Prepare station data for spatial join.
stations <- bluepyagg_stn_all %>%
  dplyr::mutate(
    day = stringr::str_pad(as.character(day), 2, pad = "0"),
    month = stringr::str_pad(as.character(month), 2, pad = "0"),
    yrmody = as.numeric(paste0(year, month, day))
  ) %>%
  dplyr::select(id, declon, declat, year, yrmody) %>%
  terra::na.omit() %>%
  sf::st_as_sf(coords = c("declon", "declat"), crs = 4326, remove = FALSE)
# there is a tow from 2012 with no year month or day in the dataset, removed here

# Join stations to OISST data using nearest neighbor join.
SSTdfs <- list.files(
  here("data-raw/gridded/sst_data/"),
  pattern = "*.rds",
  full.names = TRUE
)

dietstn_OISST <- join_oisst_to_stations(
  stations = stations,
  oisst_files = SSTdfs
)

## Merge OISST into diet data ----
# Coalesce `surftemp` and `oisst` to prefer the NEAMAP sensor data where available.
final_data <- left_join(
  bluepyagg_stn_all,
  dietstn_OISST %>%
    dplyr::select(id, oisst = sst) %>%
    sf::st_drop_geometry(),
  by = "id"
) %>%
  dplyr::mutate(surftemp = dplyr::coalesce(surftemp, oisst))

# Save the final dataset ----
saveRDS(
  final_data,
  here("test_SOEpyindex/test_bluepyagg_stn_all_OISST_1982-2023.rds")
)
print("Final dataset saved successfully.")

orig_data <- readRDS(here::here(
  "fhdat/bluepyagg_stn_all_OISST_1982-2023.rds"
)) |>
  dplyr::select(colnames(final_data))

dplyr::anti_join(orig_data, final_data)[1, ]
dplyr::anti_join(final_data, orig_data)[1, ]
