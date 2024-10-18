# Streamlined version of CreateVASTInputs.Rmd for operational updates to forage index indicators
# October 2023
#   This one is updating with 2022 NEFSC and NEAMAP data and OISST
#   To be used in the 2024 State of the Ecosystem report
# October 2024, added Brian's new input dataset, all else the same

library(tidyverse)
library(here)
library(dendextend)

# Load NEFSC stomach data received from Brian Smith

# New full dataset Septemer 2024 from https://drive.google.com/drive/folders/11aJkYoE1eVExKig-9rcMrgJ0V_GBIgkn
# downloaded datafile allfh.rmd.epu.Rdata and put it in my fhdat folder on this repo

# object is called `allfh`
load(here("fhdat/allfh.rmd.epu.Rdata"))

# Prior to Oct 2024, datasets were appended
# object is called `allfh`
# load(here("fhdat/allfh.Rdata"))
# 
# #object is called allfh21
# load(here("fhdat/allfh21.Rdata"))
# 
# #object is called allfh22
# load(here("fhdat/allfh22.Rdata"))
# 
# # bind all NEFSC stomach datasets
# allfh <- allfh %>%
#   dplyr::bind_rows(allfh21) |>
#   dplyr::bind_rows(allfh22)

###############################################################################
# 2024 note: diet overlap could be updated with new diet data, but 
# I DONT WANT THE PREDATOR LIST TO CHANGE SLIGHTLY EVERY YEAR WITHOUT TESTING
# Revisit after several years, do comparisons, then decide whether to update
#
# read predator similarity info to generate predator list
# Input NEFSC food habits overlap matrix:

dietoverlap <- read_csv(here("fhdat/tgmat.2022-02-15.csv"))

# use dendextend functions to get list
d_dietoverlap <- dist(dietoverlap)

guilds <- hclust(d_dietoverlap, method = "complete")

dend <- as.dendrogram(guilds)

dend <- color_branches(dend, k=6) # Brian uses 6 categories

labels(dend) <- paste(as.character(names(dietoverlap[-1]))[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")

pisccomplete <- partition_leaves(dend)[[
  which_node(dend, c("Bluefish..S(37)", "Bluefish..M(36)", "Bluefish..L(35)"))
]]


# Filter NEFSC food habits data with predator list
pisccompletedf <- data.frame("COMNAME" = toupper(str_remove(pisccomplete, "\\..*")),
                             "SizeCat" = str_remove(str_extract(pisccomplete, "\\..*[:upper:]+"), "\\.."),
                             "feedguild" = "pisccomplete")

fh.nefsc.pisc.pisccomplete <- allfh %>%
  #filter(pynam != "EMPTY") %>%
  left_join(pisccompletedf, by = c("pdcomnam" = "COMNAME",
                                   "sizecat" = "SizeCat")) %>%
  filter(!is.na(feedguild))


##############################################################################
# Get prey list from NEFSC and NEAMAP

preycount  <- fh.nefsc.pisc.pisccomplete %>%
  #group_by(year, season, pdcomnam, pynam) %>%
  group_by(pdcomnam, pynam) %>%
  summarise(count = n()) %>%
  #arrange(desc(count))
  pivot_wider(names_from = pdcomnam, values_from = count)


gencomlist <- allfh %>%
  select(pynam, pycomnam2, gencom2) %>%
  distinct()

NEFSCblueprey <- preycount %>%
  #filter(BLUEFISH > 9) %>%
  filter(!pynam %in% c("EMPTY", "BLOWN",
                       "FISH", "OSTEICHTHYES",
                       "ANIMAL REMAINS",
                       "FISH SCALES")) %>%
  #filter(!str_detect(pynam, "SHRIMP|CRAB")) %>%
  left_join(gencomlist) %>%
  filter(!gencom2 %in% c("ARTHROPODA", "ANNELIDA",
                         "CNIDARIA", "UROCHORDATA",
                         "ECHINODERMATA", "WORMS",
                         "BRACHIOPODA", "COMB JELLIES",
                         "BRYOZOA", "SPONGES",
                         "MISCELLANEOUS", "OTHER")) %>%
  arrange(desc(BLUEFISH))

# March 2023, formally add NEAMAP to prey decisions
NEAMAPblueprey <- read.csv(here("fhdat/Full Prey List_Common Names.csv")) %>%
  #filter(BLUEFISH > 9) %>%
  filter(!SCIENTIFIC.NAME %in% c("Actinopterygii", "fish scales",
                                 "Decapoda (megalope)", 
                                 "unidentified material",
                                 "Plantae",
                                 "unidentified animal"))

NEAMAPprey <- NEAMAPblueprey %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, BLUEFISH) %>%
  dplyr::filter(!is.na(BLUEFISH)) %>%
  dplyr::mutate(pynam2 = tolower(SCIENTIFIC.NAME),
                pynam2 = stringr::str_replace(pynam2, "spp.", "sp")) %>%
  dplyr::rename(NEAMAP = BLUEFISH)


NEFSCprey <- NEFSCblueprey %>%
  dplyr::select(pycomnam2, pynam, BLUEFISH) %>%
  dplyr::filter(!is.na(BLUEFISH)) %>%
  dplyr::mutate(pynam2 = tolower(pynam)) %>%
  dplyr::rename(NEFSC = BLUEFISH)

# new criteria March 2023, >20 observations NEAMAP+NEFSC, but keep mackerel
# removes the flatfish order (too broad) and unid Urophycis previously in NEAMAP
blueprey <- NEFSCprey %>% 
  dplyr::full_join(NEAMAPprey) %>%
  dplyr::mutate(NEAMAP = ifelse(is.na(NEAMAP), 0, NEAMAP),
                NEFSC = ifelse(is.na(NEFSC), 0, NEFSC),
                total = NEFSC + NEAMAP,
                PREY = ifelse(is.na(SCIENTIFIC.NAME), pynam, SCIENTIFIC.NAME),
                COMMON = ifelse(is.na(COMMON.NAME), pycomnam2, COMMON.NAME),
                pynam = ifelse(is.na(pynam), toupper(pynam2), pynam)) %>%
  dplyr::arrange(desc(total)) %>%
  dplyr::filter(total>20 | pynam=="SCOMBER SCOMBRUS") %>% # >20 leaves out mackerel
  dplyr::mutate(COMMON = case_when(pynam=="ILLEX SP" ~ "Shortfin squids",
                                   pynam2=="teuthida" ~ "Unidentified squids",
                                   TRUE ~ COMMON)) %>%
  dplyr::mutate(PREY = stringr::str_to_sentence(PREY),
                COMMON = stringr::str_to_sentence(COMMON))


fh.nefsc.pisc.pisccomplete.blueprey <- fh.nefsc.pisc.pisccomplete %>%
  mutate(blueprey = case_when(pynam %in% blueprey$pynam ~ "blueprey",
                              TRUE ~ "othprey"))

###############################################################################
# Make the NEFSC dataset aggregating prey based on prey list

bluepyall_stn <- fh.nefsc.pisc.pisccomplete.blueprey %>%
  #create id linking cruise6_station
  #create season_ng spring and fall Spring=Jan-May, Fall=June-Dec
  mutate(id = paste0(cruise6, "_", station),
         year = as.numeric(year),
         month = as.numeric(month),
         season_ng = case_when(month <= 6 ~ "SPRING",
                               month >= 7 ~ "FALL",
                               TRUE ~ as.character(NA))
  ) %>%
  dplyr::select(year, season_ng, id, stratum,
                pynam, pyamtw, pywgti, pyvoli, blueprey, 
                pdcomnam, pdid, pdlen, pdsvol, pdswgt, 
                beglat, beglon, declat, declon, 
                bottemp, surftemp, setdepth) %>%
  group_by(id) %>%
  #mean blueprey g per stomach per tow: sum all blueprey g/n stomachs in tow
  mutate(bluepywt = case_when(blueprey == "blueprey" ~ pyamtw,
                              TRUE ~ 0.0),
         bluepynam = case_when(blueprey == "blueprey" ~ pynam,
                               TRUE ~ NA_character_)) 

# Optional: save at prey disaggregated stage for paper
#saveRDS(bluepyall_stn, here("fhdat/bluepyall_stn.rds"))

# Now get station data in one line
stndat <- bluepyall_stn %>%
  dplyr::select(year, season_ng, id, 
                beglat, beglon, declat, declon, 
                bottemp, surftemp, setdepth) %>%
  distinct()

#pisc stomachs in tow count pdid for each pred and sum
piscstom <- bluepyall_stn %>%
  group_by(id, pdcomnam) %>%
  summarise(nstompd = n_distinct(pdid)) %>%
  group_by(id) %>%
  summarise(nstomtot = sum(nstompd))

#mean and var pred length per tow
pisclen <- bluepyall_stn %>%
  summarise(meanpisclen = mean(pdlen),
            varpisclen = var(pdlen))

# Aggregated prey at station level with predator covariates
bluepyagg_stn <- bluepyall_stn %>%
  summarise(sumbluepywt = sum(bluepywt),
            nbluepysp = n_distinct(bluepynam, na.rm = T),
            npreysp = n_distinct(pynam),
            npiscsp = n_distinct(pdcomnam)) %>%
  left_join(piscstom) %>%
  mutate(meanbluepywt = sumbluepywt/nstomtot) %>%
  left_join(pisclen) %>%
  left_join(stndat)

# save at same stage as before, writing over old file
#saveRDS(bluepyagg_stn, here("fhdat/bluepyagg_stn.rds"))

# current dataset, fix declon, add vessel, rename NEFSC
#nefsc_bluepyagg_stn <- readRDS(here("fhdat/bluepyagg_stn.rds")) %>%
nefsc_bluepyagg_stn <- bluepyagg_stn %>%
  mutate(declon = -declon,
         vessel = case_when(year<2009 ~ "AL",
                            year>=2009 ~ "HB", 
                            TRUE ~ as.character(NA)))

##############################################################################
# Add NEAMAP to make full aggregated stomach dataset

# Read in NEAMAP updated input from Jim Gartland, reformat with same names
neamap_bluepreyagg_stn <- read_csv(here("fhdat/NEAMAP_Mean stomach weights_Bluefish Prey_Oct2023.csv")) %>%  
  mutate(vessel = "NEAMAP") %>%
  rename(id = station,
         sumbluepywt = sumbluepreywt,
         nbluepysp = nbfpreyspp,
         #npreysp = ,
         npiscsp = npiscspp,
         nstomtot = nstomtot, 
         meanbluepywt = meanbluepreywt,
         meanpisclen = meanpisclen.simple, 
         #varpisclen = ,
         season_ng = season,
         declat  = lat,
         declon = lon,
         bottemp = bWT,
         #surftemp = , 
         setdepth = depthm) 

# Read in NEAMAP 2023 input from Jim Gartland, reformat with same names
neamap_bluepreyagg_stn23 <- read_csv(here("fhdat/NEAMAP_Mean stomach weights_Bluefish Prey_Oct2024.csv")) %>%  
  mutate(vessel = "NEAMAP") %>%
  rename(id = station,
         sumbluepywt = sumbluepreywt,
         nbluepysp = nbfpreyspp,
         #npreysp = ,
         npiscsp = npiscspp,
         nstomtot = nstomtot, 
         meanbluepywt = meanbluepreywt,
         meanpisclen = meanpisclen.simple, 
         #varpisclen = ,
         season_ng = season,
         declat  = lat,
         declon = lon,
         bottemp = bWT,
         #surftemp = , 
         setdepth = depthm) 

neamap_bluepreyagg_stn <- dplyr::bind_rows(neamap_bluepreyagg_stn, neamap_bluepreyagg_stn23)

# combine NEAMAP and NEFSC
bluepyagg_stn_all <-  nefsc_bluepyagg_stn %>%
  bind_rows(neamap_bluepreyagg_stn) 

# Save before SST integration step
#saveRDS(bluepyagg_stn_all, here("fhdat/bluepyagg_stn_all.rds"))

###############################################################################
# Add SST into NEAMAP and reintegrate into full dataset

# Read back in if needed for SST
#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

NEFSCstations <- allfh %>%
  dplyr::mutate(id = paste0(cruise6, "_", station),
                year = as.numeric(year),
                month = as.numeric(month),
                day = as.numeric(day),
                declon = -declon) %>%
  dplyr::select(id, year, month, day, declat, declon) %>%
  dplyr::distinct()

# Need NEAMAP SST update! This is the old file
NEAMAPstationSST <- read.csv(here("fhdat/NEAMAP SST_2007_2022.csv"))

NEAMAPstationSST23 <- read.csv(here("fhdat/NEAMAP SST_2023.csv"))

NEAMAPstationSST <- dplyr::bind_rows(NEAMAPstationSST, NEAMAPstationSST23)

NEAMAPstations <- NEAMAPstationSST %>%
  dplyr::mutate(id = station,
                year = as.numeric(year),
                month = as.numeric(month),
                day = as.numeric(day)) %>%
  dplyr::select(id, year, month, day) %>%
  dplyr::distinct()

# remake diethauls
diethauls <- bluepyagg_stn_all %>%
  dplyr::select(id, declat, declon)

NEFSCstations <- dplyr::select(NEFSCstations, c(-declat, -declon))

Allstations <- bind_rows(NEFSCstations, NEAMAPstations)

#station id, lat lon, year month day
diethauls <- left_join(diethauls, Allstations)

#add year month day to diet data
bluepyagg_stn_all <- left_join(bluepyagg_stn_all, diethauls)

# add NEAMAP SST to surftemp field
NEAMAPidSST <- NEAMAPstationSST %>%
  mutate(id = station) %>%
  dplyr::select(id, SST)

bluepyagg_stn_all <- left_join(bluepyagg_stn_all, NEAMAPidSST, by="id") %>%
  mutate(surftemp = coalesce(surftemp, SST)) %>%
  dplyr::select(-SST)

# save merged dataset with day, month, and NEAMAP surftemp, same name
#saveRDS(bluepyagg_stn_all, here("fhdat/bluepyagg_stn_all.rds"))

###############################################################################
#Now match stations to OISST

#make an SST dataframe for 2023! Add to saved sst_data in data-raw/gridded

library(sf)
library(raster)
library(terra)
library(nngeo)

# Bastille function from https://github.com/kimberly-bastille/ecopull/blob/main/R/utils.R

nc_to_raster <- function(nc,
                         varname,
                         extent = c(0, 360, -90, 90),
                         crop = raster::extent(280, 300, 30, 50),
                         show_images = FALSE) {
  
  message("Reading .nc as brick...")
  
  r <- raster::brick(nc, varname = varname)
  
  message("Setting CRS...")
  raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)
  
  if(show_images){
    par(mfrow = c(1,2))
    raster::plot(r, 1, sub = "Full dataset")
  }
  
  message("Cropping data...")
  ne_data <- raster::crop(r, crop)
  #ne_data <- raster::rotate(ne_data) add here for future pulls
  
  if(show_images){
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1,1))
  }
  
  message("Done!")
  
  return(ne_data)
}

# function to convert to dataframe based on
# https://towardsdatascience.com/transforming-spatial-data-to-tabular-data-in-r-4dab139f311f

raster_to_sstdf <- function(brick,
                            rotate=TRUE){
  
  if(rotate) brick_r <- raster::rotate(brick)
  brick_r <- raster::crop(brick_r, raster::extent(-77,-65,35,45))
  sstdf <- as.data.frame(raster::rasterToPoints(brick_r, spatial = TRUE))
  sstdf <- sstdf %>%
    dplyr::rename(Lon = x,
                  Lat = y) %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = c("year", "month", "day"),
                        names_prefix = "X",
                        names_sep = "\\.",
                        values_to = "sst",
    )
  return(sstdf)
}

# pull the OISST data as raster brick, modified from 
# https://github.com/kimberly-bastille/ecopull/blob/main/.github/workflows/pull_satellite_data.yml

varname <- "sst"

# 1985-2022 previously pulled, processed and stored. add 2023.
# add 1981-1984 to extend back in time. No OISST before 1981.
# 1981 is only Sept-Dec so don't use

years <- 2023 #1982:1984 # 2022
for(i in years) {
  name <- paste0(i, ".nc")
  dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
  filename <- here::here("data-raw","gridded", "sst_data", paste0("test_", i, ".grd"))
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", i, ".nc")
  options(timeout = max(300, getOption("timeout")))
  download.file(url, destfile = name)
  
  text <- knitr::knit_expand(text = "test_{{year}} <- nc_to_raster(nc = name, varname = varname)
                                     raster::writeRaster(test_{{year}}, filename = filename, overwrite=TRUE)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
  unlink(name) # remove nc file to save space
  print(paste("finished",i))
}


# convert raster to dataframe
#years <- 2022
for(i in years) {
  name <- get(paste0("test_",i))
  filename <- here::here("data-raw","gridded", "sst_data", paste0("sst", i, ".rds"))
  text <- knitr::knit_expand(text = "sst{{year}} <- raster_to_sstdf(brick = name)
                                     saveRDS(sst{{year}}, filename)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
}
#read in diet data with month day fields

#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

stations <- bluepyagg_stn_all %>%
  dplyr::mutate(day = str_pad(day, 2, pad='0'),
                month = str_pad(month, 2, pad='0'),
                yrmody = as.numeric(paste0(year, month, day))) %>%
  dplyr::select(id, declon, declat, year, yrmody) %>%
  na.omit() %>%
  sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE) 



#list of SST dataframes
SSTdfs <- list.files(here("data-raw/gridded/sst_data/"), pattern = "*.rds")

dietstn_OISST <- tibble()


for(df in SSTdfs){
  sstdf <- readRDS(paste0(here("data-raw/gridded/sst_data/", df)))
  
  # keep only bluefish dates in SST year
  stationsyr <- stations %>%
    filter(year == unique(sstdf$year))
  
  # keep only sst days in bluefish dataset
  sstdf_survdays <- sstdf %>%
    dplyr::mutate(yrmody = as.numeric(paste0(year, month, day)) )%>%
    dplyr::filter(yrmody %in% unique(stationsyr$yrmody)) %>%
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  declon = Lon,
                  declat = Lat) %>%
    dplyr::select(-Lon, -Lat) %>%
    sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE)  
  
  # now join by nearest neighbor and date
  
  #https://stackoverflow.com/questions/71959927/spatial-join-two-data-frames-by-nearest-feature-and-date-in-r      
  
  yrdietOISST <- do.call('rbind', lapply(split(stationsyr, 1:nrow(stationsyr)), function(x) {
    sf::st_join(x, sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),],
                #join = st_nearest_feature
                join = st_nn, k = 1, progress = FALSE
    )
  }))
  
  #   #datatable solution--works but doesnt seem faster?
  #    df1 <- data.table(stationsyr)
  #   
  #  .nearest_samedate <- function(x) {
  #    st_join(st_as_sf(x), sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),], join = st_nearest_feature)
  #  }
  # # 
  #  yrdietOISST <- df1[, .nearest_samedate(.SD), by = list(1:nrow(df1))]
  
  dietstn_OISST <- rbind(dietstn_OISST, yrdietOISST)
  
}

# takes >10 minutes to run, save and tack on next years rather than full merge?
saveRDS(dietstn_OISST, here("data-raw/dietstn_OISST_1982_2023.rds"))

# Now join with OISST dataset

#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))
#dietstn_OISST <- readRDS(here("data-raw/dietstn_OISST.rds"))


dietstn_OISST_merge <- dietstn_OISST %>%
  dplyr::rename(declon = declon.x,
                declat = declat.x,
                year = year.x,
                oisst = sst) %>%
  dplyr::select(id, oisst) %>%
  sf::st_drop_geometry()

bluepyagg_stn_all_OISST <- left_join(bluepyagg_stn_all, dietstn_OISST_merge)

#saveRDS(bluepyagg_stn_all_OISST, here("fhdat/bluepyagg_stn_all_OISST_1982-2022.rds"))
saveRDS(bluepyagg_stn_all_OISST, here("fhdat/bluepyagg_stn_all_OISST_1982-2023.rds"))

