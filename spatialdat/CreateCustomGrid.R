# script to make the custom extrapolation grid used for the forage index
# original grid was created using code in 
# https://sgaichas.github.io/bluefishdiet/VASTcovariates_updatedPreds_sst_3mi.html
# and
# https://sgaichas.github.io/bluefishdiet/VASTcovariates_finalmod_3misurvstrat.html

################################################################################
# CREATE THE NEW SHAPE OR STRATA
################################################################################

# The area within and outside 3 miles of shore within MABGB needs to be defined. 
# At the moment, 3 nm is approximated as 5.556 km which is used as a buffer from 
# a high resolution coastline from the`rnaturalearth`package:
  
## Northeast:

# set bounding boxes
neus.xmin=-77
neus.xmax=-65
neus.ymin=35
neus.ymax=45

# high resolution coastline
usamap <- rnaturalearth::ne_countries(scale = "large", country = "united states of america", returnclass = "sf")[1] %>% 
  sf::st_cast("MULTILINESTRING") # get basic map of the country 

neus.bbox1 <- sf::st_set_crs(sf::st_as_sf(as(raster::extent(neus.xmin, neus.xmax, neus.ymin, neus.ymax), "SpatialPolygons")), sf::st_crs(usamap))
neus.bbox2 <- sf::st_set_crs(sf::st_as_sf(as(raster::extent(-78, -74, 42, 45), "SpatialPolygons")), sf::st_crs(usamap)) # smaller bounding box to get rid of extra lines on the map 

# just the NEUS coastline high res

neuscoast <- usamap %>% 
  sf::st_intersection(neus.bbox1) %>%  
  sf::st_difference(neus.bbox2) # gets rid of extra non coastal line 

#plot(neuscoast)

# add a 5.556 km (3 nautical mi) buffer around coastline

neuscoast_buff_3nm  <-  sf::st_buffer(neuscoast, dist = 5556)

#plot(neuscoast_buff_3nm)

# intersect buffer with the current FishStatsUtils::northwest_atlantic_grid
# make northwest atlantic grid into sf object
nwagrid_sf  <-  sf::st_as_sf(FishStatsUtils::northwest_atlantic_grid, coords = c("Lon","Lat")) %>%
  sf::st_set_crs(sf::st_crs(neuscoast))

# intersect, rearrange in same format as nwatl grid, and save
coast3nmbuff <- sf::st_intersection(nwagrid_sf,neuscoast_buff_3nm) %>%
  dplyr::mutate(Lon = as.numeric(sf::st_coordinates(.)[,1]),
                Lat = as.numeric(sf::st_coordinates(.)[,2])) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(-featurecla) %>%
  dplyr::select(stratum_number, Lon, Lat, everything())

readr::write_rds(coast3nmbuff, here("spatialdat","neus_coast3nmbuff.rds"))

#coast3nmbuff <- readRDS(here("spatialdat/neus_coast3nmbuff.rds"))

bfinshore <- c(3020, 3050, 3080, 3110, 3140, 3170, 3200, 3230, 
               3260, 3290, 3320, 3350, 3380, 3410, 3440, 3450, 3460)

bfoffshore <- c(1010, 1730, 1690, 1650, 1050, 1060, 1090, 1100, 1250, 1200, 1190, 1610)

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

MABGBinshore <- c(3010:3450, 3460, 3470, 3480, 3490, 3500, 3510, 3520:3550)

MABGBoffshore <- c(1010:1080, 1090, 1100:1120,1130:1210, 1230, 1250, 1600:1750)


coast3nmbuffst <- coast3nmbuff %>%
  dplyr::mutate(strat2 = 1) %>% #state waters = 1
  dplyr::right_join(FishStatsUtils::northwest_atlantic_grid) %>%
  dplyr::mutate(strat2 = tidyr::replace_na(strat2, 2)) %>% #replace NA with 2 for fed waters
  dplyr::mutate(strat2 = dplyr::case_when(!stratum_number %in% c(MAB, GB) ~ 0, #ignore outside MABGB
                                   TRUE ~ strat2)) %>%
  dplyr::mutate(stratum_number2 = as.numeric(paste0(stratum_number, strat2))) %>%
  dplyr::select(-strat2)

# save the modified grid
saveRDS(coast3nmbuffst, file = here("spatialdat/coast3nmbuffst.rds"))


################################################################################
# CREATE A CUSTOM EXTRAPOLATION GRID
################################################################################

# new lookups

MAB2 <- coast3nmbuffst %>% 
  dplyr::filter(stratum_number %in% MAB) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# MAB state waters
MAB2state <- MAB2 %>%
  dplyr::filter(stratum_number2 %% 10 == 1) 

# MAB federal waters
MAB2fed <- MAB2 %>%
  dplyr::filter(stratum_number2 %% 10 == 2) 

# Georges Bank EPU
GB2 <- coast3nmbuffst %>% 
  dplyr::filter(stratum_number %in% GB) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# GB state waters
GB2state <- GB2 %>%
  dplyr::filter(stratum_number2 %% 10 == 1) 

#GB federal waters
GB2fed <- GB2 %>%
  dplyr::filter(stratum_number2 %% 10 == 2) 

# whole bluefish domain MABG
MABGB2 <- dplyr::bind_rows(MAB2, GB2)

# MABGB state waters
MABGBstate <- dplyr::bind_rows(MAB2state, GB2state)

# MABGB federal waters
MABGBfed <- dplyr::bind_rows(MAB2fed, GB2fed)

# gulf of maine EPU (for SOE)
GOM2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% GOM) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# scotian shelf EPU (for SOE)
SS2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% SS) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# previous bluefish strata
bfinshore2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% bfinshore) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# additional new bluefish strata 2022
bfoffshore2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% bfoffshore) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# all bluefish strata
bfall2 <- dplyr::bind_rows(bfinshore2, bfoffshore2)

# albatross inshore strata
albinshore2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% setdiff(MABGBinshore, bfinshore)) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# Albold for WHAM input
albbfinshore <- dplyr::bind_rows(albinshore2, bfinshore2)

# Albnew for WHAM input
albbfall <- dplyr::bind_rows(albinshore2, bfall2)

# offshore of all bluefish survey strata
MABGBothoffshore2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% setdiff(MABGBoffshore, bfoffshore)) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# needed to cover the whole northwest atlantic grid
allother2 <- coast3nmbuffst %>%
  dplyr::filter(!stratum_number %in% c(MAB, GB, GOM, SS)) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# all epus
allEPU2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% c(MAB, GB, GOM, SS)) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()


Prepare_NWA_Extrapolation_Data_Fn <-
  function( strata.limits=NULL, epu_to_use = c('All', 'Georges_Bank','Mid_Atlantic_Bight','Scotian_Shelf','Gulf_of_Maine','Other')[1], projargs=NA, zone=NA, flip_around_dateline=FALSE, ... ){
    # Infer strata
    if( is.null(strata.limits)){
      strata.limits = list('All_areas'=1:1e5)
    }
    message("Using strata ", strata.limits)
    
    if(any(tolower(epu_to_use) %in% "all")) {
      epu_to_use <- c('Georges_Bank','Mid_Atlantic_Bight','Scotian_Shelf','Gulf_of_Maine','Other')
    }
    
    # Read extrapolation data
    utils::data( northwest_atlantic_grid, package="FishStatsUtils" )
    Data_Extrap <- northwest_atlantic_grid
    
    # Augment with strata for each extrapolation cell
    Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'])
    if( length(strata.limits)==1 && strata.limits[1]=="EPU" ){
      # Specify epu by 'epu_to_use'
      Data_Extrap <- Data_Extrap[Data_Extrap$EPU %in% epu_to_use, ]
      Data_Extrap$EPU <- droplevels(Data_Extrap$EPU)
      
      a_el = matrix(NA, nrow=nrow(Data_Extrap), ncol=length(epu_to_use), dimnames=list(NULL, epu_to_use) )
      Area_km2_x = Data_Extrap[, "Area_in_survey_km2"]
      for(l in 1:ncol(a_el)){
        a_el[,l] = ifelse( Data_Extrap[,'EPU'] %in% epu_to_use[[l]], Area_km2_x, 0 )
      }
    }else{
      # Specify strata by 'stratum_number'
      a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=length(strata.limits), dimnames=list(NULL,names(strata.limits))))
      
      # Survey areas
      Area_km2_x = Data_Extrap[,'Area_in_survey_km2']
      for(l in 1:ncol(a_el)){
        a_el[,l] = ifelse( Data_Extrap[,'stratum_number'] %in% strata.limits[[l]], Area_km2_x, 0 )
      }
    }
    
    # Convert extrapolation-data to an Eastings-Northings coordinate system
    #tmpUTM = Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'], zone=zone, flip_around_dateline=flip_around_dateline)
    tmpUTM = project_coordinates( X=Data_Extrap[,'Lon'], Y=Data_Extrap[,'Lat'], projargs=projargs, zone=zone, flip_around_dateline=flip_around_dateline)                                                         #$
    
    # Extra junk
    Data_Extrap = cbind( Data_Extrap, 'Include'=1)
    Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]
    
    # Return
    Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "projargs"=attr(tmpUTM,"projargs"),
                   "flip_around_dateline"=flip_around_dateline, "Area_km2_x"=Area_km2_x)
    return( Return )
  }

Prepare_NWA_Extrapolation_Data_Fn_skg <- function (strata.limits = NULL, 
                                                   epu_to_use = c("All", "Georges_Bank", "Mid_Atlantic_Bight", "Scotian_Shelf", "Gulf_of_Maine", "Other")[1],
                                                   projargs = NA, zone = NA, flip_around_dateline = FALSE, ...) 
{
  if (is.null(strata.limits)) {
    strata.limits = list(All_areas = 1:1e+05)
  }
  message("Using strata ", strata.limits)
  if (any(tolower(epu_to_use) %in% "all")) {
    epu_to_use <- c("Georges_Bank", "Mid_Atlantic_Bight", 
                    "Scotian_Shelf", "Gulf_of_Maine", "Other")
  }
  utils::data(northwest_atlantic_grid, package = "FishStatsUtils")
  Data_Extrap <- coast3nmbuffst
  Tmp = cbind(BEST_DEPTH_M = 0, BEST_LAT_DD = Data_Extrap[, 
                                                          "Lat"], BEST_LON_DD = Data_Extrap[, "Lon"])
  if (length(strata.limits) == 1 && strata.limits[1] == "EPU") {
    Data_Extrap <- Data_Extrap[Data_Extrap$EPU %in% epu_to_use, 
    ]
    Data_Extrap$EPU <- droplevels(Data_Extrap$EPU)
    a_el = matrix(NA, nrow = nrow(Data_Extrap), ncol = length(epu_to_use), 
                  dimnames = list(NULL, epu_to_use))
    Area_km2_x = Data_Extrap[, "Area_in_survey_km2"]
    for (l in 1:ncol(a_el)) {
      a_el[, l] = ifelse(Data_Extrap[, "EPU"] %in% epu_to_use[[l]], 
                         Area_km2_x, 0)
    }
  }
  else {
    a_el = as.data.frame(matrix(NA, nrow = nrow(Data_Extrap), 
                                ncol = length(strata.limits), dimnames = list(NULL, 
                                                                              names(strata.limits))))
    Area_km2_x = Data_Extrap[, "Area_in_survey_km2"]
    for (l in 1:ncol(a_el)) {
      a_el[, l] = ifelse(Data_Extrap[, "stratum_number2"] %in% 
                           strata.limits[[l]], Area_km2_x, 0)
    }
  }
  tmpUTM = project_coordinates(X = Data_Extrap[, "Lon"], Y = Data_Extrap[, 
                                                                         "Lat"], projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline)
  Data_Extrap = cbind(Data_Extrap, Include = 1)
  Data_Extrap[, c("E_km", "N_km")] = tmpUTM[, c("X", "Y")]
  Return = list(a_el = a_el, Data_Extrap = Data_Extrap, zone = attr(tmpUTM, 
                                                                    "zone"), projargs = attr(tmpUTM, "projargs"), flip_around_dateline = flip_around_dateline, 
                Area_km2_x = Area_km2_x)
  return(Return)
}

strata.limits <- as.list(c("AllEPU" = allEPU2, 
                           "MABGB" = MABGB2,
                           "MABGBstate" = MABGBstate,
                           "MABGBfed" = MABGBfed,
                           "MAB" = MAB2,
                           "GB" = GB2,
                           "GOM" = GOM2,
                           "bfall" = bfall2,
                           "bfin" = bfinshore2,
                           "bfoff" = bfoffshore2,
                           "MABGBalbinshore" = albinshore2,
                           "MABGBothoffshore" = MABGBothoffshore2,
                           "albbfin" = albbfinshore,
                           "albbfall" = albbfall,
                           "allother" = allother2))

Extrapolation_List_VAST  <-  Prepare_NWA_Extrapolation_Data_Fn( strata.limits=strata.limits)

Extrapolation_List  <-  Prepare_NWA_Extrapolation_Data_Fn_skg( strata.limits=strata.limits)

saveRDS(Extrapolation_List, file = here("spatialdat/CustomExtrapolationList.rds"))

