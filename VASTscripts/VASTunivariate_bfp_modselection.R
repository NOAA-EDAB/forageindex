# VAST attempt 2 univariate model selection as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in SSTmethods.Rmd

bluepyagg_stn <- readRDS(here::here("fhdat/bluepyagg_stn_all_OISST.rds"))

# make SST column that uses surftemp unless missing or 0
# there are 3 surftemp 0 values in the dataset, all with oisst > 15
bluepyagg_stn <- bluepyagg_stn %>%
  dplyr::mutate(sstfill = ifelse((is.na(surftemp)|surftemp==0), oisst, surftemp))

#bluepyagg_stn <- bluepyagg_pred_stn

# filter to assessment years at Tony's suggestion

# code Vessel as AL=0, HB=1, NEAMAP=2

bluepyagg_stn_all <- bluepyagg_stn %>%
  #ungroup() %>%
  filter(#season_ng == "FALL", # Annual model for MRIP index
         year > 1984) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanbluepywt, #use bluepywt for individuals input in example
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanpisclen,
         npiscsp,
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         oisst,
         sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()

bluepyagg_stn_fall <- bluepyagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL", # Annual model for MRIP index
    year > 1984) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanbluepywt, #use bluepywt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanpisclen,
                npiscsp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                oisst,
                sstfill
  ) %>%
  na.omit() %>%
  as.data.frame()

bluepyagg_stn_spring <- bluepyagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "SPRING", # Annual model for MRIP index
    year > 1984) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanbluepywt, #use bluepywt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanpisclen,
                npiscsp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                oisst,
                sstfill
  ) %>%
  na.omit() %>%
  as.data.frame()


# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302

# use only MAB, GB, GOM, SS EPUs 
# leave out south of Cape Hatteras at Elizabeth's suggestion
# could also leave out SS?
# CHECK if these EPUs match what we use in SOE

bfinshore <- c(3020, 3050, 3080, 3110, 3140, 3170, 3200, 3230, 
              3260, 3290, 3320, 3350, 3380, 3410, 3440, 3450, 3460)

bfoffshore <- c(1010, 1730, 1690, 1650, 1050, 1060, 1090, 1100, 1250, 1200, 1190, 1610)

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

MABGBinshore <- c(3010:3450, 3460, 3470, 3480, 3490, 3500, 3510, 3520:3550)

MABGBoffshore <- c(1010:1080, 1090, 1100:1120,1130:1210, 1230, 1250, 1600:1750)

coast3nmbuffst <- readRDS(here::here("spatialdat/coast3nmbuffst.rds"))

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


# Model selection 1 (spatial, spatio-temporal effects, no covariates) options and naming:
# Use_REML = TRUE in fit_model
# Season_knots + suffix below
# _alleffectson             FieldConfig default (all IID)
# _noaniso                  FieldConfig default (all IID) and use_anistropy = FALSE
# _noomeps2                 FieldConfig 0 for Omega2, Epsilon2
# _noomeps2_noaniso         FieldConfig 0 for Omega2, Epsilon2 and use_anistropy = FALSE
# _noomeps2_noeps1          FieldConfig 0 for Omega2, Epsilon2, Epsilon1
# _noomeps2_noeps1_noaniso  FieldConfig 0 for Omega2, Epsilon2, Epsilon1 and use_anistropy = FALSE
# _noomeps12                FieldConfig both Omega, Epsilon 0
# _noomeps12_noaniso        FieldConfig both Omega, Epsilon 0 and use_anistropy = FALSE

# default configs
FieldConfig = matrix( "IID", ncol=2, nrow=3, 
                      dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# not testing alternative RhoConfigs here just noted for completeness
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

use_anisotropy <- TRUE


# Model selection 2 (covariates) options, FieldConfig default (all IID), no REML
# Season_knots + suffix below 
# _base         No vessel overdispersion or length/number covariates  (ensure same dataset)  
# _len          Predator mean length covariate
# _num          Number of predator species covariate
# _lennum       Predator mean length and number of predator species covariates
# _sst          Combined in situ and OISST covariate
# _lensst       Predator mean length and SST covariates
# _numsst       Number of predator species and SST covariates
# _lennumsst    Predator mean length, number of predator species, and SST covariates
# _eta10        Overdispersion (vessel effect) in first linear predictor (prey encounter)
# _eta11        Overdispersion (vessel effect) in both linear predictors (prey wt)


OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

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
                           "allother" = allother2))

# run all with custom extrapolation list just in case that makes a difference
New_Extrapolation_List <- readRDS(here::here("spatialdat/CustomExtrapolationList.rds"))

# list of data, settings, and directory for output for each option

mod.season <- c("all_500", "fall_500", "spring_500") #includes n knots

mod.dat <- list(bluepyagg_stn_all, bluepyagg_stn_fall, bluepyagg_stn_spring)

names(mod.dat) <- mod.season

mod.config <- c("alleffectson", "noaniso", 
                "noomeps2", "noomeps2_noaniso", 
                "noomeps2_noeps1", "noomeps2_noeps1_noaniso",
                "noomeps12", "noomeps12_noaniso")

FieldConfig1 <- matrix( "IID", ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig2 <- matrix( c("IID","IID","IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig3 <- matrix( c("IID",0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig4 <- matrix( c(0,0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))

mod.FieldConfig <- list(FieldConfig1, FieldConfig1,
                        FieldConfig2, FieldConfig2,
                        FieldConfig3, FieldConfig3,
                        FieldConfig4, FieldConfig4)

names(mod.FieldConfig) <- mod.config

mod.use_anistropy <- list(TRUE, FALSE, 
                          TRUE, FALSE,
                          TRUE, FALSE,
                          TRUE, FALSE)

names(mod.use_anistropy) <- mod.config

#########################################################
# Run model selection 1

for(season in mod.season){
  
  season <- season # c("annual_500_lennosst_ALLsplit")
  
  dat <- mod.dat[[season]]
  
  for(config in mod.config) {
    
    name <- paste0(season,"_", config)
    
    working_dir <- here::here(sprintf("pyindex_modsel1/allagg_%s/", name))
    
    if(!dir.exists(working_dir)) {
      dir.create(working_dir)
    }
    
    FieldConfig <- mod.FieldConfig[[config]]
    use_anisotropy <- mod.use_anistropy[[config]]
    
    settings <- make_settings( n_x = 500, 
                               Region = "northwest_atlantic",
                               Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                               #strata.limits = list('All_areas' = 1:1e5), full area
                               strata.limits = strata.limits,
                               purpose = "index2", 
                               bias.correct = FALSE,
                               use_anisotropy = use_anisotropy,
                               FieldConfig = FieldConfig,
                               RhoConfig = RhoConfig, #always default
                               OverdispersionConfig = OverdispersionConfig
    )
    
    
    fit <- fit_model(
      settings = settings, 
      extrapolation_list = New_Extrapolation_List,
      Lat_i = dat$Lat, 
      Lon_i = dat$Lon, 
      t_i = dat$Year, 
      b_i = as_units(dat[,'Catch_g'], 'g'),
      a_i = rep(1, nrow(dat)),
      v_i = dat$Vessel,
      #Q_ik = as.matrix(dat[,c("npiscsp", 
      #                                       "meanpisclen", 
      #                                       "sstfill"
      #                                      )]),
      Use_REML = TRUE,
      working_dir = paste0(working_dir, "/"))
    
    #saveRDS(fit, file = paste0(working_dir, "/fit.rds"))
    
    # Plot results
    plot( fit,
          working_dir = paste0(working_dir, "/"))
    
  } # end config loop
} # end season loop

#########################################################
# Define covariate combinations

mod.covar <- c("base", "len", 
               "num", "lennum", 
               "sst", "lensst", 
               "numsst", "lennumsst",
               "eta10", "eta11")

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

OverdispersionConfig1 <- c("eta1"=1, "eta2"=0)
OverdispersionConfig2 <- c("eta1"=1, "eta2"=1)

mod.eta <- list(OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig1, OverdispersionConfig2)

names(mod.eta) <- mod.covar

#########################################################
# Run model selection 2

for(season in mod.season){
  
  season <- season # c("annual_500_lennosst_ALLsplit")
  
  dat <- mod.dat[[season]]
  
  Q_ikbase  <-  NULL
  Q_iklen <- as.matrix(dat[,c("meanpisclen")])
  Q_iknum <- as.matrix(dat[,c("npiscsp")])
  Q_iklennum <- as.matrix(dat[,c("meanpisclen", "npiscsp")])
  Q_iksst <- as.matrix(dat[,c("sstfill")])
  Q_iklensst <- as.matrix(dat[,c("meanpisclen", "sstfill")])
  Q_iknumsst <- as.matrix(dat[,c("npiscsp", "sstfill")])
  Q_iklennumsst <- as.matrix(dat[,c("meanpisclen", "npiscsp", "sstfill")])
  
  mod.Qik <- list(Q_ikbase, Q_iklen, 
                  Q_iknum, Q_iklennum,
                  Q_iksst, Q_iklensst, 
                  Q_iknumsst, Q_iklennumsst,
                  Q_ikbase, Q_ikbase)
  
  names(mod.Qik) <- mod.covar
  
  for(covar in mod.covar) {
    
    name <- paste0(season,"_", covar)
    
    working_dir <- here::here(sprintf("pyindex_modsel2/allagg_%s/", name))
    
    if(!dir.exists(working_dir)) {
      dir.create(working_dir)
    }
    
    # winners of model selection 1
    use_anisotropy <- TRUE
    FieldConfig <- FieldConfig1
     
    OverdispersionConfig <- mod.eta[[covar]]
    Q_ik <- mod.Qik[[covar]]
    
    settings <- make_settings( n_x = 500, 
                               Region = "northwest_atlantic",
                               Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                               #strata.limits = list('All_areas' = 1:1e5), full area
                               strata.limits = strata.limits,
                               purpose = "index2", 
                               bias.correct = FALSE,
                               use_anisotropy = use_anisotropy,
                               FieldConfig = FieldConfig,
                               RhoConfig = RhoConfig, #always default
                               OverdispersionConfig = OverdispersionConfig
    )
    
    
    fit <- fit_model(
      settings = settings, 
      extrapolation_list = New_Extrapolation_List,
      Lat_i = dat$Lat, 
      Lon_i = dat$Lon, 
      t_i = dat$Year, 
      b_i = as_units(dat[,'Catch_g'], 'g'),
      a_i = rep(1, nrow(dat)),
      v_i = dat$Vessel,
      Q_ik = Q_ik,
      #Use_REML = TRUE,
      working_dir = paste0(working_dir, "/"))
    
    #saveRDS(fit, file = paste0(working_dir, "/fit.rds"))
    
    # Plot results
    plot( fit,
          working_dir = paste0(working_dir, "/"))
    
  } # end covar loop
} # end season loop
