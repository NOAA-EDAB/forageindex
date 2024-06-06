# This is the exact VAST code used in Gaichas et al 2023, but with data years 
# extended from 1973-2022
# VAST attempt 2 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in SSTmethods.Rmd

herringagg_stn <- readRDS(here::here("fhdat/herringagg_stn_all_OISST_1982-2022.rds"))

# make SST column that uses surftemp unless missing or 0
# there are 3 surftemp 0 values in the dataset, all with oisst > 15
herringagg_stn <- herringagg_stn %>%
  dplyr::mutate(sstfill = ifelse((is.na(surftemp)|surftemp==0), oisst, surftemp))

#herringagg_stn <- bluepyagg_pred_stn

# filter to assessment years at Tony's suggestion

# code Vessel as AL=0, HB=1, NEAMAP=2

herringagg_stn_fall <- herringagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL", #) |>
        year > 1981) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanherringwt, #use bluepywt for individuals input in example
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanpisclen,
         npiscsp,
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         #oisst, #leaves out everything before 1982
         sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()

herringagg_stn_spring <- herringagg_stn %>%
  filter(season_ng == "SPRING", #) |>
         year > 1981) %>%
  mutate(AreaSwept_km2 =1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanherringwt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanpisclen,
         npiscsp,
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         #oisst, #leaves out everything before 1982
         sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()


# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302

# Fall model no data south of herring survey strata so cut domain off there

herring_spring <- c(01010, 01020, 01030, 01040, 01050, 01060, 01070, 01080, 01090, 
                    01100, 01110, 01120, 01130, 01140, 01150, 01160, 01170, 01180, 
                    01190, 01200, 01210, 01220, 01230, 01240, 01250, 01260, 01270, 
                    01280, 01290, 01300, 01360, 01370, 01380, 01390, 01400, 01610, 
                    01620, 01630, 01640, 01650, 01660, 01670, 01680, 01690, 01700, 
                    01710, 01720, 01730, 01740, 01750, 01760)
herring_fall <- c(01050, 01060, 01070, 01080, 01090, 01100, 01110, 01120, 01130, 
                  01140, 01150, 01160, 01170, 01180, 01190, 01200, 01210, 01220, 
                  01230, 01240, 01250, 01260, 01270, 01280, 01290, 01300, 01360, 
                  01370, 01380, 01390, 01400)

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510) #spring
SNE <- c(1050:1080, 1100:1120) # fall
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

# spring herring NEFSC BTS
her_spr <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% herring_spring) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# fall herring NEFSC BTS
her_fall <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% herring_fall) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# for fall models
SNE <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% SNE) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

MAB2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% MAB) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# Georges Bank EPU
GB2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% GB) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# gulf of maine EPU (for SOE)
GOM2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% GOM) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# scotian shelf EPU (for SOE)
SS2 <- FishStatsUtils::northwest_atlantic_grid %>%  
  dplyr::filter(stratum_number %in% SS) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# needed to cover the whole northwest atlantic grid--lets try without
# allother2 <- coast3nmbuffst %>%
#   dplyr::filter(!stratum_number %in% c(MAB, GB, GOM, SS)) %>%
#   dplyr::select(stratum_number2) %>%
#   dplyr::distinct()

# all epus
allEPU2 <- FishStatsUtils::northwest_atlantic_grid %>%  
  dplyr::filter(stratum_number %in% c(MAB, GB, GOM, SS)) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()
 
allEPUfall <- FishStatsUtils::northwest_atlantic_grid %>%  
  dplyr::filter(stratum_number %in% c(SNE, GB, GOM, SS)) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# # configs same FieldConfig as below but formatted differently
# FieldConfig <- c(
#   "Omega1"   = 0,   # number of spatial variation factors (0, 1, AR1)
#   "Epsilon1" = 0,   # number of spatio-temporal factors
#   "Omega2"   = 0, 
#   "Epsilon2" = 0
# ) 

# default configs, not really specified anyway
FieldConfig = matrix( "IID", ncol=2, nrow=3, 
                      dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))


RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

strata.limits.spring <- as.list(c("AllEPU" = allEPU2, 
                           "MAB" = MAB2,
                           "GB" = GB2,
                           "GOM" = GOM2,
                           "herring_spring" = her_spr))

strata.limits.fall <- as.list(c("AllEPUfall" = allEPUfall, 
                           "SNE" = SNE,
                           "GB" = GB2,
                           "GOM" = GOM2,
                           "herring_fall" = her_fall))


settings.spring = make_settings( n_x = 500, 
                          Region = "northwest_atlantic",
                          Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                          #strata.limits = list('All_areas' = 1:1e5), full area
                          strata.limits = strata.limits.spring,
                          purpose = "index2", 
                          bias.correct = TRUE,
                          #use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          #FieldConfig = FieldConfig,
                          #RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig
                          )

settings.fall = make_settings( n_x = 500, 
                                 Region = "northwest_atlantic",
                                 Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                                 #strata.limits = list('All_areas' = 1:1e5), full area
                                 strata.limits = strata.limits.fall,
                                 purpose = "index2", 
                                 bias.correct = TRUE,
                                 #use_anisotropy = FALSE,
                                 #fine_scale = FALSE,
                                 #FieldConfig = FieldConfig,
                                 #RhoConfig = RhoConfig,
                                 OverdispersionConfig = OverdispersionConfig
)


# select dataset and set directory for output

#########################################################
# Run model fall

season <- c("fall_500_lennosst_biascorrect")

working_dir <- here::here(sprintf("herringpyindex/fallfoot_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

# subset for faster testing
#herringagg_stn_fall <- herringagg_stn_fall %>% filter(Year<1990)                       

fit <- fit_model(
  settings = settings.fall, 
  #extrapolation_list = New_Extrapolation_List,
  Lat_i = herringagg_stn_fall$Lat, 
  Lon_i = herringagg_stn_fall$Lon, 
  t_i = herringagg_stn_fall$Year, 
  b_i = as_units(herringagg_stn_fall[,'Catch_g'], 'g'),
  a_i = rep(1, nrow(herringagg_stn_fall)),
  v_i = herringagg_stn_fall$Vessel,
  Q_ik = as.matrix(herringagg_stn_fall[,c("npiscsp", 
                                         "meanpisclen", 
                                         "sstfill"
                                         )]),
  #Use_REML = TRUE,
  working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

######################################################
# Run model spring

season <- c("spring_500_lennosst_biascorrect")

working_dir <- here::here(sprintf("herringpyindex/springfoot_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}                         
  
# subset for faster testing
#herringagg_stn_spring <- herringagg_stn_spring %>% filter(Year<1990) 

fit <- fit_model( settings = settings.spring,  
                 #extrapolation_list = New_Extrapolation_List,
                 Lat_i = herringagg_stn_spring[,'Lat'], 
                 Lon_i = herringagg_stn_spring[,'Lon'], 
                 t_i = herringagg_stn_spring[,'Year'], 
                 b_i = as_units(herringagg_stn_spring[,'Catch_g'], 'g'), 
                 a_i = rep(1, nrow(herringagg_stn_spring)),
                 v_i = herringagg_stn_spring$Vessel,
                 Q_ik = as.matrix(herringagg_stn_spring[,c("npiscsp", 
                                                          "meanpisclen", 
                                                          "sstfill"
                                                          )]),
                # Use_REML = TRUE,
                 working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 
