# create csv for ecodata input
# aim for similar structure to other ecodata datasets


library(dplyr)
library(ggplot2)
library(tidyr)

SOEinputs <- function(infile, season, stratlook, outfile) {
  
  splitoutput <- read.csv(infile)
  
  stratlook <- stratlook
  
  forageindex <- splitoutput %>%
    left_join(stratlook) %>%
    dplyr::select(Time, 
                  EPU = Region, 
                  "Forage Fish Biomass Estimate" = Estimate, 
                  "Forage Fish Biomass Estimate SE" = Std..Error.for.Estimate) %>%
    tidyr::pivot_longer(c("Forage Fish Biomass Estimate", "Forage Fish Biomass Estimate SE"), 
                        names_to = "Var", values_to = "Value") %>%
    dplyr::filter(EPU %in% c("MAB", "GB", "GOM", "AllEPU")) %>%
    dplyr::mutate(Units = "relative grams per stomach") %>%
    dplyr::select(Time, Var, Value, EPU, Units)
  
  forageindex$Var <- stringr::str_c(season, forageindex$Var, sep = " ")
  
  #readr::write_csv(forageindex, outfile)
  saveRDS(forageindex, outfile)
  
} 

# warning, hardcoded. obviously
stratlook_ALLsplit <- data.frame(Stratum = c("Stratum_1",
                                    "Stratum_2",
                                    "Stratum_3",
                                    "Stratum_4",
                                    "Stratum_5",
                                    "Stratum_6",
                                    "Stratum_7",
                                    "Stratum_8",
                                    "Stratum_9",
                                    "Stratum_10",
                                    "Stratum_11",
                                    "Stratum_12",
                                    "Stratum_13",
                                    "Stratum_14",
                                    "Stratum_15"),
                        Region  = c("AllEPU", 
                                    "MABGB", 
                                    "MABGBstate", 
                                    "MABGBfed", 
                                    "MAB",
                                    "GB",
                                    "GOM",
                                    "bfall",
                                    "bfin",
                                    "bfoff",
                                    "MABGBalbinshore",
                                    "MABGBothoffshore",
                                    "albbfin",
                                    "albbfall",
                                    "allother"))

stratlook_EPUonly <- data.frame(Stratum = c("Stratum_1",
                                            "Stratum_2",
                                            "Stratum_3",
                                            "Stratum_4",
                                            "Stratum_5",
                                            "Stratum_6"),
                                Region = c("AllEPU", 
                                           "MAB",
                                           "GB",
                                           "GOM",
                                           "SS",
                                           "allother"))



# make data files
SOEinputs(infile = "SOEpyindex/1982-2023/allagg_fall_500_lennosst_EPUonly_biascorrect/Index.csv",
          season = "Fall", 
          stratlook = stratlook_EPUonly, 
           outfile = "SOEpyindex/fallforageindex.rds")

SOEinputs(infile = "SOEpyindex/1982-2023/allagg_spring_500_lennosst_EPUonly_biascorrect/Index.csv",
          season = "Spring", 
          stratlook = stratlook_EPUonly,
           outfile = "SOEpyindex/springforageindex.rds")

# SOEinputs(infile = "pyindex/allagg_annual_500_lennosst_ALLsplit_biascorrect/Index.csv",
#           season = "Annual",
#           stratlook = stratlook_ALLsplit,
#            outfile = "toSOE/annualforageindex.rds")



# test plot
# foragewide <- forageindex %>%
#   pivot_wider(names_from = Var, values_from = Value)
# 
# 
# ggplot(foragewide, aes(x=Time, y=`Forage Fish Biomass Estimate`, colour = EPU)) +
#   geom_errorbar(aes(ymin=`Forage Fish Biomass Estimate`+`Forage Fish Biomass Estimate SE`, 
#                     ymax=`Forage Fish Biomass Estimate`-`Forage Fish Biomass Estimate SE`))+
#   geom_point()+
#   geom_line()

