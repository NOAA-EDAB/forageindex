# format center of gravity output for ecodata submission and SOE

library(dplyr)
library(ggplot2)
library(tidyr)

SOEinputsCOG <- function(infile, season, outfile) {
  
  cogout <- readRDS(infile)
  
  foragecog <- as.data.frame(cogout$COG_Table) |>
    dplyr::mutate(direction = ifelse(m==1, "Eastward", "Northward")) |>
    dplyr::select("Time" = Year,
                  "Forage Fish Center of Gravity" = COG_hat, 
                  "Forage Fish Center of Gravity SE" = SE,
                  direction) |>
    tidyr::pivot_longer(c("Forage Fish Center of Gravity", "Forage Fish Center of Gravity SE"), 
                        names_to = "Var", values_to = "Value") |>
    #direction into Var
    tidyr::unite(Var, direction:Var, sep = " ") |>
    dplyr::mutate(Units = "km",
                  EPU = "ALLEPU") |>
    dplyr::select(Time, Var, Value, EPU, Units)
  
  foragecog$Var <- stringr::str_c(season, foragecog$Var, sep = " ")
  
  #readr::write_csv(forageindex, outfile)
  saveRDS(foragecog, outfile)
  
  
}
  
# make data files
SOEinputsCOG(infile = "SOEpyindex/1982-2022/allagg_fall_500_lennosst_ALLsplit_biascorrect/cogout.rds",
          season = "Fall", 
          outfile = "SOEpyindex/fallforagecog.rds")

SOEinputsCOG(infile = "SOEpyindex/1982-2022/allagg_spring_500_lennosst_ALLsplit_biascorrect/cogout.rds",
          season = "Spring", 
          outfile = "SOEpyindex/springforagecog.rds")
