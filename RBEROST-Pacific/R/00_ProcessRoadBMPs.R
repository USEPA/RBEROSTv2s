###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Assign cost and efficiency data for road BMPs ####
# BY: Kelly-Anne Moffa
# DATE:  03/25/2024 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

packages <- c(
  "sf", "sp", "dplyr", "data.table", "ggplot2", "purrr", "openxlsx", "stringr", "tidyr", "foreign"
)
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
  )
)
### Set working directory to location of "Tier_1_Optimization-SSWR.5.3.2" folder -----

InPath <- ("./RBEROST-Pacific/Preprocessing/Inputs/")

user_specs_BMPs <- fread(paste(InPath,"01_UserSpecs_BMPs.csv",sep=""), data.table = FALSE) 

road_bmps <- (user_specs_BMPs %>% filter(BMP_Category == "road"))$BMP

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load relevant data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

urban_data <- read.csv(paste0(InPath, "UrbanBMPData.csv"))

road_data <- urban_data %>% filter(BMP %in% road_bmps) %>% filter(Land_Use == "UIDU") %>% select(BMP, comid, HSG, Area, effic_N:om_rmse)

road_area_tmp <- road_data %>% select(comid, HSG, Area) %>% distinct() 

road_area <- road_area_tmp %>% group_by(comid) %>% 
  mutate(area_sum = sum(Area)) %>% mutate(area_port = Area/area_sum) %>%
  ungroup() 

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Print data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

write.csv(road_data, paste0(InPath, "road_bmps.csv"), row.names = F)
write.csv(road_area, paste0(InPath, "road_areas.csv"), row.names = F)

