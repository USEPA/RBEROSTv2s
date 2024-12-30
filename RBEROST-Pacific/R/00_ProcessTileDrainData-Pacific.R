###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Calculate Percent of cropland that is tiledrained
# BY: Cathy Chamberlin
# DATE:  3/17/2022
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

# This code analysis follows work in ArcGIS to calculate the number of pixels from Valayamkunnath et al 2020 that are tiledrained in each comid.

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
packages <- c("dplyr", "ggplot2")
lapply(packages, library, character.only = TRUE)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

tiles <- read.csv("./Data/TileDrainPresence.txt")

cropland <- read.csv(
  "./RBEROST-Pacific/Preprocessing/Inputs/WA_streamcat_2011_cropland.csv"
)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Calculate Percent Tiled -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

percent_tiled <- tiles %>%
  rename(COMID = FEATUREID) %>%
  left_join(., cropland, by = "COMID") %>%
  mutate(
    tiles_m2 = SUM * 30 * 30,
    tiles_km2 = tiles_m2 / (1000 * 1000),
    crop_km2 = CatAreaSqKm * PctCrop2011Cat / 100,
    pct_tiled = tiles_km2 / crop_km2,
    pct_tiled_adj = case_when(
      tiles_km2 == 0 ~ 0, pct_tiled > 999 ~ 0, pct_tiled > 1 ~ 1, pct_tiled <= 1 ~ pct_tiled
      )
  ) %>%
  select(COMID, tiles_m2, tiles_km2, crop_km2, PctCrop2011Cat, pct_tiled, pct_tiled_adj)

summary(percent_tiled)
ggplot(percent_tiled) + geom_point(aes(x = crop_km2, y = tiles_km2))
ggplot(percent_tiled) + geom_histogram(aes(x = pct_tiled))
ggplot(percent_tiled) + geom_histogram(aes(x = pct_tiled_adj))


percent_tiled_write <- percent_tiled %>% select(comid = COMID, pct_tiled_adj)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Write Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
write.csv(
  percent_tiled_write, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/PctCroplandTileDrained_ICF24.csv", #*# The relative pathway should work if code is run from the RStudio Project
  row.names = FALSE
)
