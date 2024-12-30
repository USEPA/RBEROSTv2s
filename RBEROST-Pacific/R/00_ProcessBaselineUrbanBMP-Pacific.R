# PURPOSE: Develop Puget Sound baseline stormwater BMP implementation  ####
# BY: Alyssa Le ####
# DATE:  6/21/2023 ####

# 1. Setup -----

packages <- c(
  'tidyverse', 'sf', 'data.table', 'units'
)

lapply(packages, library, character.only = T)

theme_set(theme_classic())

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

# 2. Load Data -----

# This data represents changes in imperviousness for buildings within MS4 boundaries, compared to
# permit thresholds for urban BMPs. The end result is the area over which we would expect stormwater
# BMPs to already be applied.
# Percent imperviousness is from 2016 NLCD; building boundaries are from ???; MS4 boundaries are from the
# state of Washington
Building_Imperv_MS4 <- st_read("C:/Users/50526/OneDrive - ICF/Everest/GIS_Data/00_WMOST/Analysis/RBEROST/Puget_Sound.gdb", 
                           layer="New_Permit_Construct_MS4")

# This data represents a summary of road area by NHD catchment from NLCD.
Road_Imperv <- fread("C:/Users/50526/ICF/Optimization Tools - General/RBEROST/Puget_Sound/From_EPA/Data_Sources/PugetSound_imperviousbyNHDPlusV2Catchment_acres.csv")

# This data represents the relevant catchments in the Puget Sound watershed that fall within the MS4 boundaries.
MS4_Catchments <- st_read("C:/Users/50526/OneDrive - ICF/Everest/GIS_Data/00_WMOST/Analysis/RBEROST/Puget_Sound.gdb", 
                      layer="PugetSound_catchments_MS4")

PugetSound_Catchments <- st_read("C:/Users/50526/OneDrive - ICF/Everest/GIS_Data/00_WMOST/Data/PugetSound_RBEROST/catchments_projected.shp")

# 3. Process Road Imperviousness Data -----

Road_Imperv_MS4 <- Road_Imperv %>%
  filter(FEATUREID %in% MS4_Catchments$featurd) %>%
  mutate(ROAD_2019 = PRIMARY_ROAD_2019+SECONDARY_ROAD_2019+TERTIARY_ROAD_THINNED_ROAD_2019,
         ROAD_2016 = PRIMARY_ROAD_2016+SECONDARY_ROAD_2016+TERTIARY_ROAD_THINNED_ROAD_2016,
         NEW_ROAD_AC = ROAD_2019-ROAD_2016,
         NEW_ROAD_FT2 = conv_unit(NEW_ROAD_AC, "acre", "ft2")) %>%
  filter(NEW_ROAD_FT2 > 2000)

# 4. Process Building Imperviousness Data -----

PugetSound_Catchments_Albers <- st_transform(PugetSound_Catchments, st_crs(Building_Imperv_MS4)$wkt)

Building_Imperv_MS4_catch <- st_join(PugetSound_Catchments_Albers, 
                                     left=TRUE, 
                                     Building_Imperv_MS4
                                     ) %>%
  st_drop_geometry() %>%
  select(featurd, NewArea_ft2) %>%
  group_by(featurd) %>%
  summarise(NewArea_ft2 = sum(NewArea_ft2, na.rm=T)) %>%
  mutate(NewArea_ac = conv_unit(NewArea_ft2, "ft2", "acre"))

# 5. Combine road and building data that meets permit requirements -----

Imperv_Permit_Area <- merge(Building_Imperv_MS4_catch %>% select(featurd, NewArea_ac), 
                            Road_Imperv_MS4 %>% select(FEATUREID, NEW_ROAD_AC), 
                            by.x="featurd", by.y="FEATUREID", all.x=T) %>%
  mutate(NEW_ROAD_AC = replace(NEW_ROAD_AC, is.na(NEW_ROAD_AC), 0),
         Road_Building_Area_ac = NEW_ROAD_AC + NewArea_ac)

# 6. Export Data -----

write.csv(
  Imperv_Permit_Area, 
  file = "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Pacific/Preprocessing/Inputs/Imperv_Area_COMID.csv"
)

