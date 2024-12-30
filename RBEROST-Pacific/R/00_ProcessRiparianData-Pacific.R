### #######################################################################################
### PURPOSE: Calculate Loading and Limits on Riparian Buffers ###
### BY: Cathy Chamberlin                                      ###
### DATE:  1/3/2021                                           ###
###########################################################################################

# 1. Setup #####

packages <- c('sf', 'sp', 'stars', 'dplyr', 'tidyr')
lapply(packages, library, character.only = T)


# 2. Define Functions ####

my_st_buffer <- function(x, dist_ft) {
  ft_to_meters = 1 / 3.28084
  
  buffer_polygon <- st_buffer(
    x, 
    dist = dist_ft * ft_to_meters, 
    endCapStyle = 'ROUND',
    singleSide = FALSE,
    preserveTopology = FALSE,
    bOnlyEdges = FALSE
  )
  
  return(buffer_polygon)
}

my_merge_buffers <- function(flowline, flowlinenn, nhdarea, waterbodies) {
  flowline_barebones <- flowline %>%
    select(comid = COMID)
  flowlinenn_barebones <- flowlinenn %>%
    select(comid = COMID)
  nhdarea_barebones <- nhdarea %>%
    select(comid)
  waterbodies_barebones <- waterbodies %>%
    select(comid)
  
  everything <- rbind(
    flowline_barebones, 
    flowlinenn_barebones, 
    nhdarea_barebones, 
    waterbodies_barebones
  )
}

my_subtractwater <- function(buffer, openwater) {
  buffer %>% 
    st_union() %>%
    st_sf() %>%
    st_difference(., openwater) %>% 
    st_cast(., "POLYGON")
}

my_getintersectingbuffers <- function(buffers, targetcatchments) {
  
  intersections <- st_intersects(buffers, targetcatchments)
  
  relevantbufferindex <- which(unlist(lapply(intersections, FUN = length)) > 0)
  
  relevantbuffers <- buffers[relevantbufferindex,]
  
  return(relevantbuffers)
}

# 3. Load Data ####
## Data Paths ####
nhdplusv2path <- paste0(
  "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/",
  "PugetSoundwatershedNHDplusV2.gpkg" 
)
st_layers(nhdplusv2path)
nlcdlandcoverpath <- paste0(
  "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/",
  "NLCD_2016_Land_Cover_L48_20190424.img"
)

soilvectorpath <- paste0(
  "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/",
  "wss_gsmsoil_WA_[2016-10-13]/spatial/gsmsoilmu_a_wa.shp"
)
soiltablepath <- paste0(
  "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/",
  "wss_gsmsoil_WA_[2016-10-13]/tabular/muaggatt.txt"
)

##  Read data ####
flowline <- read_sf(nhdplusv2path, "NHDFlowline_Network")
flowlinenn <- read_sf(nhdplusv2path, "NHDFlowline_NonNetwork")
nhdarea <- read_sf(nhdplusv2path, "NHDArea")
waterbodies <- read_sf(nhdplusv2path, "NHDWaterbody")
catchments <- read_sf(nhdplusv2path, "CatchmentSP")
nlcd_dat <- read_stars(nlcdlandcoverpath)
target_comids <- read.csv(
  "./RBEROST-Pacific/Preprocessing/Inputs/01_UserSpecs_loadingtargets.csv"
)
soil_dat <- read_sf(soilvectorpath)
soil_muaggatt <- read.table(
  soiltablepath, 
  sep = "|", 
  colClasses = c(
    "character",
    "character",
    rep('NULL', 15),
    "character",
    rep("NULL", 21),
    "character"
  )
) %>%
  rename("MUSYM" = 1, "Series" = 2, "HydGrpCd" = 3, "MUKEY" = 4)


# 4. Reproject Vector Data ####

flowline <- st_transform(flowline, st_crs(nlcd_dat))
flowlinenn <- st_transform(flowlinenn, st_crs(nlcd_dat))
nhdarea <- st_transform(nhdarea, st_crs(nlcd_dat))
waterbodies <- st_transform(waterbodies, st_crs(nlcd_dat))
catchments <- st_transform(catchments, st_crs(nlcd_dat))
soil_dat <- st_transform(soil_dat, st_crs(nlcd_dat))

all(
  sapply(
    X = list(
      flowline, flowlinenn, nhdarea, waterbodies, catchments, soil_dat, nlcd_dat
    ), 
    FUN = function(x) {identical(st_crs(x), st_crs(nlcd_dat))}
  )
)

# 5. Make Buffer Masks ####

## buffer nhd+ shapes ####

flowline_20ftbuffer <- my_st_buffer(x = flowline, dist_ft = 20)
flowline_40ftbuffer <- my_st_buffer(x = flowline, dist_ft = 40)
flowline_60ftbuffer <- my_st_buffer(x = flowline, dist_ft = 60)
flowline_80ftbuffer <- my_st_buffer(x = flowline, dist_ft = 80)
flowline_100ftbuffer <- my_st_buffer(x = flowline, dist_ft = 100)
flowline_300ftbuffer <- my_st_buffer(x = flowline, dist_ft = 300)
flowline_400ftbuffer <- my_st_buffer(x = flowline, dist_ft = 400)

flowlinenn_20ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 20)
flowlinenn_40ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 40)
flowlinenn_60ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 60)
flowlinenn_80ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 80)
flowlinenn_100ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 100)
flowlinenn_300ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 300)
flowlinenn_400ftbuffer <- my_st_buffer(x = flowlinenn, dist_ft = 400)

inlandwaterarea <- nhdarea %>% filter(ftype != "SeaOcean")

nhdarea_20ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 20)
nhdarea_40ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 40)
nhdarea_60ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 60)
nhdarea_80ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 80)
nhdarea_100ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 100)
nhdarea_300ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 300)
nhdarea_400ftbuffer <- my_st_buffer(x = inlandwaterarea, dist_ft = 400)

waterbodies_20ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 20)
waterbodies_40ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 40)
waterbodies_60ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 60)
waterbodies_80ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 80)
waterbodies_100ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 100)
waterbodies_300ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 300)
waterbodies_400ftbuffer <- my_st_buffer(x = waterbodies, dist_ft = 400)

## merge the buffers ####

buffer_20ft <- my_merge_buffers(
  flowline_20ftbuffer,
  flowlinenn_20ftbuffer,
  nhdarea_20ftbuffer,
  waterbodies_20ftbuffer
)
buffer_40ft <- my_merge_buffers(
  flowline_40ftbuffer,
  flowlinenn_40ftbuffer,
  nhdarea_40ftbuffer,
  waterbodies_40ftbuffer
)
buffer_60ft <- my_merge_buffers(
  flowline_60ftbuffer,
  flowlinenn_60ftbuffer,
  nhdarea_60ftbuffer,
  waterbodies_60ftbuffer
)
buffer_80ft <- my_merge_buffers(
  flowline_80ftbuffer,
  flowlinenn_80ftbuffer,
  nhdarea_80ftbuffer,
  waterbodies_80ftbuffer
)
buffer_100ft <- my_merge_buffers(
  flowline_100ftbuffer,
  flowlinenn_100ftbuffer,
  nhdarea_100ftbuffer,
  waterbodies_100ftbuffer
)
buffer_300ft <- my_merge_buffers(
  flowline_300ftbuffer,
  flowlinenn_300ftbuffer,
  nhdarea_300ftbuffer,
  waterbodies_300ftbuffer
)
buffer_400ft <- my_merge_buffers(
  flowline_400ftbuffer,
  flowlinenn_400ftbuffer,
  nhdarea_400ftbuffer,
  waterbodies_400ftbuffer
)

## Subtract open water ####
openwater <- nhdarea %>%
  select(comid) %>%
  rbind(., waterbodies %>% select(comid)) %>%
  st_union() %>%
  st_sf()

drybuffer_20ft <- my_subtractwater(buffer_20ft, openwater)
drybuffer_40ft <- my_subtractwater(buffer_40ft, openwater)
drybuffer_60ft <- my_subtractwater(buffer_60ft, openwater)
drybuffer_80ft <- my_subtractwater(buffer_80ft, openwater)
drybuffer_100ft <- my_subtractwater(buffer_100ft, openwater)
drybuffer_300ft <- my_subtractwater(buffer_300ft, openwater)
drybuffer_400ft <- my_subtractwater(buffer_400ft, openwater)

## Select features ####

target_catchments <- catchments %>% filter(FEATUREID %in% target_comids$ComID)

relevant_drybuffer_20ft <- my_getintersectingbuffers(
  drybuffer_20ft, target_catchments
) %>% st_union()
relevant_drybuffer_40ft <- my_getintersectingbuffers(
  drybuffer_40ft, target_catchments
) %>% st_union()
relevant_drybuffer_60ft <- my_getintersectingbuffers(
  drybuffer_60ft, target_catchments
) %>% st_union()
relevant_drybuffer_80ft <- my_getintersectingbuffers(
  drybuffer_80ft, target_catchments
) %>% st_union()
relevant_drybuffer_100ft <- my_getintersectingbuffers(
  drybuffer_100ft, target_catchments
) %>% st_union()
relevant_drybuffer_300ft <- my_getintersectingbuffers(
  drybuffer_300ft, target_catchments
) %>% st_union()
relevant_drybuffer_400ft <- my_getintersectingbuffers(
  drybuffer_400ft, target_catchments
) %>% st_union()

# 6. Summarize Soil Data by Catchment ####

soil_dat_withattrs <- full_join(
  soil_dat, soil_muaggatt, by = c("MUKEY", "MUSYM")
)

all(
  sapply(
    X = list(
      soil_dat_withattrs, 
      relevant_drybuffer_20ft, 
      relevant_drybuffer_40ft, 
      relevant_drybuffer_60ft, 
      relevant_drybuffer_80ft, 
      relevant_drybuffer_100ft
    ), 
    FUN = function(x) {identical(st_crs(x), st_crs(nlcd_dat))}
  )
)

soil_20ft <- st_intersection(soil_dat_withattrs, relevant_drybuffer_20ft)
soil_40ft <- st_intersection(soil_dat_withattrs, relevant_drybuffer_40ft)
soil_60ft <- st_intersection(soil_dat_withattrs, relevant_drybuffer_60ft)
soil_80ft <- st_intersection(soil_dat_withattrs, relevant_drybuffer_80ft)
soil_100ft <- st_intersection(soil_dat_withattrs, relevant_drybuffer_100ft)

soil_20ft_bycomid <- st_intersection(catchments, soil_20ft) %>%
  group_by(FEATUREID, HydGrpCd) %>% 
  summarize(.groups = "drop")  %>% 
  mutate(
    area = geom %>% st_area(),
    HydGrpCd = case_when(HydGrpCd == "" ~ NA_character_, TRUE ~ HydGrpCd)
  ) %>% 
  as_tibble() %>% 
  drop_na() %>%
  pivot_wider(
    id_cols = FEATUREID, names_from = HydGrpCd, values_from = area
  ) %>%
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))

soil_40ft_bycomid <- st_intersection(catchments, soil_40ft) %>%
  group_by(FEATUREID, HydGrpCd) %>% 
  summarize(.groups = "drop")  %>% 
  mutate(
    area = geom %>% st_area(),
    HydGrpCd = case_when(HydGrpCd == "" ~ NA_character_, TRUE ~ HydGrpCd)
  ) %>% 
  as_tibble() %>% 
  drop_na() %>%
  pivot_wider(
    id_cols = FEATUREID, names_from = HydGrpCd, values_from = area
  ) %>%
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))

soil_60ft_bycomid <- st_intersection(catchments, soil_60ft) %>%
  group_by(FEATUREID, HydGrpCd) %>% 
  summarize(.groups = "drop")  %>% 
  mutate(
    area = geom %>% st_area(),
    HydGrpCd = case_when(HydGrpCd == "" ~ NA_character_, TRUE ~ HydGrpCd)
  ) %>% 
  as_tibble() %>% 
  drop_na() %>%
  pivot_wider(
    id_cols = FEATUREID, names_from = HydGrpCd, values_from = area
  ) %>%
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))

soil_80ft_bycomid <- st_intersection(catchments, soil_80ft) %>%
  group_by(FEATUREID, HydGrpCd) %>% 
  summarize(.groups = "drop")  %>% 
  mutate(
    area = geom %>% st_area(),
    HydGrpCd = case_when(HydGrpCd == "" ~ NA_character_, TRUE ~ HydGrpCd)
  ) %>% 
  as_tibble() %>% 
  drop_na() %>%
  pivot_wider(
    id_cols = FEATUREID, names_from = HydGrpCd, values_from = area
  ) %>%
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))

soil_100ft_bycomid <- st_intersection(catchments, soil_100ft) %>%
  group_by(FEATUREID, HydGrpCd) %>% 
  summarize(.groups = "drop")  %>% 
  mutate(
    area = geom %>% st_area(),
    HydGrpCd = case_when(HydGrpCd == "" ~ NA_character_, TRUE ~ HydGrpCd)
  ) %>% 
  as_tibble() %>% 
  drop_na() %>%
  pivot_wider(
    id_cols = FEATUREID, names_from = HydGrpCd, values_from = area
  ) %>%
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))

# 7. Write Buffers and Projected catchment layers ####

all(
  sapply(
    X = list(
      catchments, 
      relevant_drybuffer_20ft,
      relevant_drybuffer_40ft,
      relevant_drybuffer_60ft,
      relevant_drybuffer_80ft,
      relevant_drybuffer_100ft,
      relevant_drybuffer_300ft,
      relevant_drybuffer_400ft
    ), 
    FUN = function(x) {identical(st_crs(x), st_crs(nlcd_dat))}
  )
)

st_write(
  catchments, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/catchments_projected.shp"
  )
)

st_write(
  relevant_drybuffer_20ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_20ft.shp"
  )
)
st_write(
  relevant_drybuffer_40ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_40ft.shp"
  )
)
st_write(
  relevant_drybuffer_60ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_60ft.shp"
  )
)
st_write(
  relevant_drybuffer_80ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_80ft.shp"
  )
)
st_write(
  relevant_drybuffer_100ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_100ft.shp"
  )
)
st_write(
  relevant_drybuffer_300ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_300ft.shp"
  )
)
st_write(
  relevant_drybuffer_400ft, 
  dsn = paste0(
    "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/PugetSoundPilot/GISwork/", 
    "ProcessRiparianData_Pacific_R2Arc_intermediaries/buffer_400ft.shp"
  )
)

write.csv(soil_20ft_bycomid, "./Data/STATSGO2_20ft_bycomid-Pacific.csv")
write.csv(soil_40ft_bycomid, "./Data/STATSGO2_40ft_bycomid-Pacific.csv")
write.csv(soil_60ft_bycomid, "./Data/STATSGO2_60ft_bycomid-Pacific.csv")
write.csv(soil_80ft_bycomid, "./Data/STATSGO2_80ft_bycomid-Pacific.csv")
write.csv(soil_100ft_bycomid, "./Data/STATSGO2_100ft_bycomid-Pacific.csv")

# DISCONTINUITY - move to ArcGIS to clip data layers and summarize by catchment zones####

# 8. Setup ####

packages <- c('tidyverse', 'foreach', 'doParallel', 'sf', 'data.table', 'purrr')
# lapply(packages, install.packages) #*# # Run this code if packages are not installed
lapply(packages, library, character.only = T)

registerDoParallel()

# source helper functions
source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

# 9. Load Data ####

datafolder <- "./Data/" #*#

ftperkm <- 3280.84

PS_flowlines <- read_sf(
  paste0(datafolder, "PugetSoundwatershedNHDplusV2.gpkg"), "NHDFlowline_Network"
) %>%
  mutate(lengthkm_se = sqrt(
    (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + # error in each direction for each point consists of a 90% tolerance of 0.02 in (to map scale). The digitized maps have a tolerance of 0.003 in (to map scale). Not knowing exactly how these tolerances interact, I will assume that the uncertainty is similar to if they were added together. The model is using the medium resolution NHD+ data, which has a resolution of 1:100,000. Inches are then converted to kilometers.
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 )
  ) # error in stream length depends on error in the location of the start and end point, in the x and y directions, so 4 total components.
PS_catchments <- read_sf(
  paste0(datafolder, "PugetSoundwatershedNHDplusV2.gpkg"), "CatchmentSP"
) #*#

sparrow_tn_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tn.csv"
)
sparrow_tp_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tp.csv"
)

# Slope and NLCD data are read in below in parallel processing. Users can edit file paths tagged with #*#


# 10. Summarize landcover by buffer widths ####

buffer.options <- c(20, 40, 60, 80, 100, 300, 400)


catchment_crosswalk <-PS_catchments %>%
  select(id = GRIDCODE, comid = FEATUREID)
st_geometry(catchment_crosswalk) <- NULL

# This parallel process will load each NLCD data file from the data folder, format it as necessary, then combine all the NLCD data

landcovers_bycomid_tmp <- foreach(
  i = 1:length(buffer.options), 
  .combine = "rbind", 
  .packages = c("tidyverse", "data.table")
) %dopar% {
  j <- i
  
  dat <- fread(
    paste0(datafolder, "NLCD_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
  )
  
  dat.munged <- as.data.frame(t(dat)) %>% 
    slice(-2:-1) %>% 
    rename_with(.cols = everything(), .fn = ~ dat$LABEL) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(gsub("GRIDC_", "", id))) %>%
    left_join(., catchment_crosswalk, by = "id") %>%
    select(-id) %>%
    pivot_longer(cols = 1:7, names_to = "landcover") %>%
    mutate(
      buffersize = i,
      landcover = case_when(
        landcover == "Developed, Open Space" ~ "Lowdev", 
        landcover == "Developed, Low Intensity" ~ "Meddev",
        landcover == "Developed, Medium Intensity/Developed, High Intensity" ~ 
          "Highdev",
        landcover == "Deciduous Forest/Evergreen Forest/Mixed Forest" ~
          "Forest",
        landcover == "Herbaceuous" ~ "Grass",
        landcover == "Hay/Pasture" ~ "Grazing",
        landcover == "Cultivated Crops" ~ "Cropland"
      )
    )
  
  dat.munged
  
} 

# Pivot the data to wide form for easier human readibility 

landcovers_bycomid <- landcovers_bycomid_tmp %>%
  complete(comid, buffersize, landcover) %>%
  group_by(comid, landcover) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", 
    names_from = c("landcover", "buffersize"), 
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1), 
      "_", 
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))], 
      "ft"
    ), 
    .cols = !comid
  ) %>%
  mutate(cv_numpixels = 0.17) # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)

# 11. Choose slope category for each comid ####

# This parallel process will load each slope data file from the data folder, format it as necessary, then combine all the slope data


slopes_bycomid <- foreach(
  i = 1:5, .combine = "rbind"
) %do% {
  
  j <- i
  
  dat <- read.csv(
    paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
  )
  
  dat.munged <- dat %>%
    mutate(
      value = case_when(
        between(MEAN, 0, 5) ~ "0-5%", 
        between(MEAN, 5, 10) ~ "5-10%", 
        between(MEAN, 10, 15) ~ "10-15%",
        MEAN > 15 ~ ">15%"
      ),
      buffersize = i
    ) %>%
    select(comid = FEATURE, value, buffersize) 
  
  dat.munged
} %>%
  complete(comid, buffersize) %>%
  group_by(comid) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", names_from = "buffersize", values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0("meanslope_", as.numeric(.) * 20, "ft"), 
    .cols = all_of(c("1", "2", "3", "4", "5"))
  )

slopes_bycomid_dist <- foreach(
  i = 1:5, .combine = "rbind"
) %do% {
  
  j <- i
  
  dat <- read.csv(
    paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
  )
  
  dat.munged <- dat %>%
    
    rowwise() %>%
    mutate(resampledslopes = list(rnorm(n = 10, mean = MEAN, sd = STD))) %>%
    mutate(
      value = list(
        case_when(
          resampledslopes < 5 ~ "0-5%",
          between(resampledslopes, 5, 10) ~ "5-10%",
          between(resampledslopes, 10, 15) ~ "10-15%",
          resampledslopes > 15 ~ ">15%"
        )
      ),
      buffersize = i
    ) %>%
    select(comid = FEATURE, value, buffersize)
  
  dat.munged
} %>%
  complete(comid, buffersize) %>%
  group_by(comid) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", names_from = "buffersize", values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0("meanslope_", as.numeric(.) * 20, "ft"), 
    .cols = all_of(c("1", "2", "3", "4", "5"))
  )

# 12. Summarize Hydrologic Soil Group by buffer widths ####


# This parallel process will load each NATSGO data file from the data folder, format it as necessary, then combine all the NATSGO data

hsg_bycomid_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "data.table"
) %do% {
  
  j <- i
  
  dat <- fread(
    paste0(datafolder, "STATSGO2_", buffer.options[j], "ft_bycomid-Pacific.csv") #*#
  )
  
  dat.munged <- dat %>% 
    rename(comid = FEATUREID) %>%
    select(-V1) %>%
    pivot_longer(cols = -comid, names_to = "HSG") %>%
    mutate(buffersize = i)
  
  dat.munged
  
} 

# Pivot the data wider to make it more human - readable

hsg_bycomid <- hsg_bycomid_tmp %>%
  complete(comid, buffersize, HSG) %>%
  group_by(comid, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", 
    names_from = c("HSG", "buffersize"), 
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1), 
      "_", 
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))], 
      "ft"
    ), 
    .cols = !comid
  )

hsg_bycomid_toresample <- hsg_bycomid_tmp  %>%
  complete(comid, buffersize, HSG) %>%
  group_by(comid, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  ungroup() %>%
  group_by(comid, buffersize) %>%
  summarize(resampledist = list(c(rep(x = HSG, times = floor(value / 100))))) # spatial resolution here is 1 m^2, which creates unnecessarily long lists. Down sampled to 10m x 10m "pixels

# 13. Compute bank lengths for each comid ####


river.lengths <- PS_flowlines %>%
  select(comid = COMID, lengthkm = LENGTHKM, lengthkm_se, reachcode = REACHCODE) %>%
  mutate(
    totalbanklength_km = lengthkm * 2, 
    totalbanklength_ft = totalbanklength_km * ftperkm,
    totalbanklength_km_se = lengthkm_se * 2,
    totalbanklength_ft_se = totalbanklength_km_se * ftperkm
  ) 

st_geometry(river.lengths) <- NULL

# 14. Calculate Uncertainty in PLER ####

## Table 2 in Green_Credit_Report_Final.pdf gives Pollutant Load Export Rates (PLER) by land use. 
## The values are based on Table 3-1 in Attachment 3 of Appendix F of the 2017 NH MS4 permit
## https://www.epa.gov/npdes-permits/new-hampshire-small-ms4-general-permit


P.export.rates <- data.frame(
  P_SourceCategory_LandUse = c(
    "COM+IND", 
    "MFR+HDR", 
    "MDR", 
    "LDR",
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
  ),
  Ploadexport_lb_acreyear = c(
    1.78, 2.32, 1.96, 1.52, 1.34, 1.52, 1.52, 1.52, 0.03, 0.12, 0.21, 0.29, 0.37
  ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
  )
) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = P_SourceCategory_LandUse, 
    values_from = Ploadexport_lb_acreyear
  ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "P_SourceCategory_LandUse", 
    values_to = "Ploadexport_lb_acreyear"
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_P_SourceCategory_LandUse", 
    values_to = "DevPERV_Ploadexport_lb_acreyear"
  ) 

## The values are based on Table 3-2 in Attachment 3 of Appendix F of the 2017 NH MS4 permit
## https://www.epa.gov/npdes-permits/new-hampshire-small-ms4-general-permit

N.export.rates <- data.frame(
  N_SourceCategory_LandUse = c(
    "COM+IND", 
    "AllRes", 
    "AllRes", 
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
  ),
  Nloadexport_lb_acreyear = c(
    15, 14.1, 14.1, 10.5, 11.3, 11.3, 11.3, 0.3, 1.2, 2.4, 3.1, 3.6
  ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
  )
) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = N_SourceCategory_LandUse, 
    values_from = Nloadexport_lb_acreyear
  ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "N_SourceCategory_LandUse", 
    values_to = "Nloadexport_lb_acreyear"
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_N_SourceCategory_LandUse", 
    values_to = "DevPERV_Nloadexport_lb_acreyear"
  ) 

GC_landuses <- data.frame(
  LandUse = c(
    rep("LowResidential", 36), 
    rep("Residential", 25), 
    rep("Commercial+Transportation", 40)
  ),
  PercDensityofIC = 0:100 / 100
) %>%
  mutate(PercDevPERV = (1 - PercDensityofIC)) 

P.exports <- GC_landuses %>%
  full_join(., P.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Ploadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Ploadexport_lb_acreyear
  )

N.exports <- GC_landuses%>%
  full_join(., N.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Nloadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Nloadexport_lb_acreyear
  )

GC_PLER_estimates = data.frame(
  LandUse = c("LowResidential", "Residential", "Commercial+Transportation"),
  PLER_TP = c(0.55, 1.07, 1.16),
  PLER_TN = c(3.8, 6.2, 9.3)
) 

P_export_ses <- P.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER)/ sqrt(n()))

N_export_ses <- N.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER) / sqrt(n()))


# 15.Calculate Riparian Loading for each comid ####

m2peracre <- 4046.86
lbperkg <- 2.20462

# Calculate loading based on Green_Credits_Report_Final.pdf

riparianloadings <- landcovers_bycomid %>%
  select(comid, Lowdev_400ft, Meddev_300ft, Highdev_100ft, cv_numpixels) %>%
  mutate(
    contrib_lowres_m2 = as.numeric(Lowdev_400ft) * 30 * 30, # pixels are 30 m x 30 m
    contrib_lowres_acres = contrib_lowres_m2 / m2peracre, 
    contrib_res_m2 = as.numeric(Meddev_300ft) * 30 * 30,# pixels are 30 m x 30 m
    contrib_res_acres = contrib_res_m2 / m2peracre, 
    contrib_commerc_m2 = as.numeric(Highdev_100ft) * 30 * 30,# pixels are 30 m x 30 m
    contrib_commerc_acres = contrib_commerc_m2 / m2peracre,
    contrib_lowres_acres_se = 
      as.numeric(Lowdev_400ft) * cv_numpixels * 30 * 30 / m2peracre,
    contrib_res_acres_se = 
      as.numeric(Meddev_300ft) * cv_numpixels * 30 * 30 / m2peracre,
    contrib_commerc_acres_se = 
      as.numeric(Highdev_100ft) * cv_numpixels * 30 * 30 / m2peracre
  ) %>%
  mutate(
    P_lowres_PLER_lb_acyr = 0.55,
    P_res_PLER_lb_acyr = 1.07,
    P_commerc_PLER_lb_acyr = 1.16,
    N_lowres_PLER_lb_acyr = 3.8,
    N_res_PLER_lb_acyr = 6.2,
    N_commerc_PLER_lb_acyr = 9.3,
    P_lowres_PLER_lb_acyr_se = P_export_ses[[2,2]],
    P_res_PLER_lb_acyr_se = P_export_ses[[3,2]],
    P_commerc_PLER_lb_acyr_se = P_export_ses[[1,2]],
    N_lowres_PLER_lb_acyr_se = N_export_ses[[2,2]],
    N_res_PLER_lb_acyr_se = N_export_ses[[3,2]],
    N_commerc_PLER_lb_acyr_se = N_export_ses[[1,2]]
  ) %>%
  mutate(
    P_PLER_lb_yr = contrib_lowres_acres * P_lowres_PLER_lb_acyr + 
      contrib_res_acres * P_res_PLER_lb_acyr + 
      contrib_commerc_acres * P_commerc_PLER_lb_acyr,
    N_PLER_lb_yr = contrib_lowres_acres * N_lowres_PLER_lb_acyr + 
      contrib_res_acres * N_res_PLER_lb_acyr + 
      contrib_commerc_acres * N_commerc_PLER_lb_acyr,
    P_PLER_kg_yr = P_PLER_lb_yr / lbperkg,
    N_PLER_kg_yr = N_PLER_lb_yr / lbperkg
  ) %>%
  rowwise() %>%
  mutate(
    P_PLER_lb_yr_se = my_propogateerror(
      vals = list(
        if(contrib_lowres_acres > 0) {
          c(
            contrib_lowres_acres * P_lowres_PLER_lb_acyr, 
            my_propogateerror(
              vals = list(
                c(contrib_lowres_acres, contrib_lowres_acres_se), 
                c(P_lowres_PLER_lb_acyr, P_lowres_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else {c(1,0)},
        if(contrib_res_acres > 0) {
          c(
            contrib_res_acres * P_res_PLER_lb_acyr,           
            my_propogateerror(
              vals = list(
                c(contrib_res_acres, contrib_res_acres_se), 
                c(P_res_PLER_lb_acyr, P_res_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else {c(1,0)},
        if(contrib_commerc_acres > 0) {
          c(
            contrib_commerc_acres * P_commerc_PLER_lb_acyr,          
            my_propogateerror(
              vals = list(
                c(contrib_commerc_acres, contrib_commerc_acres_se), 
                c(P_commerc_PLER_lb_acyr, P_commerc_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else{c(1,0)}
      ), 
      method = "addsub"
    ),
    N_PLER_lb_yr_se = my_propogateerror(
      vals = list(
        if(contrib_lowres_acres > 0) {
          c(
            contrib_lowres_acres * N_lowres_PLER_lb_acyr, 
            my_propogateerror(
              vals = list(
                c(contrib_lowres_acres, contrib_lowres_acres_se), 
                c(N_lowres_PLER_lb_acyr, N_lowres_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else {c(1,0)},
        if(contrib_res_acres > 0) {
          c(
            contrib_res_acres * N_res_PLER_lb_acyr,           
            my_propogateerror(
              vals = list(
                c(contrib_res_acres, contrib_res_acres_se), 
                c(N_res_PLER_lb_acyr, N_res_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else {c(1,0)},
        if(contrib_commerc_acres > 0) {
          c(
            contrib_commerc_acres * N_commerc_PLER_lb_acyr,          
            my_propogateerror(
              vals = list(
                c(contrib_commerc_acres, contrib_commerc_acres_se), 
                c(N_commerc_PLER_lb_acyr, N_commerc_PLER_lb_acyr_se)
              ),
              method = "mult"
            )
          )
        } else{c(1,0)}
      ), 
      method = "addsub"
    )
  ) %>%
  mutate(
    P_PLER_kg_yr_se = P_PLER_lb_yr_se / lbperkg,
    N_PLER_kg_yr_se = N_PLER_lb_yr_se / lbperkg,
  ) %>%
  right_join(., river.lengths, by = "comid") %>%
  replace_na(
    list(
      N_PLER_kg_yr = 0, 
      P_PLER_kg_yr = 0, 
      P_PLER_kg_yr_se = 0,
      N_PLER_kg_yr_se = 0
    )
  ) %>%
  select(
    comid, 
    N_riparian_developed_kgyr = N_PLER_kg_yr, 
    P_riparian_developed_kgyr = P_PLER_kg_yr,
    N_riparian_developed_kgyr_se = N_PLER_kg_yr_se, 
    P_riparian_developed_kgyr_se = P_PLER_kg_yr_se
  ) %>% 
  left_join(
    ., sparrow_tn_out %>% select(comid, il_tn_fer, ilse_tn_fer), by = "comid"
  ) %>% 
  left_join(
    ., 
    sparrow_tp_out %>% 
      select(comid, il_tp_fer, ilse_tp_fer, il_tp_graz, ilse_tp_graz), 
    by = "comid"
  ) %>%
  mutate(
    N_riparian_kgyr = N_riparian_developed_kgyr + il_tn_fer,
    N_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(N_riparian_developed_kgyr, N_riparian_developed_kgyr_se), 
        c(il_tn_fer, ilse_tn_fer)
      ),
      method = "addsub"
    )
  ) %>%
  mutate(
    P_riparian_kgyr = P_riparian_developed_kgyr + il_tp_fer + il_tp_graz,
    P_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(P_riparian_developed_kgyr, P_riparian_developed_kgyr_se), 
        c(il_tp_fer, ilse_tp_fer),
        c(il_tp_graz, ilse_tp_graz)
      ),
      method = "addsub"
    )
  ) %>%
  select(
    comid, 
    N_riparian_kgyr, 
    P_riparian_kgyr, 
    N_riparian_kgyr_se, 
    P_riparian_kgyr_se
  )


# 16. Calculate current river length in buffer already ####


# Use the landcover data and calculate total area in forest and herbaceous.
# Divide total area by buffer width to get the bank length

sqftpersqm <- 10.7639

bufferedlengths_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  landcovers_bycomid %>%
    select(comid, contains(paste(bufwid)), cv_numpixels) %>%
    mutate(across(contains(paste(bufwid)), ~ as.numeric(.))) %>%
    rename_with(
      .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
      .cols = contains(paste(bufwid))
    ) %>%
    right_join(., river.lengths, by = "comid") %>%
    select(
      comid, 
      Forest, 
      Grass, 
      totalbanklength_ft, 
      cv_numpixels, 
      totalbanklength_ft_se
    ) %>%
    mutate(
      bufferwidth = bufwid,
      Forest = case_when(is.na(Forest) ~ 0, TRUE ~ Forest),
      Grass = case_when(is.na(Grass) ~ 0, TRUE ~ Grass)
    ) %>%
    mutate(
      Forest_area_m2 = Forest * 30 * 30,
      Grass_area_m2 = Grass * 30 * 30,
      Forest_area_ft2 = Forest_area_m2 * sqftpersqm,
      Grass_area_ft2 = Grass_area_m2 * sqftpersqm,
      Forest_area_ft2_se = Forest_area_ft2 * cv_numpixels,
      Grass_area_ft2_se = Grass_area_ft2 * cv_numpixels
    ) %>%
    mutate(
      Forest_bufferlength_ft = Forest_area_ft2 / bufferwidth,
      Grass_bufferlength_ft = Grass_area_ft2 / bufferwidth,
      Forest_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Forest_area_ft2 > 0) {
            c(Forest_area_ft2, Forest_area_ft2_se)
          } else {c(1,0)}, 
          c(bufferwidth, 0)
        ), 
        method = "div"
      ),
      Grass_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Grass_area_ft2 >0) {
            c(Grass_area_ft2, Grass_area_ft2_se)
          } else {c(1,0)}, 
          c(bufferwidth, 0)
        ), 
        method = "div"
      )
    ) %>%
    mutate(
      Forest_bufferlength_ft_rev = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft
      ),
      Forest_bufferlength_ft_rev_se = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft_se,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft_se
      ),
      Grass_bufferlength_ft_rev = case_when(
        Grass_bufferlength_ft > 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          totalbanklength_ft - Forest_bufferlength_ft_rev,
        Grass_bufferlength_ft <= 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          Grass_bufferlength_ft
      ),
      Grass_bufferlength_ft_rev_se = case_when(
        Grass_bufferlength_ft > 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          my_propogateerror(
            vals = list(
              c(totalbanklength_ft, totalbanklength_ft_se), 
              c(Forest_bufferlength_ft_rev, Forest_bufferlength_ft_rev_se)
            ), 
            method = "addsub"
          ),
        Grass_bufferlength_ft <= 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          Grass_bufferlength_ft_se
      )
    )
  
} 

# Pivot to a wider format that is more human-readible 
bufferedlengths <- bufferedlengths_tmp %>%
  select(
    comid, 
    bufferwidth_ft = bufferwidth, 
    Grass_buffer_ft = Grass_bufferlength_ft_rev, 
    Forest_buffer_ft = Forest_bufferlength_ft_rev,
    Grass_buffer_ft_se = Grass_bufferlength_ft_rev_se,
    Forest_buffer_ft_se = Forest_bufferlength_ft_rev_se
  ) %>%
  pivot_wider(
    id_cols = c(bufferwidth_ft, comid), 
    names_from = bufferwidth_ft, 
    values_from = c(
      Grass_buffer_ft, Forest_buffer_ft, Grass_buffer_ft_se, Forest_buffer_ft_se
    )
  ) %>%
  right_join(
    ., 
    river.lengths %>% 
      select(comid, totalbanklength_ft, totalbanklength_ft_se), 
    by = "comid"
  ) %>% 
  rename_with(
    .fn = ~paste0(gsub("_ft", "", .), "ft_ft"), .cols = contains("buffer")
  )


# 17. Enter Efficiency Data ####


# The data entered here was read off of Green_Credit_Report_Final.pdf

# Create the empty dataframe
base_curve_data_empty <- data.frame(
  expand.grid(
    HSG = c("A", "B", "C", "D"),
    Width = c(20, 35, 100),
    Nutrient = c("N", "P"),
    VegType = c("Grass", "Forest")
  ),
  Efficiency = NA_real_
) 

# Enter data
base_curve_data <-   mutate(
  base_curve_data_empty,
  Efficiency = case_when(
    Width == 100 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 18/100,
          HSG == "B" ~ 28/100,
          HSG == "C" ~ 48/100,
          HSG == "D" ~ 65/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 45/100,
          HSG == "B" ~ 34/100,
          HSG == "C" ~ 20/100,
          HSG == "D" ~ 11/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 13/100,
          HSG == "B" ~ 23/100,
          HSG == "C" ~ 39/100,
          HSG == "D" ~ 52/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 36/100,
          HSG == "B" ~ 27/100,
          HSG == "C" ~ 16/100,
          HSG == "D" ~ 9/100
        )
      )
    ),
    Width == 35 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 5/100,
          HSG == "B" ~ 8/100,
          HSG == "C" ~ 14/100,
          HSG == "D" ~ 20/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 30/100,
          HSG == "B" ~ 22/100,
          HSG == "C" ~ 13/100,
          HSG == "D" ~ 8/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 4/100,
          HSG == "B" ~ 6/100,
          HSG == "C" ~ 12/100,
          HSG == "D" ~ 15/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 24/100,
          HSG == "B" ~ 18/100,
          HSG == "C" ~ 10/100,
          HSG == "D" ~ 6/100
        )
      )
    ),
    Width == 20 ~ case_when(
      VegType == "Forest" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 2/100,
          HSG == "B" ~ 4/100,
          HSG == "C" ~ 8/100,
          HSG == "D" ~ 10/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 20/100,
          HSG == "B" ~ 13/100,
          HSG == "C" ~ 9/100,
          HSG == "D" ~ 5/100
        )
      ),
      VegType == "Grass" ~ case_when(
        Nutrient == "N" ~ case_when(
          HSG == "A" ~ 2/100,
          HSG == "B" ~ 4/100,
          HSG == "C" ~ 6/100,
          HSG == "D" ~ 8/100
        ),
        Nutrient == "P" ~ case_when(
          HSG == "A" ~ 16/100,
          HSG == "B" ~ 12/100,
          HSG == "C" ~ 7/100,
          HSG == "D" ~ 4/100
        )
      )
    )
  )
)  


# 18. Compute efficiency curves ####

# Phosphorus curves are best fit with an exponential function and Nitrogen curves with a linear function


# Phosphorus curves are best fit with an exponential function and Nitrogen curves with a linear function

basecurves <- tibble(
  foreach(
    i = unique(base_curve_data$HSG), 
    .combine = "rbind",
    .packages = c("foreach", "tidyverse"),
    .export = "base_curve_data"
  ) %do% {
    foreach(j = unique(base_curve_data$VegType), .combine = "rbind") %do% {
      foreach(k = unique(base_curve_data$Nutrient), .combine = "rbind") %do% {
        mod1 <- with(
          
          base_curve_data[
            with(
              base_curve_data, which(HSG == i & VegType == j & Nutrient == k)
            ), 
          ],
          if(k == "N") {
            lm(Efficiency ~ (Width))
          } else if(k == "P") {
            lm(Efficiency ~ log(Width))
          }
        )
        
        adj.rsquared <- summary(mod1)$adj.r.squared
        
        curve.form <- if(k == "N") {
          "coef.3 * (coef.1 + coef.2 * x)"
        } else if(k == "P") {
          " coef.3 * (coef.1 + coef.2 * log(x))"
        }
        
        tibble(
          HSG = i, 
          VegType = j, 
          Nutrient = k, 
          Slope = c("0-5%", "5-10%", "10-15%", ">15%"),
          coef.1 = coef(mod1)[1], 
          coef.2 = coef(mod1)[2],
          coef.3 = c(1, 0.75, 0.5, 0),
          curve.form = curve.form,
          adj.r.squared = adj.rsquared
        ) %>%
          rowwise() %>%
          mutate(
            Efficiency = gsub(
              "coef.3", coef.3, gsub(
                "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
              )
            )
          ) 
      }
    }
  }
)

# 19. Write efficiency curves and limits for each comid ####

riparianefficiencies_bycomid_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  river.lengths %>%
    select(comid) %>%
    left_join(
      ., 
      hsg_bycomid %>% 
        select(comid, contains(paste(bufwid))) %>%
        mutate(across(contains(paste(bufwid)), ~ as.numeric(.))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
        ), 
      by= "comid"
    ) %>%
    mutate(
      HSG_N = case_when(
        A > 0 ~ "A", B > 0 ~ "B", C > 0 ~ "C", D > 0 ~ "D", TRUE ~ "A"
      ),
      HSG_P = case_when(
        D > 0 ~ "D", C > 0 ~ "C", B > 0 ~ "B", A > 0 ~ "A", TRUE ~ "D"
      ),
      bufferwidth_ft = bufwid
    ) %>%
    left_join(
      ., 
      slopes_bycomid %>%  
        select(comid, contains(paste(bufwid))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
        ), 
      by = "comid"
    )
  
} 

# format the curves for writing to csv

riparianefficiencies_bycomid_vals <- riparianefficiencies_bycomid_tmp %>%
  select(
    comid, 
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope = meanslope
  ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
  ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
  ) %>%
  complete(comid, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(comid, Nutrient, bufferwidth_ft) %>%
  fill(HSG, Slope, .direction = "downup") %>%
  mutate(Slope = replace_na(Slope, ">15%")) %>% # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal
  left_join(., basecurves, by = c("HSG", "VegType", "Nutrient", "Slope")) %>%
  rowwise() %>%
  mutate(
    Efficiency = gsub(
      "coef.3", coef.3, gsub(
        "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
      )
    )
  ) %>%
  select(comid, Nutrient, VegType, bufferwidth_ft, Efficiency) %>%
  pivot_wider(
    id_cols = c(comid, Nutrient, VegType, bufferwidth_ft), 
    names_from = c(Nutrient, VegType, bufferwidth_ft), 
    values_from = Efficiency
  ) %>%
  rename_with(.fn = ~paste0(., "ft"), .cols = -comid)

# Combine all the necessary data to choose an efficiency curve

riparianefficiencies_bycomid_tmp_dist <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %do% {
  
  bufwid <- buffer.options[i]
  
  river.lengths %>%
    select(comid) %>%
    left_join(
      ., 
      hsg_bycomid_toresample %>% 
        filter(buffersize == i),
      by= "comid"
    ) %>%
    rowwise() %>%
    mutate(
      HSG_N = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE), 
          error = function(e) {list()}
        )
      ),
      HSG_P = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE), 
          error = function(e) {list()})
      ),
      bufferwidth_ft = bufwid
    )  %>%
    left_join(
      .,
      slopes_bycomid_dist %>%
        select(comid, contains(paste(bufwid))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .),
          .cols = contains(paste(bufwid))
        ),
      by = "comid"
    )
  
} 
# format the curves for writing to csv

riparianefficiencies_bycomid_dist <- riparianefficiencies_bycomid_tmp_dist %>%
  select(
    comid,
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope = meanslope
  ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
  ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
  ) %>%
  complete(comid, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(comid, Nutrient, bufferwidth_ft) %>%
  fill(HSG, Slope, .direction = "downup") %>%
  mutate(Slope = replace_na(Slope, list(rep(">15%", 10)))) %>% # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal
  mutate(
    Efficiency_uncertainty = pmap_chr(
      .l = list(x = HSG, y = Slope, Nutrient, VegType),
      .f = ~ {
        nut = ..3
        a = ..4
        x = .x
        y = .y
        paste0(
          "c('",
          paste(
            c(
              basecurves$Efficiency[
                pmap_int(
                  .l = list(x = x, y = y),
                  .f = ~which(
                    basecurves$Nutrient == nut &
                      basecurves$VegType == a &
                      basecurves$HSG == .x &
                      basecurves$Slope == .y
                  ),
                  nut = nut,
                  a = a
                )
              ]
            ), collapse = "', '"
          ),
          "')"
        )
      }
    )
  ) %>%
select(comid, Nutrient, VegType, bufferwidth_ft, Efficiency_uncertainty) %>%
  pivot_wider(
    id_cols = c(comid, Nutrient, VegType, bufferwidth_ft),
    names_from = c(Nutrient, VegType, bufferwidth_ft),
    values_from = Efficiency_uncertainty
  ) %>%
  rename_with(.fn = ~paste0(., "ft_uncertainty"), .cols = -comid)

riparianefficiencies_bycomid <- merge(
  riparianefficiencies_bycomid_vals, 
  riparianefficiencies_bycomid_dist,
  by = 'comid',
  all = T
)

# 20. Write Files ####

write.csv(
  riparianloadings,
  file = "./RBEROST-Pacific/Preprocessing/Inputs/RiparianLoadings.csv" #*#
)

write.csv(
  bufferedlengths, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/LengthinBuffer_2016.csv" #*#
)

write.csv(
  riparianefficiencies_bycomid, 
  file = 
    "./RBEROST-Pacific/Preprocessing/Inputs/RiparianEfficiencies.csv"
)

