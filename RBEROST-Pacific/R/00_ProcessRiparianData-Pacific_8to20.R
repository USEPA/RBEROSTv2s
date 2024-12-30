# 8. Setup -----

packages <- c('tidyverse', 'foreach', 'doParallel', 'sf', 'data.table', 'purrr')
# lapply(packages, install.packages) #*# # Run this code if packages are not installed
lapply(packages, library, character.only = T)

registerDoParallel()

# source helper functions
source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R") ##AK

# 9. Load Data -----

datafolder <- "./Data/" #*#

### Import Data ----- 
PS_flowlines <- read_sf(
  paste0(datafolder, "nhdplus_1711.gpkg"), "flowline"
)

PS_catchments <- read_sf(
  paste0(datafolder, "nhdplus_1711.gpkg"), "catchment"
) 

sparrow_tn_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tn.txt"
)
sparrow_tp_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tp.txt"
)

land_covers <- fread(paste0(datafolder,"landcovers_bycomid_tmp_new.csv")) %>% rename(GRIDCODE=COMID) #*#

grid_compare <- fread(paste0(datafolder,"grid_compare.csv")) #*#  THIS IS A TEMPORARY TABLE TO CROSSWALK GRIDID TO COMID FOR LANDCOVERS - UPDATE landcovers_bycomid_temp.csv TO INCLUDE COMIDS TO DELETE
###

ftperkm <- 3280.84

PS_flowlines <- read_sf(
  paste0(datafolder, "nhdplus_1711.gpkg"), "flowline"
) %>%
  mutate(lengthkm_se = sqrt(
    (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + # error in each direction for each point consists of a 90% tolerance of 0.02 in (to map scale). The digitized maps have a tolerance of 0.003 in (to map scale). Not knowing exactly how these tolerances interact, I will assume that the uncertainty is similar to if they were added together. The model is using the medium resolution NHD+ data, which has a resolution of 1:100,000. Inches are then converted to kilometers.
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 + 
      (sqrt(0.02^2 + 0.003^2) * 100000 / (12 * ftperkm)) ^ 2 )
  ) # error in stream length depends on error in the location of the start and end point, in the x and y directions, so 4 total components.

PS_catchments <- read_sf(
  paste0(datafolder, "nhdplus_1711.gpkg"), "catchment"
) #*#

sparrow_tn_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tn.txt"
)
sparrow_tp_out <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_output_tp.txt"
)

# Slope and NLCD data are read in below in parallel processing. Users can edit file paths tagged with #*#


# 10. Summarize landcover by buffer widths ####

buffer.options <- c(20, 40, 60, 80, 100, 300, 400)


catchment_crosswalk <-PS_catchments %>%
  select(id = gridcode, comid = featureid)
st_geometry(catchment_crosswalk) <- NULL
# Due to memory constraints the earlier Model Builder approach which
# used zonal histograms to characterize land-use in riparian zones has
# been updated to use Zonal Statistics as table instead. 

# Process output files from Tabulate Area in SAS and export as csv then import here


landcovers_bycomid_tmp <- land_covers %>%
  left_join(grid_compare, by = c("GRIDCODE")) %>%
  rename(COMID=FEATUREID) %>%
  select(-GRIDCODE) %>%
  filter(!is.na(COMID)) %>%
  mutate(landcover = case_when(
    landcover == "Developed, Open Space" ~ "Lowdev", 
    landcover == "Developed, Low Intensity" ~ "Meddev",
    landcover == "Developed, Medium Intensity/Developed, High Intensity" ~ 
      "Highdev",
    landcover == "Deciduous Forest/Evergreen Forest/Mixed Forest" ~
      "Forest",
    landcover == "Herbaceuous" ~ "Grass",
    landcover == "Hay/Pasture" ~ "Grazing",
    landcover == "Cultivated Crops" ~ "Cropland")
  )    

# Pivot the data to wide form for easier human readibility 

landcovers_bycomid <- landcovers_bycomid_tmp %>%
  complete(COMID, buffersize, landcover) %>%
  group_by(COMID, landcover) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID", 
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
    .cols = !COMID
  ) %>%
  mutate(cv_numpixels = 0.17) # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)

# 11. Choose slope category for each comid ####
# NOTE: ALL SLOPES (% X 100) ARE < 500 (SO IN 0-5% CATEGORY) IN THIS
# DATASET SO THIS SECTION CAN BE GREATLY SIMPLIFIED --- AK: Simplification could be a problem if other values are ever introduced. 

# This parallel process will load each slope data file from the data folder, format it as necessary, then combine all the slope data

buffer.options <- c(20, 40, 60, 80, 100, 300, 400)

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
        between(MEAN, 0, 500) ~ "0-5%",
        between(MEAN, 500, 1000) ~ "5-10%",
        between(MEAN, 1000, 1500) ~ "10-15%",
        MEAN/100 > 1500 ~ ">15%"
      ),
      buffersize = i
    ) %>%
    select(comid = featurd, value, buffersize)
  
  # %>%
  #   rbind(dat.munged)
}  

slopes_bycomid_dist_pre <- foreach(
  i = 1:5, .combine = "rbind"
) %do% {
  
  j <- i
  
  dat <- read.csv(
    paste0(datafolder, "slope_", buffer.options[j], "ft_bycomid-Pacific.txt") #*#
  )
  # NOTE: Slopes in original grid were in percent slope x 100 so need to divide by 100;
  dat.munged <- dat %>%
    
    rowwise() %>%
    mutate(resampledslopes = list(rnorm(n = 10, mean = MEAN, sd = STD))) %>%
    mutate(
      value = list(
        case_when(
          resampledslopes < 500 ~ "0-5%",
          between(resampledslopes, 500, 1000) ~ "5-10%",
          between(resampledslopes, 1000, 1500) ~ "10-15%",
          resampledslopes > 1500 ~ ">15%"
        )
      ),
      buffersize = paste0("buff_",i)
    ) %>%
    select(comid = featurd, value, buffersize)  
  
  # %>%
  #   rbind(dat.munged)
} 

slopes_bycomid_dist <- slopes_bycomid_dist_pre %>%
  complete(comid, buffersize) %>%
  group_by(comid) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "comid", names_from = "buffersize", values_from = "value"
  ) %>%
  rename_with(
    # .fn = ~paste0("meanslope_", as.numeric(.) * 20, "ft"),
    .fn = ~paste0("meanslope_", c(1:5) * 20, "ft"),
    .cols = all_of(c("buff_1", "buff_2", "buff_3", "buff_4", "buff_5"))
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
    rename(comid = featureid) %>%
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
  # rename(comid=COMID,
  #        lengthkm=LENGTHKM,
  #        reachcode=REACHCODE) %>%
  select(comid, lengthkm, lengthkm_se, reachcode) %>%
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

# NEED TO UPDATE FOR PUGET SOUND BASIN
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
  # filter(!is.na(COMID)) %>%
  select(comid = COMID, Lowdev_400ft, Meddev_300ft, Highdev_100ft, cv_numpixels) %>%
  mutate(
    contrib_lowres_m2 = as.numeric(Lowdev_400ft), 
    contrib_lowres_acres = contrib_lowres_m2 / m2peracre, 
    contrib_res_m2 = as.numeric(Meddev_300ft), 
    contrib_res_acres = contrib_res_m2 / m2peracre, 
    contrib_commerc_m2 = as.numeric(Highdev_100ft), 
    contrib_commerc_acres = contrib_commerc_m2 / m2peracre,
    contrib_lowres_acres_se = 
      as.numeric(Lowdev_400ft) * cv_numpixels/ m2peracre,
    contrib_res_acres_se = 
      as.numeric(Meddev_300ft) * cv_numpixels/ m2peracre,
    contrib_commerc_acres_se = 
      as.numeric(Highdev_100ft) * cv_numpixels/ m2peracre
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
# NEED TO FIX - ALL FOREST AND GRASS BUFFER AREAS CONVERTED TO ZEROS

# Use the landcover data and calculate total area in forest and herbaceous.
# Divide total area by buffer width to get the bank length
# AK FIX HERE

# bufferedlengths_tmp <- landcovers_bycomid %>%
#   pivot_longer(cols = ends_with("ft"),
#                names_to = "type",
#                values_to = "value") %>%
#   separate_wider_delim(type,delim="_", names=c("type", "buffer")) %>%
#   mutate(bufferwidth=as.numeric(gsub("ft","",buffer))) %>%
#   rename(comid=COMID) %>%
#   right_join(river.lengths, by = "comid") %>%
#   select(
#     comid,
#     type,
#     bufferwidth,
#     value,
#     totalbanklength_ft,
#     cv_numpixels,
#     totalbanklength_ft_se
#   ) %>%
#   mutate(
#     value = replace_na(value,0),
#     area_ft2 = value * sqftpersqm,
#     area_ft2_se = value * cv_numpixels,
#     bufferlength_ft = value / bufferwidth,
#     bufferlength_ft_se = my_propogateerror(
#       vals = list(
#         if(area_ft2 > 0) {
#           c(area_ft2, area_ft2_se)
#         } else {c(1,0)},
#         c(bufferwidth, 0)
#       ),
#       method = "div"
#     )) %>%
#   mutate(
#     Forest_bufferlength_ft_rev = case_when(
#       Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft,
#       Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft
#     ),
#     Forest_bufferlength_ft_rev_se = case_when(
#       Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft_se,
#       Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft_se
#     ),
#     Grass_bufferlength_ft_rev = case_when(
#       Grass_bufferlength_ft >
#         totalbanklength_ft - Forest_bufferlength_ft_rev ~
#         totalbanklength_ft - Forest_bufferlength_ft_rev,
#       Grass_bufferlength_ft <=
#         totalbanklength_ft - Forest_bufferlength_ft_rev ~
#         Grass_bufferlength_ft
#     ),
#     Grass_bufferlength_ft_rev_se = case_when(
#       Grass_bufferlength_ft >
#         totalbanklength_ft - Forest_bufferlength_ft_rev ~
#         my_propogateerror(
#           vals = list(
#             c(totalbanklength_ft, totalbanklength_ft_se),
#             c(Forest_bufferlength_ft_rev, Forest_bufferlength_ft_rev_se)
#           ),
#           method = "addsub"
#         ),
#       Grass_bufferlength_ft <=
#         totalbanklength_ft - Forest_bufferlength_ft_rev ~
#         Grass_bufferlength_ft_se
#     )
#   )



sqftpersqm <- 10.7639

bufferedlengths_tmp <- foreach(
  i = 1:5, .combine = "rbind", .packages = "tidyverse"
) %dopar% {
  
  bufwid <- buffer.options[i]
  
  landcovers_bycomid %>%
    select(comid = COMID, contains(paste(bufwid)), cv_numpixels) %>%
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
      Forest_area_m2 = Forest,
      Grass_area_m2 = Grass,
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
    id_cols = c(comid), 
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
    ) %>% filter (!is.na(A))
} 

# format the curves for writing to csv

riparianefficiencies_bycomid_vals <- riparianefficiencies_bycomid_tmp %>%
  select(
    comid, 
    bufferwidth_ft,
    HSG_N,
    HSG_P
    # ,
    # Slope = meanslope
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
  fill(HSG, .direction = "downup") %>%
  # mutate(Slope = replace_na(Slope, ">15%")) %>% # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal
  # mutate(Slope = ">15%") %>% # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal
  left_join(., basecurves, by = c("HSG", "VegType", "Nutrient")) %>% ##Included SLOPE
  rowwise() %>%
  mutate(
    Efficiency = gsub(
      "coef.3", coef.3, gsub(
        "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
      )
    )
  ) %>%
  select(comid, Nutrient, VegType, bufferwidth_ft, Efficiency) %>%
  mutate(coef = gsub( " .*$", "", Efficiency)) %>%
  complete(coef) %>%
  distinct()  %>% ## AK
  filter(coef != "") %>%
  pivot_wider(
    names_from = c(Nutrient, VegType, bufferwidth_ft, coef),
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
  mutate(Slope = replace_na(Slope, list(rep(">15%", 10))))  # for missing slope values, assume >15%, which is the least effective slope class for riparian buffer nutrient removal

riparianefficiencies_bycomid_dist_end <- riparianefficiencies_bycomid_dist %>%
  filter(HSG != "list()") %>%
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
    id_cols = c(comid), 
    names_from = c(Nutrient, VegType, bufferwidth_ft),
    values_from = Efficiency_uncertainty
  ) %>%
  rename_with(.fn = ~paste0(., "ft_uncertainty"), .cols = -comid)

riparianefficiencies_bycomid <- merge(
  riparianefficiencies_bycomid_vals, 
  riparianefficiencies_bycomid_dist_end,
  by = 'comid',
  all = T
)


# 20. Write Files ####
write.csv(
  riparianloadings,
  file = "./RBEROST-Pacific/Preprocessing/Inputs/RiparianLoadings_update.csv" #*#
)

write.csv(
  bufferedlengths, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/LengthinBuffer_2016_update.csv" #*#
)

write.csv(
  riparianefficiencies_bycomid, 
  file = 
    "./RBEROST-Pacific/Preprocessing/Inputs/RiparianEfficiencies_update.csv"
)

