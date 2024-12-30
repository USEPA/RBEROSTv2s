###########################################################################################
# PURPOSE: Develop Puget Sound WWTP data input files ####
# BY: Cathy Chamberlin ####
# DATE:  8/19/2021 ####
###########################################################################################

#################################################
# 1. Setup ####
##################################################
packages <- c(
  'tidyverse', 'sf', 'maps', 'sp', 'data.table', 'foreach', 'doParallel', 
  'nhdplusTools', 'purrr'
)

lapply(packages, library, character.only = T)

theme_set(theme_classic())

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")
##################################################
# 2. Load Data ####
##################################################

WWTPlist_PS_TT <- read.csv("./Data/PS_WWTPdata_list.csv")

states <- st_as_sf(
  maps::map(database = "state", plot = TRUE, fill = TRUE, col = "white")
)
states_subset <- states %>% 
  filter(ID %in% c("washington"))

ProposedTargets <- read.csv("./Data/ListofPugetSoundTargets.csv")

sparrow_inputs <- fread(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_input.txt",
  data.table = FALSE
)

data.dir <- paste0(
  "C:/Users/cchamber/Environmental Protection Agency (EPA)/", 
  "Puget Sound Regional Optimization - General/Salish Sea Model GIS data"
)
list.files(data.dir)
Watersheds <- st_read(
  paste(data.dir, "SalishSeaModel_Watersheds.dbf", sep = "/")
)
class(Watersheds)
str(Watersheds)

dat.effic <- read_csv("./Data/WWTPEffluentData.csv")
dat.cost <- read_csv("./Data/WWTPCostData.csv")
###################################################
# 3. Define functions ####
###################################################

use_get_upstream_ComIDs <- function(outofnetworkflags, pore_pt, sparrow_in) {
  if(
    !is.na(outofnetworkflags) & outofnetworkflags == "X"
  ) {
    
    upstream.comids_tmp_tp <- c(pore_pt)
    
  } else {
    
    upstream.comids_tmp_tp <- get_upstream_ComIDs(
      sparrow_in = sparrow_in, target.comid = pore_pt
    )
    
  }
  upstream.comids_tmp_tp
  
}

cleanup_intersections <- function(x) {
  if(length(x) == 0) {tmp <- NA} else {tmp <- x[[1]]}
  return(tmp)
}
####################################################
# 4. Get COMIDs & Watersheds of Loading Targets ####
####################################################

UserSpecs_loadingtargets_PStemplate <- expand_grid(
  ComID = ProposedTargets %>% filter(!is.na(COMID)) %>% .$COMID %>% unique(.),
  TN_or_TP = c("TN", "TP")
) %>%
  left_join(., sparrow_inputs %>% rename(ComID = comid), by = "ComID") %>%
  select(
    ComID,
    TN_or_TP,
    Waterbody_Name = pname, 
    Watershed_HUC = huc12
  ) %>%
  mutate(    
    Percent_Reduction = 0.1, 
    OutofNetworkFlag_X = "", 
    TermFlag_X = "X"
  ) %>%
  mutate(
    Waterbody_Name = case_when(
      Waterbody_Name == "" ~ paste("Unnamed Target", ComID), 
      Waterbody_Name != "" ~ paste(Waterbody_Name, ComID)
    )
  ) %>%
  select(
    Waterbody_Name, 
    ComID,
    Watershed_HUC,
    Percent_Reduction, 
    TN_or_TP,
    OutofNetworkFlag_X,
    TermFlag_X
  )

# write.csv(
#   UserSpecs_loadingtargets_PStemplate, 
#   file = "./Preprocessing/Inputs/01_UserSpecs_loadingtargets_PStemplate.csv"
#   )

# Specify comids of loading targets
pore_pt_tn <- with(
  UserSpecs_loadingtargets_PStemplate %>% filter(TN_or_TP == "TN"),
  as.character(ComID)
) #*#

pore_pt_tp <- with(
  UserSpecs_loadingtargets_PStemplate %>% filter(TN_or_TP == "TP"),
  as.character(ComID)
) #*#

outofnetworkflags_tn <- with(
  UserSpecs_loadingtargets_PStemplate %>% filter(TN_or_TP == "TN"), 
  OutofNetworkFlag_X
)

outofnetworkflags_tp <- with(
  UserSpecs_loadingtargets_PStemplate %>% filter(TN_or_TP == "TP"), 
  OutofNetworkFlag_X
)

# Get list of upstream.comids
# get_upstream_ComIDs source code is in Optimization_HelperFunctions.R

# upstream.comids_tn <- if(length(outofnetworkflags_tn) > 0) {
#   mapply(
#     use_get_upstream_ComIDs, 
#     outofnetworkflags = outofnetworkflags_tn, 
#     pore_pt = pore_pt_tn, 
#     MoreArgs = list(sparrow_in = sparrow_inputs)
#   )
# }
# 
# upstream.comids_tp <- if(length(outofnetworkflags_tp) > 0) {
#   mapply(
#     use_get_upstream_ComIDs, 
#     outofnetworkflags = outofnetworkflags_tp, 
#     pore_pt = pore_pt_tp, 
#     MoreArgs = list(sparrow_in = sparrow_inputs)
#   )
# }
# 
# comids_all <- unique(
#   c(
#     unlist(upstream.comids_tn, use.names = FALSE), 
#     unlist(upstream.comids_tp, use.names = FALSE)
#   )
# )

##################################################
#### 4. Get NHDPlus v2 data ####
##################################################

# nhdplus <- subset_nhdplus(
#   comids = comids_all,
#   output_file = "./Data/PugetSoundNHDplusV2.gpkg",
#   nhdplus_data = paste0(
#     "C:/sustain/NHDplusV2/NHDPlusNationalData/", 
#     "NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
#   ),
#   overwrite = TRUE,
#   return_data = FALSE,
#   status = TRUE,
#   flowline_only = FALSE
# )


st_layers("./Data/PugetSoundwatershedNHDplusV2.gpkg")

flowline <- read_sf(
  "./Data/PugetSoundwatershedNHDplusV2.gpkg", "NHDFlowline_Network"
)
catchments <- read_sf(
  "./Data/PugetSoundwatershedNHDplusV2.gpkg", "CatchmentSP"
)



###################################################
# 5. Get COMIDs of WWTPs in the River Networks ####
###################################################

WWTPlist_riveroutfalls <- WWTPlist_PS_TT %>%
  filter(RiverNetworkOutfall_YorN == "Y", between(as.numeric(WRIA), 1, 19)) %>%
  select(TYPE, Facility, Permit, Latitude, Longitude, Size = Correction) %>%
  mutate(
    Latitude = Latitude,
    Longitude = 
      case_when( 
        grepl("-", Longitude) ~ Longitude,
        !grepl("-", Longitude) ~ paste0("-", Longitude)
      )
  ) %>%
  st_as_sf(
    ., 
    coords = c("Longitude", "Latitude"),
    crs = 4326, 
    agr = c("constant", "identity", "identity", "constant")
  )
# Check projections & bounding box
st_crs(states_subset)
st_crs(WWTPlist_riveroutfalls)
st_crs(catchments)
st_crs(Watersheds)
st_crs(states_subset) == st_crs(WWTPlist_riveroutfalls)
st_crs(states_subset) == st_crs(catchments)
st_crs(states_subset) == st_crs(Watersheds)

catchments <- catchments %>% 
  st_transform(st_crs(WWTPlist_riveroutfalls))
Watersheds <- Watersheds %>% 
  st_transform(st_crs(WWTPlist_riveroutfalls))

st_crs(states_subset)
st_crs(WWTPlist_riveroutfalls)
st_crs(catchments)
st_crs(Watersheds)
st_crs(states_subset) == st_crs(WWTPlist_riveroutfalls)
st_crs(states_subset) == st_crs(catchments)
st_crs(states_subset) == st_crs(Watersheds)

sp_bbox <- bbox(as_Spatial(WWTPlist_riveroutfalls))

sp_bbox[,1] <- sp_bbox[,1] - 0.1 # Expand the bounding box slightly
sp_bbox[,2] <- sp_bbox[,2] + 0.2 # Expand the bounding box slightly

# View WWTP sites

ggplot() +
  geom_sf(data = Watersheds, color = "black", fill = "white", lwd = 1) +
  geom_sf(data = catchments, color = "grey50") + 
  geom_sf(data = WWTPlist_riveroutfalls, color = "firebrick4", size = 1) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) +
  ggtitle("WWTPs")

## Get comids
intersections <- st_intersects(WWTPlist_riveroutfalls, catchments)

WWTPcomids <- catchments$FEATUREID[
  mapply(cleanup_intersections, x = intersections)
]

## View WWTP sites w/ comids

ggplot() +
  geom_sf(data = Watersheds, color = "black", fill = "white", lwd = 1) +
  geom_sf(
    data = catchments %>% 
      mutate(
        containsWWTP = case_when(
          FEATUREID %in% WWTPcomids ~ TRUE, TRUE ~ FALSE
        )
      ), 
    mapping = aes(color = containsWWTP, fill = containsWWTP)
  ) + 
  geom_sf(data = WWTPlist_riveroutfalls, color = "firebrick4", size = 2) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) +
  scale_color_manual(values = c("white", "seagreen")) + 
  scale_fill_manual(values = c("white", "seagreen")) + 
  ggtitle("WWTPs")

## Format data for preprocessor

WWTP_COMIDs_PS <- WWTPlist_riveroutfalls %>%
  mutate(COMID = WWTPcomids, State = "WA") %>%
  as_tibble(.) %>%
  select(State, Plant_Name = Facility, NPDES_ID = Permit, COMID) %>%
  filter(!is.na(COMID)) %>%
  group_by(COMID) %>%
  mutate(
    Plant_Name = paste0(Plant_Name, collapse = "_"), 
    NPDES_ID = paste0(NPDES_ID, collapse = "_")
  ) %>%
  filter(!duplicated(Plant_Name))

#####################
# 6. Get Efficiency Data ####
#####################

WWTP_BMPs <- c(
  "MLE", 
  "FourBDP_M",
  "C", 
  "C_F",
  "MLE_C", 
  "FourBDP_M_C_F",
  "MLE_MBR", 
  "FourBMP_MBR_M", 
  "MLE_MBR_C", 
  "FourBMP_MBR_M_C",
  "SBR",
  "SBR_DNF_M", 
  "SBR_C", 
  "SBR_C_F", 
  "SBR_DNF_C_F_M", 
  "OC", 
  "M",
  "C_M"
)

PlantTypes <- unique(WWTPlist_riveroutfalls$TYPE)

all_effics <- dat.effic %>%
  group_by(PlantType, DesignSpec, Nutrient) %>%
  mutate(
    Efficiency = (
      EffluentValue_mgL[which(BMP == "base_effluent")] - EffluentValue_mgL
    ) / EffluentValue_mgL[which(BMP == "base_effluent")]
  ) %>%
  group_by(PlantType, Nutrient, BMP) %>%
  summarize(
    Efficiency_val = mean(Efficiency[which(DesignSpec == "MMWW")]),
    Efficiency_se = sd(Efficiency) / 3
  ) %>%
  rename(Efficiency = Efficiency_val) %>%
  filter(!(BMP %in% c("base_effluent", "OC"))) %>%
  pivot_wider(
    id_cols = c(PlantType, Efficiency_se), 
    names_from = c(BMP, Nutrient), 
    values_from = c(Efficiency, Efficiency_se),
    names_glue = "{BMP}_{Nutrient}_{.value}"
  ) 

WWTP_efficiency <- WWTPlist_riveroutfalls %>%
  mutate(COMID = WWTPcomids, State = "WA", PlantType = TYPE) %>%
  as_tibble(.) %>%
  filter(!is.na(COMID)) %>%
  left_join(., all_effics, by = "PlantType") %>%
  select(COMID, contains(paste0(WWTP_BMPs, "_T"))) %>%
  group_by(COMID) %>%
  summarize(across(everything(), ~mean(.))) 

#######################
# 7. Get Cost Data ####
#######################

costplots <- ggplot(
  data = dat.cost, mapping = aes(x = Capacity_mgd, y = Cost_2010dollars)
) + 
  geom_point() +
  geom_line() +
  facet_wrap(c("PlantType", "CostType", "BMP"), scales = "free", ncol = 5)

ggsave(
  "./Data/WWTPcostplots.pdf", costplots, width = 8, height = 33, units = "in"
)

cost.functions <- dat.cost %>%
  group_by(PlantType, CostType, BMP) %>%
  filter(BMP != "OC") %>% # don't include OC. Saves space
  summarize(
    fit = list(lm(Cost_2010dollars ~ Capacity_mgd)), 
    r2 = summary(fit[[1]])$adj.r.squared
  )

WWTP_costs_dat <- WWTPlist_riveroutfalls %>%
  mutate(COMID = WWTPcomids, State = "WA", PlantType = TYPE) %>%
  as_tibble(.) %>%
  filter(!is.na(COMID)) %>%
  group_by(COMID) %>%
  mutate(
    Plant_Name = paste0(Facility, collapse = "_"), 
    NPDES_ID = paste0(Permit, collapse = "_"),
    Size = case_when(
      Plant_Name == "BUCKLEY STP_ENUMCLAW STP" ~ sum(Size),
      TRUE ~ Size[1]
    )
  ) %>%
  filter(!duplicated(Plant_Name)) %>%
  full_join(., cost.functions, by = "PlantType") %>%
  ungroup() %>%
  filter(!is.na(Plant_Name))

inflationcorrection2010to2021 <- 1.21 
# January 2010 to January 2021 in the CPI Inflation Claculator at 
# bls.gov/data/inflation_calculator.htm

WWTP_costs_pred <- WWTP_costs_dat %>%
  mutate(
    Cost_2010dollars_text =  c(
      mapply(
        FUN = predict.lm, 
        object = fit,
        newdata = split(
          WWTP_costs_dat %>% select(Capacity_mgd = Size),
          seq(NROW(WWTP_costs_dat$Size))
        ), 
        se.fit = TRUE
      )[1,]
    ),
    Cost_2010dollars_se_text =  c(
      mapply(
        FUN = predict.lm, 
        object = fit,
        newdata = split(
          WWTP_costs_dat %>% select(Capacity_mgd = Size),
          seq(NROW(WWTP_costs_dat$Size))
        ), 
        se.fit = TRUE
      )[2,]
    ), 
    Cost_2010dollars = parse_number(
      gsub(pattern = "`1`", replacement = "", x = paste(Cost_2010dollars_text))
    ),
    Cost_2010dollars_se = parse_number(paste(Cost_2010dollars_se_text)),
    Cost_2021dollars = Cost_2010dollars * inflationcorrection2010to2021,
    Cost_2021dollars_se = Cost_2010dollars_se * inflationcorrection2010to2021
  ) 

costplots_Puget <- ggplot(
  WWTP_costs_pred, aes(x = Cost_2021dollars, y = Cost_2021dollars_se)
) +
  geom_point() +
  facet_wrap(c("Plant_Name", "CostType"), scales = "free", ncol = 5)

ggsave(
  "./Data/WWTPcostplots_PS.pdf", 
  costplots_Puget, 
  width = 8, 
  height = 33, 
  units = "in"
)

WWTP_costs <- WWTP_costs_pred %>%
  pivot_wider(
    id_cols = c(COMID), 
    names_from = c(BMP, CostType), 
    values_from = c(Cost_2021dollars, Cost_2021dollars_se),
    names_glue = "{BMP}_{CostType}_{.value}"
  ) 

##################################
# 8. Get Point Source Loading ####
##################################
htf_years <- seq(from = 2012, to = 2021, by = 1)

htf_loadingdat <- purrr::map_dfr(
  .x = htf_years, 
  .f = ~read.csv(
    paste0("./Data/HypoxiaTaskForce/National_", .x, ".csv"), skip = 3
  )
) 

npdes_locations <- fread(
  "./Data/FinalDMR_MasterSites_Sciencebase.csv", data.table = FALSE
)

PS_bbox <- st_bbox(catchments)

PugetSound_loading_correctedlocations <- left_join(
  htf_loadingdat %>% rename(NPDES_ID = NPDES.Permit.Number), 
  npdes_locations, 
  by = "NPDES_ID"
) %>%
  filter(
    between(OUTFALL_LATITUDE, PS_bbox$ymin, PS_bbox$ymax), 
    between(OUTFALL_LONGITUDE, PS_bbox$xmin, PS_bbox$xmax)
  ) %>% # CRS of the location data not listed in their metadata, 
  # but it was spatially joined to NHD for the HUC12s, 
  # so assume it is the same as the nhd data
  st_as_sf(
    coords = c("OUTFALL_LONGITUDE", "OUTFALL_LATITUDE"), 
    crs = st_crs(catchments),
    agr = "constant"
  ) %>%
  st_join(., catchments, left = TRUE) %>%
  as_tibble() %>%
  select(
    State, NPDES_ID, PERM_FEATURE_ID, COMID = FEATUREID, Year, 
    Nutrient = Nutrient.Type, Load_lbyr = Total.Pounds..lb.yr., 
    AvgConc_mgL = Avg.Concentration, Flow_mgalyr = Total.Annual.Flow..MGal.yr. 
  ) %>%
  mutate(
    Load_lbyr = case_when(
      Flow_mgalyr == 0 ~ NA_real_, 
      AvgConc_mgL == 0 ~ NA_real_,
      # An average concentration of 0 mg/L or a flow of 0 is more likely an indication that 
      # there was no data. 
      # Having concentrations of 0 causes major problems for % change calculations.
      AvgConc_mgL != 0 & Flow_mgalyr != 0 ~ Load_lbyr
    )
  ) %>%
  left_join(
    ., 
    WWTPlist_riveroutfalls %>% 
      mutate(NPDES_ID = gsub("[[:upper:]]$", "", Permit)) %>%
      as_tibble() %>%
      select(NPDES_ID, Plant_Name = Facility),
    by = c("NPDES_ID")
  ) %>% # Keep the plant names only of the ones to upgrade.
  group_by(
    State, Plant_Name, NPDES_ID, PERM_FEATURE_ID, Year, Nutrient, Load_lbyr
  ) %>%
  summarize(COMID = COMID[1], .groups = "drop") %>% 
  # in a few of the Seattle CSO outfalls, outfall locations were on the border 
  # between 2 comids. Assign them to only one.
  group_by(State, NPDES_ID, Year, Nutrient, Load_lbyr) %>%
  mutate(numoutfalls = n(), correctedload = Load_lbyr / numoutfalls) %>%
  # assume the load is distributed equally among the outfalls, then sum by COMID 
  group_by(State, COMID, Year, Nutrient) %>%
  summarize(
    Plant_Name = paste0(unique(Plant_Name), collapse = "_"), 
    NPDES_ID = paste0(unique(NPDES_ID), collapse = "_"),
    load_kgyr = sum(correctedload) * 0.453592
  ) %>%
  filter(!is.na(COMID)) %>%
  mutate(
    Plant_Name = case_when(
      Plant_Name == "NA" ~ NA_character_, Plant_Name != "NA" ~ Plant_Name
    )
  )

# Note: The loading data from the Hypoxia Task Force is not the data that was 
# used in the SPARROW model.
# The 2012 data for the SPARROW model is from Skinner and Wise 2019
# However, we may assume that the relative *change* between 2012 and 2021 in the 
# Hypoxia Task Force may approximate the relative *change* that we might find if
# Skinner and Wise repeated their analysis for the year 2021. Similarly, the
# relative error associated with any changes over time might be the same between
# the two different estimates of point-source loading.
# A comparison with Skinner and Wise et al is given below:
SPARROW_2012_loadingdata <- read.csv(
  "./Data/load_summary_by_discharger_Skinner+Wise2019.csv"
)

WA_2012_dat <- PugetSound_loading_correctedlocations %>%
  filter(Year == "2012") %>%
  rename(npdes = NPDES_ID) %>%
  left_join(., SPARROW_2012_loadingdata, by = "npdes")

ggplot() +
  geom_point(
    data = WA_2012_dat %>% filter(Nutrient == "Nitrogen"), 
    mapping = aes(x = load_kgyr, y = kg_00600) 
  ) + 
  labs(
    x = "HTF estimated loading (kg/yr; aggregated to COMID)",
    y = "Skinner & Wise estimated loading (kg/yr)"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  ggtitle(
    paste0(
      "Point Source loading of Total Nitrogen to the Puget Sound in 2012"
    )
  ) +
  geom_smooth()


ggplot() +
  geom_point(
    data = WA_2012_dat %>% filter(Nutrient == "Phosphorus"), 
    mapping = aes(x = load_kgyr, y = kg_00665) 
  ) + 
  labs(
    x = "HTF estimated loading (kg/yr; aggregated to COMID)",
    y = "Skinner & Wise estimated loading (kg/yr)"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  ggtitle(
    paste0(
      "Point Source loading of Total Phosphorus to the Puget Sound in 2012"
    )
  ) +
  geom_smooth()


ggplot(data = PugetSound_loading_correctedlocations)+
  geom_line(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = as.character(COMID)
    ),
    show.legend = FALSE
  ) +
  geom_point(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = as.character(COMID)
    ),
    show.legend = FALSE
  ) +
  facet_wrap("Nutrient", scales = "free")

ggplot(
  data = PugetSound_loading_correctedlocations %>% filter(!is.na(Plant_Name))
)+
  geom_line(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = as.character(COMID)
    ),
    show.legend = FALSE
  ) +
  geom_point(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = as.character(COMID)
    ),
    show.legend = FALSE
  ) +
  facet_wrap("Nutrient", scales = "free") +
  ggtitle("Trends in WWTPs considered for upgrade")

baselineplots <- ggplot(data = PugetSound_loading_correctedlocations)+
  geom_line(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = COMID
    ),
    show.legend = FALSE
  ) +
  geom_point(
    mapping = aes(
      x = Year, y = load_kgyr, group = COMID, color = COMID
    ),
    show.legend = FALSE
  ) +
  facet_wrap(c("Nutrient", "COMID"), scales = "free", ncol = 5)

ggsave(
  "./Data/WWTPbaselineshifts.pdf", 
  baselineplots, 
  width = 8, 
  height = 33, 
  units = "in"
)

ggplot(PugetSound_loading_correctedlocations) + 
  geom_histogram(aes(x = load_kgyr)) + 
  facet_wrap("Nutrient", scales = "free") + 
  scale_x_continuous(
    trans = "pseudo_log", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 1000000)
  )

# When calculating the % change over time, the best comparison is 2012 
# (the year of the SPARROW study) to 2021 (the most recent year)
# If these years weren't available, I used the oldest and newest available
# In cases where the value was 0, I treated this as an NA.
# For load to be 0 kg/yr, either the plant had no flow/wasn't operation 
WWTP_baselineremoval <- PugetSound_loading_correctedlocations %>%
  pivot_wider(
    id_cols = c("State", "Plant_Name", "NPDES_ID", "COMID"), 
    values_from = load_kgyr,
    names_from = c("Year", "Nutrient"),
    names_glue = "{.value}_{Nutrient}_{Year}"
  ) %>%
  mutate(
    load_TN_2012orclosest = case_when(
      !is.na(load_kgyr_Nitrogen_2012) ~ load_kgyr_Nitrogen_2012,
      is.na(load_kgyr_Nitrogen_2012) ~ case_when(
        !is.na(load_kgyr_Nitrogen_2013) ~ load_kgyr_Nitrogen_2013,
        is.na(load_kgyr_Nitrogen_2013) ~ case_when(
          !is.na(load_kgyr_Nitrogen_2014) ~ load_kgyr_Nitrogen_2014,
          is.na(load_kgyr_Nitrogen_2014) ~ case_when(
            !is.na(load_kgyr_Nitrogen_2015) ~ load_kgyr_Nitrogen_2015,
            is.na(load_kgyr_Nitrogen_2015) ~ case_when(
              !is.na(load_kgyr_Nitrogen_2016) ~ load_kgyr_Nitrogen_2016,
              is.na(load_kgyr_Nitrogen_2016) ~ case_when(
                !is.na(load_kgyr_Nitrogen_2017) ~ load_kgyr_Nitrogen_2017,
                is.na(load_kgyr_Nitrogen_2017) ~ case_when(
                  !is.na(load_kgyr_Nitrogen_2018) ~ load_kgyr_Nitrogen_2018,
                  is.na(load_kgyr_Nitrogen_2018) ~ case_when(
                    !is.na(load_kgyr_Nitrogen_2019) ~ load_kgyr_Nitrogen_2019,
                    is.na(load_kgyr_Nitrogen_2019) ~ case_when(
                      !is.na(load_kgyr_Nitrogen_2020) ~ load_kgyr_Nitrogen_2020,
                      is.na(load_kgyr_Nitrogen_2020) ~ NA_real_
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    load_TN_2021orclosest = case_when(
      !is.na(load_kgyr_Nitrogen_2021) ~ load_kgyr_Nitrogen_2021,
      is.na(load_kgyr_Nitrogen_2021) ~ case_when(
        !is.na(load_kgyr_Nitrogen_2020) ~ load_kgyr_Nitrogen_2020,
        is.na(load_kgyr_Nitrogen_2020) ~ case_when(
          !is.na(load_kgyr_Nitrogen_2019) ~ load_kgyr_Nitrogen_2019,
          is.na(load_kgyr_Nitrogen_2019) ~ case_when(
            !is.na(load_kgyr_Nitrogen_2018) ~ load_kgyr_Nitrogen_2018,
            is.na(load_kgyr_Nitrogen_2018) ~ case_when(
              !is.na(load_kgyr_Nitrogen_2017) ~ load_kgyr_Nitrogen_2017,
              is.na(load_kgyr_Nitrogen_2017) ~ case_when(
                !is.na(load_kgyr_Nitrogen_2016) ~ load_kgyr_Nitrogen_2016,
                is.na(load_kgyr_Nitrogen_2016) ~ case_when(
                  !is.na(load_kgyr_Nitrogen_2015) ~ load_kgyr_Nitrogen_2015,
                  is.na(load_kgyr_Nitrogen_2015) ~ case_when(
                    !is.na(load_kgyr_Nitrogen_2014) ~ load_kgyr_Nitrogen_2014,
                    is.na(load_kgyr_Nitrogen_2014) ~ case_when(
                      !is.na(load_kgyr_Nitrogen_2013) ~ load_kgyr_Nitrogen_2013,
                      is.na(load_kgyr_Nitrogen_2013) ~ NA_real_
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Rem_2012oroldest_2021ornewest_load_ch_TN = 
      (load_TN_2012orclosest - load_TN_2021orclosest) / load_TN_2012orclosest,
    load_TP_2012orclosest = case_when(
      !is.na(load_kgyr_Phosphorus_2012) ~ load_kgyr_Phosphorus_2012,
      is.na(load_kgyr_Phosphorus_2012) ~ case_when(
        !is.na(load_kgyr_Phosphorus_2013) ~ load_kgyr_Phosphorus_2013,
        is.na(load_kgyr_Phosphorus_2013) ~ case_when(
          !is.na(load_kgyr_Phosphorus_2014) ~ load_kgyr_Phosphorus_2014,
          is.na(load_kgyr_Phosphorus_2014) ~ case_when(
            !is.na(load_kgyr_Phosphorus_2015) ~ load_kgyr_Phosphorus_2015,
            is.na(load_kgyr_Phosphorus_2015) ~ case_when(
              !is.na(load_kgyr_Phosphorus_2016) ~ load_kgyr_Phosphorus_2016,
              is.na(load_kgyr_Phosphorus_2016) ~ case_when(
                !is.na(load_kgyr_Phosphorus_2017) ~ load_kgyr_Phosphorus_2017,
                is.na(load_kgyr_Phosphorus_2017) ~ case_when(
                  !is.na(load_kgyr_Phosphorus_2018) ~ load_kgyr_Phosphorus_2018,
                  is.na(load_kgyr_Phosphorus_2018) ~ case_when(
                    !is.na(load_kgyr_Phosphorus_2019) ~ 
                      load_kgyr_Phosphorus_2019,
                    is.na(load_kgyr_Phosphorus_2019) ~ case_when(
                      !is.na(load_kgyr_Phosphorus_2020) ~ 
                        load_kgyr_Phosphorus_2020,
                      is.na(load_kgyr_Phosphorus_2020) ~ NA_real_
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    load_TP_2021orclosest = case_when(
      !is.na(load_kgyr_Phosphorus_2021) ~ load_kgyr_Phosphorus_2021,
      is.na(load_kgyr_Phosphorus_2021) ~ case_when(
        !is.na(load_kgyr_Phosphorus_2020) ~ load_kgyr_Phosphorus_2020,
        is.na(load_kgyr_Phosphorus_2020) ~ case_when(
          !is.na(load_kgyr_Phosphorus_2019) ~ load_kgyr_Phosphorus_2019,
          is.na(load_kgyr_Phosphorus_2019) ~ case_when(
            !is.na(load_kgyr_Phosphorus_2018) ~ load_kgyr_Phosphorus_2018,
            is.na(load_kgyr_Phosphorus_2018) ~ case_when(
              !is.na(load_kgyr_Phosphorus_2017) ~ load_kgyr_Phosphorus_2017,
              is.na(load_kgyr_Phosphorus_2017) ~ case_when(
                !is.na(load_kgyr_Phosphorus_2016) ~ load_kgyr_Phosphorus_2016,
                is.na(load_kgyr_Phosphorus_2016) ~ case_when(
                  !is.na(load_kgyr_Phosphorus_2015) ~ load_kgyr_Phosphorus_2015,
                  is.na(load_kgyr_Phosphorus_2015) ~ case_when(
                    !is.na(load_kgyr_Phosphorus_2014) ~ 
                      load_kgyr_Phosphorus_2014,
                    is.na(load_kgyr_Phosphorus_2014) ~ case_when(
                      !is.na(load_kgyr_Phosphorus_2013) ~ 
                        load_kgyr_Phosphorus_2013,
                      is.na(load_kgyr_Phosphorus_2013) ~ NA_real_
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Rem_2012oroldest_2021ornewest_load_ch_TP = 
      (load_TP_2012orclosest - load_TP_2021orclosest) / load_TP_2012orclosest
  ) %>%
  select(
    -load_TN_2012orclosest, -load_TP_2012orclosest, -load_TN_2021orclosest, 
    -load_TP_2021orclosest
  )

ggplot(WWTP_baselineremoval) + 
  geom_point(
    aes(
      x = Rem_2012oroldest_2021ornewest_load_ch_TN,
      y = Rem_2012oroldest_2021ornewest_load_ch_TP,
      color = is.na(Plant_Name)
    )
  ) + 
  geom_abline(slope = 1, intercept = 0, color = "grey90") +
  scale_color_manual(
    name = "WWTP being considered for upgrade?", 
    values = c("black", "red"),
    breaks = c(TRUE, FALSE),
    labels = c("No", "Yes")
  ) +
  scale_x_continuous(
    name = "Change in TN", 
    trans = "pseudo_log",
    breaks = c(1, 0, -1, -10, -100, -1000)
  ) +
  scale_y_continuous(
    name = "Change in TP", 
    trans = "pseudo_log",
    breaks = c(1, 0, -1, -10, -100, -1000)
  )

#####################
# 8. Export Data ####
#####################

write_csv(
  WWTP_COMIDs_PS, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/WWTP_COMIDs.csv"
)
write_csv(
  WWTP_efficiency,
  file = "./RBEROST-Pacific/Preprocessing/Inputs/WWTP_RemovalEffic.csv"
)
write_csv(
  WWTP_costs, file = "./RBEROST-Pacific/Preprocessing/Inputs/WWTP_Costs.csv"
)
write_csv(
  WWTP_baselineremoval, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/WWTP_BaselineRemoval.csv"
)
