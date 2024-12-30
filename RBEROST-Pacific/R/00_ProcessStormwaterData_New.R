###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Load in updated precipitation, run off, and land use data for each COMID as well as updated cost and efficiency data ####
# BY: Kelly-Anne Moffa
# DATE:  11/2/2023 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

packages <- c(
  "sf", "sp", "dplyr", "data.table", "ggplot2", "purrr", "openxlsx", "stringr", "tidyr", "foreign", "openxlsx"
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

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

InPath <- ("./RBEROST-Pacific/Preprocessing/Inputs/")

sf::sf_use_s2(FALSE)



###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~


### User defined list of BMPs -----
user_specs_BMPs <- fread(
  paste(InPath,"01_UserSpecs_BMPs.csv",sep=""), data.table = FALSE
) 

#Pull out urban BMPs and make into list to filter future datasets by
urban_BMP <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "urban")

relevant_BMP <- urban_BMP %>%
  as_tibble() %>%
  dplyr::select(BMP) %>%
  pull() %>%
  unique()

# Replace "rain_garden6" and "rain_garden10" with "rain_garden" if selected
if ("Rain_Garden6" %in% relevant_BMP || "Rain_Garden10" %in% relevant_BMP) {
  relevant_BMP <- c(relevant_BMP, "Rain_Garden")
  relevant_BMP <- setdiff(relevant_BMP, c("Rain_Garden6", "Rain_Garden10"))
}


### 2a. Load in updated catchment data to maintain area information -----

catchments_upd <- read_sf("./Data/Catchment_alb_1711.shp")
catchments_temp <- read.dbf("./Data/Catchment_centroid_1711wcounty_alb.dbf")

match <- catchments_temp %>% filter(FEATUREID %in% catchments_upd$FEATUREID)

catchments <- merge(catchments_temp, catchments_upd) %>% rename(county = CNTY_NAME)


### 2b. Read in land value data and map numeric land use identifiers to character identifiers -----

land_value_temp <- read.dbf("./Data/AvLandValueft2xCountyxNLCD16.dbf") %>% 
  dplyr::select(nlcd_value = RASTERVALU, county = CNTY_NAME, FREQUENCY, land_val_sqft = MEAN_LandV)

land_value_sqft <- land_value_temp %>% filter(nlcd_value %in% c("21", "22", "23", "24"))

land_use_mapping <- data.frame(
  NumericValue = c(21, 22, 23, 24),
  LandUseCode = c("URLD", "URMD", "URHD", "UIDU")
)

land_value <- land_value_sqft %>% mutate(nlcd_value = case_when(
  nlcd_value == land_use_mapping$NumericValue[1] ~ land_use_mapping$LandUseCode[1],
  nlcd_value == land_use_mapping$NumericValue[2] ~ land_use_mapping$LandUseCode[2],
  nlcd_value == land_use_mapping$NumericValue[3] ~ land_use_mapping$LandUseCode[3],
  nlcd_value == land_use_mapping$NumericValue[4] ~ land_use_mapping$LandUseCode[4],
  TRUE ~ as.character(nlcd_value)
))

land_value_acre <- land_value %>% mutate(land_val_acre = land_val_sqft/43560) %>% select(nlcd_value, county, land_val_acre)



### 2c. Read in run off and precipitation data -----

RO2d_in_temp <- fread("./Data/PS_1983to2018RO2d_in.csv")
RO2d_in_missing <- fread("./Data/PS_1983to2018RO2d_in_missing.csv")
RO2d_in <- rbind(RO2d_in_temp, RO2d_in_missing) %>%
  distinct(FEATUREID, .keep_all=T)

Anav_in_temp <- fread("./Data/PSppt_1983to2018anav.csv")
Anav_in_missing <- fread("./Data/PSppt_1983to2018anav_missing.csv")
Anav_in <- rbind(Anav_in_temp, Anav_in_missing) %>%
  distinct(FEATUREID, .keep_all=T)

missing_precip <- read.xlsx("./Data/allHRUs_missingprecip.xlsx") %>%
  dplyr::select(COMID, NearCOMID) %>%
  mutate(COMID = as.integer(COMID),
         NearCOMID = as.integer(NearCOMID))

#Identify missing catchment
missing <- catchments %>%
  filter(!FEATUREID %in% Anav_in$FEATUREID)



### 2d. Read in land use data to get land use by COMID and HSG -----

NLCD_bycomid_byHSG_temp <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG.txt") %>%
  dplyr::select(ID, contains(c("21", "22", "23", "24")))

#*# Note: filled in the missing data with a variety of input files
NLCD_bycomid_byHSG_missing <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG_fillmissing.txt") %>%
  dplyr::select(ID = IDCORR, contains(c("21", "22", "23", "24"))) %>%
  mutate(VALUE_23 = 0,
         VALUE_24 = 0)

MissingHSG_domHSG <- read.xlsx("./Data/Missinghsgwdomhsg.xlsx", ) %>%
  dplyr::select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID))
MissingHSG_closeHSG <- read.xlsx("./Data/MissingHSG_071123.xlsx") %>%
  dplyr::select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID)) %>%
  rbind(., MissingHSG_domHSG) %>%
  filter(!is.na(HSG)) %>%
  rename(HSG_upd = HSG)

NLCD_bycomid_byHSG <- rbind(NLCD_bycomid_byHSG_temp, NLCD_bycomid_byHSG_missing) %>%
  distinct(ID, .keep_all=T) %>%
  mutate(COMID = as.integer(sub("_.*", "", ID)),
         HSG_temp = sub(".*_", "", ID)) %>%
  dplyr::select(-ID) %>%
  merge(., MissingHSG_closeHSG, by="COMID", all.x=T) %>%
  mutate(HSG = case_when(
    HSG_temp=="" & !is.na(HSG_upd) ~ HSG_upd,
    HSG_temp=="" & is.na(HSG_upd) ~ NA_character_,
    TRUE ~ HSG_temp)
  ) %>%
  dplyr::select(-HSG_upd, -HSG_temp)


### 2e. Load in cost and efficiency data -----

# Read in costs (Cap, OM, min cap/OM, and rain garden)
cap_costs_temp <- read.xlsx("./Data/Capcost_finalparameters_092823_clean.xlsx", sheet="#LN16126", startRow=3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error, 
                Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2,
                ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4, bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

cap_costs_temp$BMP.Type <- gsub(" ", "_", cap_costs_temp$BMP.Type)
cap_costs_temp$BMP.Type <- gsub("[/]", "_", cap_costs_temp$BMP.Type)

cap_costs <- cap_costs_temp %>%
  filter(BMP.Type %in% relevant_BMP)

om_costs_temp <- read.xlsx("./Data/OMcost_finalparameters_092823_clean.xlsx", sheet="#LN16147", startRow=3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_omcost, min_omcost, max_omcost, om_rmse = Root.mean.squared.error,
                Annav_omcost_coeff = AnnavPrecipin, ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2,
                ro2d2_omcost_coeff = ro2d_in2, bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

om_costs_temp$BMP.Type <- gsub(" ", "_", om_costs_temp$BMP.Type)
om_costs_temp$BMP.Type <- gsub("[/]", "_", om_costs_temp$BMP.Type)

om_costs <- om_costs_temp %>%
  filter(BMP.Type %in% relevant_BMP)


min_capcost <- read.xlsx("./Data/Mincapcostparam_statswq.xlsx", sheet = "#LN16161", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_capcost, min_capcost, max_capcost, 
                Annav_capcost_coeff = AnnavPrecipin, min_cap_rmse = Root.mean.squared.error, 
                ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
                bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

min_capcost$BMP.Type <- gsub(" ", "_", min_capcost$BMP.Type)
min_capcost$BMP.Type <- gsub("[/]", "_", min_capcost$BMP.Type)

min_omcost <- read.xlsx("./Data/Minomcostparam_statswq.xlsx", sheet = "#LN16164", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Intercept, mean_omcost, min_omcost, max_omcost,
                min_om_rmse = Root.mean.squared.error, Annav_omcost_coeff = AnnavPrecipin, 
                ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, 
                bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

min_omcost$BMP.Type <- gsub(" ", "_", min_omcost$BMP.Type)
min_omcost$BMP.Type <- gsub("[/]", "_", min_omcost$BMP.Type)


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Make matrix of comid x  HSG x Landcover types -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

rel_comids <- catchments %>%
  as_tibble() %>%
  dplyr::select(comid = FEATUREID) %>%
  pull() %>%
  unique()

relevantcomids <- catchments %>%
  dplyr::select(comid = FEATUREID, county)

all_HRUs_temp <- merge(NLCD_bycomid_byHSG, relevantcomids, by.x = "COMID", by.y = "comid")

all_qa <- all_HRUs_temp %>% as_tibble() %>%
  dplyr::select(COMID) %>%
  pull() %>%
  unique()

missing_hru <- catchments %>%
  filter(!FEATUREID %in% all_qa)
#*# Identified 15 catchments that drop out of the data, however they are all over the Canadian border. 

all_HRUs_prelim <- all_HRUs_temp %>%
  rename(URLD = VALUE_21,
         URMD = VALUE_22,
         URHD = VALUE_23,
         UIDU = VALUE_24
  ) %>%
  gather(., Land_Use, Area, URLD:UIDU) %>%
  filter(Area > 0)

all_HRUs <- merge(all_HRUs_prelim, land_value_acre, by.x = c("county", "Land_Use"), by.y = c("county", "nlcd_value"))

all_HRUs_with_precip_temp <- all_HRUs %>%
  merge(., RO2d_in %>% rename(ro2d_in_temp = ro2d_in), by.x="COMID", by.y="FEATUREID", all.x=T) %>%
  merge(., Anav_in %>% rename(AnAvPpt_in_temp = AnAvPpt_in), by.x="COMID", by.y="FEATUREID", all.x=T) %>%
  merge(., missing_precip, by="COMID", all.x=T) %>%
  merge(., RO2d_in %>% rename(ro2d_in_miss = ro2d_in), by.x="NearCOMID", by.y="FEATUREID", all.x=T) %>%
  merge(., Anav_in %>% rename(AnAvPpt_in_miss = AnAvPpt_in), by.x="NearCOMID", by.y="FEATUREID", all.x=T) %>%
  mutate(ro2d_in = ifelse(is.na(ro2d_in_temp), ro2d_in_miss, ro2d_in_temp),
         AnAvPpt_in = ifelse(is.na(AnAvPpt_in_temp), AnAvPpt_in_miss, AnAvPpt_in_temp)) %>%
  dplyr::select(COMID:Area, land_val_acre, ro2d_in, AnAvPpt_in) 

length(unique(all_HRUs_with_precip_temp$COMID))

all_HRUs_with_precip <- all_HRUs_with_precip_temp %>%
  group_by(COMID, Land_Use, HSG) %>%
  summarize(
    AnAvPpt_in = mean(AnAvPpt_in, na.rm = TRUE),
    ro2d_in = mean(ro2d_in, na.rm = T),
    land_val_acre = mean(land_val_acre, na.rm = T),
    Area = sum(Area)
  )

length(unique(all_HRUs_with_precip$COMID))

# QA - how many comids do not have precip data? None.
all_HRUs_precip_QA <- all_HRUs_with_precip %>%
  filter(is.na(ro2d_in))

write.csv(all_HRUs_with_precip, "./Data/allHRUs_precipinfo_grouped.csv", row.names = F)




###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Make matrix of costs -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

Cost_matrix <- merge(cap_costs, om_costs, by=c("BMP.Type", "Land_Use", "HSG"), all.x=T)
min_cost_matrix <- merge(min_capcost, min_omcost, by=c("BMP.Type", "Land_Use", "HSG"), all.x=T)


all_HRUs_withcosts <- merge(all_HRUs_with_precip, Cost_matrix, by=c("Land_Use", "HSG"), all.x=T)

all_HRUs_with_mincosts <- merge(all_HRUs_with_precip, min_cost_matrix, by=c("Land_Use", "HSG"), all.x=T)
all_HRUs_with_mincosts <- all_HRUs_with_mincosts %>% rename_all(~paste0("min_",.)) 
all_HRUs_with_mincosts <- all_HRUs_with_mincosts %>%
  select(Land_Use = min_Land_Use, HSG = min_HSG, COMID = min_COMID, AnAvPpt_in = min_AnAvPpt_in, ro2d_in = min_ro2d_in, land_val_acre = min_land_val_acre,
         Area = min_Area, BMP.Type = min_BMP.Type, min_ModelLabel.x:min_Ro2dOrder_omcost)

convert_cols <- c(10:26, 28:40)
all_HRUs_withcosts[,convert_cols] <- apply(all_HRUs_withcosts[,convert_cols], 2, as.numeric)
all_HRUs_with_mincosts[,convert_cols] <- apply(all_HRUs_with_mincosts[,convert_cols], 2, as.numeric)

write.csv(all_HRUs_withcosts, paste0(InPath, "allHRUs_withcosts_standard.csv"), row.names = F)
write.csv(all_HRUs_with_mincosts, paste0(InPath, "allHRUs_withcosts_min.csv"), row.names = F)

#*# The last two .csvs printed will be used in the next R file ("00_CostandEffic_Calc.R") to actually calculate costs #*#