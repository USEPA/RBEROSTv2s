###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Calculate Urban Costs & Efficiency for each COMID ####
# BY: Cathy Chamberlin, Alyssa Le ####
# DATE:  6/23/2023 #### to update
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
packages <- c(
  "sf", "sp", "dplyr", "data.table", "rgdal", "ggplot2", "purrr", "openxlsx", "stringr", "tidyr"
)
lapply(packages, library, character.only = TRUE)

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

sf::sf_use_s2(FALSE)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

catchments_upd <- read_sf("./Data/Catchment_alb_1711.shp")

RO2d_in_temp <- fread("./Data/PS_1983to2018RO2d_in.csv")
RO2d_in_missing <- fread("./Data/PS_1983to2018RO2d_in_missing.csv")
RO2d_in <- rbind(RO2d_in_temp, RO2d_in_missing) %>%
  distinct(FEATUREID, .keep_all=T)

Anav_in_temp <- fread("./Data/PSppt_1983to2018anav.csv")
Anav_in_missing <- fread("./Data/PSppt_1983to2018anav_missing.csv")
Anav_in <- rbind(Anav_in_temp, Anav_in_missing) %>%
  distinct(FEATUREID, .keep_all=T)

missing_precip <- read.xlsx("./Data/allHRUs_missingprecip.xlsx") %>%
  select(COMID, NearCOMID) %>%
  mutate(COMID = as.integer(COMID),
         NearCOMID = as.integer(NearCOMID))

#*# Note: precip datasets are missing one large catchment that mainly falls in Canada
missing <- catchments_upd %>%
  filter(!FEATUREID %in% Anav_in$FEATUREID)

NLCD_bycomid_byHSG_temp <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG.txt") %>%
  select(ID, contains(c("21", "22", "23", "24")))
#*# Note: filled in the missing data with a variety of input files
NLCD_bycomid_byHSG_missing <- fread("./Data/PugetSound_NLCDAreabycomidbyHSG_fillmissing.txt") %>%
  select(ID = IDCORR, contains(c("21", "22", "23", "24"))) %>%
  mutate(VALUE_23 = 0,
         VALUE_24 = 0)
MissingHSG_domHSG <- read.xlsx("./Data/Missinghsgwdomhsg.xlsx", ) %>%
  select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID))
MissingHSG_closeHSG <- read.xlsx("./Data/MissingHSG_071123.xlsx") %>%
  select(COMID, HSG) %>%
  mutate(COMID = as.integer(COMID)) %>%
  rbind(., MissingHSG_domHSG) %>%
  filter(!is.na(HSG)) %>%
  rename(HSG_upd = HSG)

NLCD_bycomid_byHSG <- rbind(NLCD_bycomid_byHSG_temp, NLCD_bycomid_byHSG_missing) %>%
  distinct(ID, .keep_all=T) %>%
  mutate(COMID = as.integer(sub("_.*", "", ID)),
         HSG_temp = sub(".*_", "", ID)) %>%
  select(-ID) %>%
  merge(., MissingHSG_closeHSG, by="COMID", all.x=T) %>%
  mutate(HSG = case_when(
    HSG_temp=="" & !is.na(HSG_upd) ~ HSG_upd,
    HSG_temp=="" & is.na(HSG_upd) ~ NA_character_,
    TRUE ~ HSG_temp)
    ) %>%
  select(-HSG_upd, -HSG_temp)
length(unique(NLCD_bycomid_byHSG$COMID)) # 12584

Cap_costs <- read.xlsx("./Data/Capcostparam_statswq_noint.xlsx", sheet="#LN00057", startRow=3) %>%
  select(BMP.Type, Land_Use, HSG = Hydgrp, mean_capcost, Annav_capcost_coeff = AnnavPrecipin, 
         ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
         bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

Om_costs <- read.xlsx("./Data/Omcostparam_statswq_noint.xlsx", sheet="#LN00060", startRow=3) %>%
  select(BMP.Type, Land_Use, HSG = Hydgrp, mean_omcost, Annav_omcost_coeff = AnnavPrecipin, 
         ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, 
         bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

#*# AL: Still need to update Nparam and Pparam to "noint" datasets as of 7/25/23
Nparam <- read.xlsx("./Data/Nparam_wqstats.xlsx", sheet="Nparam_wqstats", startRow=3) %>%
  select(BMP.Type, HSG = Hydgrp, mean_efficN, Intercept_Neff = Intercept, Annav_Neff_coeff = AnnavPrecipin, 
         ro2d_Neff_coeff = ro2d_in, Annav2_Neff_coeff = AnnavPrecipin2, ro2d2_Neff_coeff = ro2d_in2, 
         bestAIC_Neff = bestAIC, bestmodel_Neff = bestmodel, PrecOrder_Neff = PrecOrder, Ro2dOrder_Neff = Ro2dOrder)

Pparam <- read.xlsx("./Data/Pparam_wqstats.xlsx", sheet="Pparam_statswq", startRow=3) %>%
  select(BMP.Type, HSG = Hydgrp, mean_efficP, Intercept_Peff = Intercept, Annav_Peff_coeff = AnnavPrecipin, 
         ro2d_Peff_coeff = ro2d_in, Annav2_Peff_coeff = AnnavPrecipin2, ro2d2_Peff_coeff = ro2d_in2, 
         bestAIC_Peff = bestAIC, bestmodel_Peff = bestmodel, PrecOrder_Peff = PrecOrder, Ro2dOrder_Peff = Ro2dOrder)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Make matrix of comid x  HSG x Landcover types -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

relevantcomids <- catchments_upd %>%
  as_tibble() %>%
  select(comid = FEATUREID) %>%
  pull() %>%
  unique()

all_HRUs <- NLCD_bycomid_byHSG %>%
  filter(COMID %in% relevantcomids) %>%
  rename(URLD = VALUE_21,
         URMD = VALUE_22,
         URHD = VALUE_23,
         UIDU = VALUE_24
         ) %>%
  gather(., Land_Use, Area, URLD:UIDU) %>%
  filter(Area > 0)

all_HRUs_with_precip <- all_HRUs %>%
  merge(., RO2d_in %>% rename(ro2d_in_temp = ro2d_in), by.x="COMID", by.y="FEATUREID", all.x=T) %>%
  merge(., Anav_in %>% rename(AnAvPpt_in_temp = AnAvPpt_in), by.x="COMID", by.y="FEATUREID", all.x=T) %>%
  merge(., missing_precip, by="COMID", all.x=T) %>%
  merge(., RO2d_in %>% rename(ro2d_in_miss = ro2d_in), by.x="NearCOMID", by.y="FEATUREID", all.x=T) %>%
  merge(., Anav_in %>% rename(AnAvPpt_in_miss = AnAvPpt_in), by.x="NearCOMID", by.y="FEATUREID", all.x=T) %>%
  mutate(ro2d_in = ifelse(is.na(ro2d_in_temp), ro2d_in_miss, ro2d_in_temp),
         AnAvPpt_in = ifelse(is.na(AnAvPpt_in_temp), AnAvPpt_in_miss, AnAvPpt_in_temp)) %>%
  select(COMID:Area, ro2d_in, AnAvPpt_in)

# QA - how many comids do not have precip data? None.
all_HRUs_precip_QA <- all_HRUs_with_precip %>%
  filter(is.na(ro2d_in))

write.csv(all_HRUs_with_precip, "C:/Users/50526/ICF/Optimization Tools - General/RBEROST/Puget_Sound/RBEROST_Updates/Outputs/allHRUs_precipinfo.csv")

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Make matrix of costs and efficiencies -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

Cost_matrix <- merge(Cap_costs, Om_costs, by=c("BMP.Type", "Land_Use", "HSG"), all.x=T)

Effic_matrix <- merge(Nparam, Pparam, by=c("BMP.Type", "HSG"), all.x=T)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 5. Calculate costs and efficiencies by BMP type, land use, hydro group, and COMID ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

all_HRUs_withcosts <- merge(all_HRUs_with_precip_temp, Cost_matrix, by=c("Land_Use", "HSG"), all.x=T)

all_HRUs_witheffic <- merge(all_HRUs_withcosts, Effic_matrix, by=c("HSG", "BMP.Type"), all.x=T)

#*# Still need to update the commented out case_when statements, use correct column names and correct identifiers (numbers or character)
#*# Getting capital costs that are less than zero, working with EPA to resolve these.
final_allHRUs <- all_HRUs_witheffic %>%
  mutate(
    capital_2020USD = case_when(
    bestAIC_capcost == "." | AIC1 == "." | AIC2 == "." | AIC3 == "." | AIC4 == "." ~ mean_capcost,
    PrecOrder_capcost==0 & Ro2dOrder_capcost==0 ~ mean_capcost,
    bestmodel_capcost==1 ~ Annav_capcost_coeff * AnAvPpt_in,
    bestmodel_capcost==2 ~ ro2d_capcost_coeff * ro2d_in,
    bestmodel_capcost==3 ~ Annav2_capcost_coeff * (AnAvPpt_in)^2 + Annav_capcost_coeff * AnAvPpt_in,
    bestmodel_capcost==4 ~ ro2d2_capcost_coeff * (ro2d_in)^2 + ro2d_capcost_coeff * ro2d_in,
    TRUE ~ mean_capcost)
    ) %>%
  select(HSG:AnAvPpt_in, contains("capcost"), capital_2020USD) %>%
  filter(capital_2020USD < 0)

write.csv(final_allHRUs, "C:/Users/50526/ICF/Optimization Tools - General/RBEROST/Puget_Sound/RBEROST_Updates/Outputs/capcost_QA.csv")

# ,
#     
#     operations_2020USD = case_when(
#       PrecOrder_omcost==0 & Ro2dOrder_omcost==0 ~ mean_omcost,
#       bestAIC_omcost==1 ~ Annav_omcost_coeff * AnAvPpt_in + Intercept_omcost,
#       bestAIC_omcost==2 ~ ro2d_omcost_coeff * ro2d_in + Intercept_omcost,
#       bestAIC_omcost==3 ~ Annav2_omcost_coeff * (AnAvPpt_in)^2 + Annav_omcost_coeff * AnAvPpt_in + Intercept_omcost,
#       bestAIC_omcost==4 ~ ro2d2_omcost_coeff * (ro2d_in)^2 + ro2d_omcost_coeff * ro2d_in + Intercept_omcost,
#       TRUE ~ mean_omcost),
# 
#     effic_N = case_when(
#       PrecOrder_Neff==0 & Ro2dOrder_Neff==0 ~ mean_efficN,
#       bestAIC_Neff==1 ~ Annav_Neff_coeff * AnAvPpt_in + Intercept_Neff,
#       bestAIC_Neff==2 ~ ro2d_Neff_coeff * ro2d_in + Intercept_Neff,
#       bestAIC_Neff==3 ~ Annav2_Neff_coeff * (AnAvPpt_in)^2 + Annav_Neff_coeff * AnAvPpt_in + Intercept_Neff,
#       bestAIC_Neff==4 ~ ro2d2_Neff_coeff * (ro2d_in)^2 + ro2d_Neff_coeff * ro2d_in + Intercept_Neff,
#       .default = mean_efficN),
# 
#     effic_P = case_when(
#       PrecOrder_Peff==0 & Ro2dOrder_Peff==0 ~ mean_efficP,
#       bestAIC_Peff==1 ~ Annav_Peff_coeff * AnAvPpt_in + Intercept_Peff,
#       bestAIC_Peff==2 ~ ro2d_Peff_coeff * ro2d_in + Intercept_Peff,
#       bestAIC_Peff==3 ~ Annav2_Peff_coeff * (AnAvPpt_in)^2 + Annav_Peff_coeff * AnAvPpt_in + Intercept_Peff,
#       bestAIC_Peff==4 ~ ro2d2_Peff_coeff * (ro2d_in)^2 + ro2d_Peff_coeff * ro2d_in + Intercept_Peff,
#       .default = mean_efficP)
#     )
  
#*# Haven't updated the programming beyond here.
DevelopedLand_bycomidbyHSG <- as.data.frame(t(DevelopedLand_bycomidbyHSG_wide)) %>% 
  slice(-2:-1) %>% 
  rename_with(.cols = everything(), .fn = ~ DevelopedLand_bycomidbyHSG_wide$OBJECTID) %>%
  rownames_to_column(var = "id") %>%
  mutate(
    comid = str_extract(id, "(?<=ID_)[[:digit:]]*(?=_)"),
    HSG = str_extract(id, "(?<=ID_[[:digit:]]{0,20}_)[[:upper:]]*$")
  ) %>%
  filter(HSG %in% c("A", "B", "C", "D")) %>%
  select(comid, HSG, all_of(LULC_SWAT)) %>%
  pivot_longer(
    cols = all_of(LULC_SWAT), names_to = "LULC_SWAT", values_to = "count"
  ) %>%
  complete(comid, HSG, LULC_SWAT) %>%
  mutate(comid = as.character(comid), count = as.numeric(count)) %>%
  mutate(count = replace_na(count, 0)) # This replaces values that are missing because there were no occurrences of a certain type of LULC and HSG in a given comid
# The fact that these values are missing, means they were not identified in GIS work, and their count should be zero.

HRUs_data_assigned_counted <- HRUs_data_assigned %>%
  mutate(comid = as.character(comid)) %>%
  full_join(., DevelopedLand_bycomidbyHSG, by = c("comid", "HSG", "LULC_SWAT")) %>%
  # one comid from DevelopedLand_bycomidbyHSG is not in HRUs_data_assigned, because the mean precip value was not extracted above. 
  # 168 comids from HRUs_data_assigned are not in DevelopedLand_bycomidbyHSG, either because there was no developed land in these comids, or because GIS work didn't cover it for some reason.
  group_by(comid, BMP) %>%
  mutate(
    totalcount = sum(count), 
    pctofdevland = case_when(
      totalcount == 0 ~ 0,
      totalcount >0 ~ count / totalcount
    )
  ) # Here, where totalcount is NA, that means we do not have data for this comid.
# There are 34944 / (13 * 4 * 4) = 168 such comids

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 6. Make matrix of costs and efficiencies ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

weighteddat_bycomid <- HRUs_data_assigned_counted %>%
  group_by(comid, BMP) %>%
  mutate(
    across(
      .cols = c(capital_2020USDacre, operations_2020USDacreyr, effic_N, effic_P),
      .fns = ~(. * pctofdevland),
      .names = "{.col}_weighted"
    ),
    pctnotD = case_when(
      mean(totalcount) == 0 ~ 0, 
      mean(totalcount) > 0 ~ 1 - sum(count[which(HSG == "D")])/totalcount
    )
  ) %>%
  summarize(
    across(.cols = contains("_weighted"), .fns = ~ sum(.)),
    pctnotD = unique(pctnotD),
    totalcount = unique(totalcount),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      .cols = contains("effic"),
      .fns = ~ case_when(totalcount == 0 ~ -999, totalcount > 0 ~ .) 
      # For areas that have no identified urban land, setting the efficiency as -999 prevents the optimization from implementing these BMPs in these comids. 
      # It may be the case that the SPARROW model inputs identified urban land in these catchments, and so there is a mismatch. 
      # Even if both the costs and efficiencies are 0, RBEROST may still implement the BMPs, because "0" often actually has some miniscule value with AMPL.
    )
  ) %>%
  drop_na() # There are 2184 entries that are NA. That is 168 comids x 13 BMPs. These are not written to disk.

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Assign cost and efficiency data ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

AvailablePrecips <- RepresentativeHUC12s %>%
  group_by(Hydgrp, Land_Use) %>%
  summarize(RepresentativePrecips_in = list(unique(AnnavPrecip_in)))

all_HRUs_assigned <- all_HRUs_withmapp %>%
  mutate(
    availprecips = map2(
      .x = HSG, 
      .y = LULC_SWAT, 
      .f = ~unlist(
        t(
          AvailablePrecips[
            which(AvailablePrecips$Hydgrp == .x & AvailablePrecips$Land_Use == .y), 
            "RepresentativePrecips_in"
          ]
        )
      )
      
    ),
    matching_meanannprecip_inyr = unlist(map2(
      .x = meanprecip_inyr, 
      .y = availprecips, 
      .f = ~custom.round(.x, unlist(.y))
    )
    )
  ) %>%
  select(-availprecips)

HRUs_data_assigned <- all_HRUs_assigned %>%
  crossing(., BMP = BMPs) %>%
  left_join(
    ., 
    RepresentativeHUC12s %>% 
      rename(
        HSG = Hydgrp, LULC_SWAT = Land_Use, 
        matching_meanannprecip_inyr = AnnavPrecip_in
      ),
    by = c("HSG", "LULC_SWAT", "matching_meanannprecip_inyr", "BMP")
  ) 

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 7. Write Data ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

write.csv(
  weighteddat_bycomid,
  file = "./RBEROST-Pacific/Preprocessing/Inputs/WeightedStormwaterBMPData.csv",
  row.names = FALSE
)
