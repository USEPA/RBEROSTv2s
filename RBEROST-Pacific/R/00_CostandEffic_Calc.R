###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Calculate updated cost and efficiency data ####
# BY: Kelly-Anne Moffa
# DATE:  12/04/2023 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~



# 1. Setup -----

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

source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

InPath <- ("./RBEROST-Pacific/Preprocessing/Inputs/")

sf::sf_use_s2(FALSE)


# 2. Load Data -----


### Read in HRU files with cost coefficients -----
all_HRUs_withcosts <- read_csv(paste0(InPath, "allHRUs_withcosts_standard.csv"))

all_HRUs_min_withcosts <- read_csv(paste0(InPath, "allHRUs_withcosts_min.csv"))


### Read in efficiencies -----
np_effic <- read.xlsx("./Data/Bmpefficsumxlu.xlsx", startRow = 3)
wp_effic_temp <- read.xlsx("./Data/Bmpefficsumxluhsg.xlsx", startRow = 3)

#### Update BMP names to only have underscores, no spaces or special characters
np_effic$BMP.Type <- gsub(" ", "_", np_effic$BMP.Type)
np_effic$BMP.Type <- gsub("/", "_", np_effic$BMP.Type)

wp_effic_temp$BMP.Type <- gsub(" ", "_", wp_effic_temp$BMP.Type)
wp_effic_temp$BMP.Type <- gsub("/", "_", wp_effic_temp$BMP.Type)

wp_effic <- wp_effic_temp %>% filter(BMP.Type == "Wet_Pond") %>% rename(HSG = Hydgrp) %>% 
  mutate(mean_efficN = as.numeric(mean_efficN), mean_efficP = as.numeric(mean_efficP))


### Read in rain garden cost data -----
RG_6in_pond <- read.xlsx("./Data/Hrualllutn_bmp2_6cwq.xlsx", startRow = 3)
RG_10in_pond <- read.xlsx("./Data/Hrualllutn_bmp2_10cwq.xlsx", startRow = 3)


# 3. Combine Data Frames -----


combined_costs_temp <- merge(all_HRUs_withcosts, all_HRUs_min_withcosts, by = c("COMID", "BMP.Type", "Land_Use", "HSG"), all.x=T)

combined_costs_nowp <- combined_costs_temp %>% filter(!BMP.Type == "Wet_Pond")
wp <- combined_costs_temp %>% filter(BMP.Type == "Wet_Pond")

combined_effic_temp <- merge(combined_costs_nowp, np_effic, by = c("BMP.Type", "Land_Use"))

combined_effic_wp <- merge(wp, wp_effic, by = c("BMP.Type", "Land_Use", "HSG"))

combined_costs_effic <- rbind(combined_effic_temp, combined_effic_wp)

combined_costs_NA <- combined_costs_effic %>% rename(AnAvPpt_in = AnAvPpt_in.x, ro2d_in = ro2d_in.x, land_val_acre = land_val_acre.x,
                                                     Area = Area.x) %>% mutate(effic_N = case_when(BMP.Type == "Wet_Pond" ~ mean_efficN, 
                                                                                                   TRUE ~ as.numeric(median_efficN)),
                                                                               effic_P = case_when(BMP.Type == "Wet_Pond" ~ mean_efficP,
                                                                                                   TRUE ~ as.numeric(median_efficP))) %>% 
                                              select(BMP.Type:Ro2dOrder_omcost, min_ModelLabel.x:min_Ro2dOrder_omcost, effic_N, effic_P,
                                                     std_efficN, std_efficP, n_efficN, n_efficP)

### Removing instances of NAs for intercepts (no intercept is equivalent to 0 intercept) -----
combined_costs <- combined_costs_NA %>% mutate(
  Intercept.x = case_when(
    is.na(Intercept.x) ~ 0, 
    TRUE ~ Intercept.x),
  Intercept.y = case_when(
    is.na(Intercept.y) ~ 0,
    TRUE ~ Intercept.y),
  min_Intercept.x = case_when(
    is.na(min_Intercept.x) ~ 0, 
    TRUE ~ min_Intercept.x),
  min_Intercept.y = case_when(
    is.na(min_Intercept.y) ~ 0,
    TRUE ~ min_Intercept.y))

### Assign instances of Grass Swale BMP with Model 0 -----
combined_costs <- combined_costs %>% mutate(ModelLabel.y = case_when(
  BMP.Type == "Grass_Swale_w_D" ~ "MODEL0",
  TRUE ~ ModelLabel.y
))


# 4. Calculate Costs ------


allHRUs <- combined_costs %>%
  mutate(
    o_capital_2020USD = case_when(
      is.na(bestAIC_capcost) | is.na(AIC1) | is.na(AIC2) | is.na(AIC3) | is.na(AIC4) ~ mean_capcost,
      ModelLabel.x == "MODEL0" ~ Intercept.x,
      PrecOrder_capcost==0 & Ro2dOrder_capcost==0 ~ mean_capcost,
      ModelLabel.x == "MODEL1" ~ Intercept.x + (Annav_capcost_coeff * AnAvPpt_in),
      ModelLabel.x == "MODEL2" ~ Intercept.x + (ro2d_capcost_coeff * ro2d_in),
      ModelLabel.x == "MODEL3" ~ Intercept.x + (Annav_capcost_coeff * AnAvPpt_in) + (Annav2_capcost_coeff * (AnAvPpt_in)^2),
      ModelLabel.x == "MODEL4" ~ Intercept.x + (ro2d2_capcost_coeff * (ro2d_in)^2) + (ro2d_capcost_coeff * ro2d_in),
      TRUE ~ mean_capcost),
    m_capital_2020USD = case_when(
      is.na(min_bestAIC_capcost) | is.na(min_AIC1) | is.na(min_AIC2) | is.na(min_AIC3) | is.na(min_AIC4) ~ min_mean_capcost,
      min_ModelLabel.x == "MODEL0" ~ min_Intercept.x,
      min_PrecOrder_capcost==0 & min_Ro2dOrder_capcost==0 ~ min_mean_capcost,
      min_ModelLabel.x == "MODEL1" ~ min_Intercept.x + (min_Annav_capcost_coeff * AnAvPpt_in),
      min_ModelLabel.x == "MODEL2" ~ min_Intercept.x + (min_ro2d_capcost_coeff * ro2d_in),
      min_ModelLabel.x == "MODEL3" ~ min_Intercept.x + (min_Annav2_capcost_coeff * (AnAvPpt_in)^2 + min_Annav_capcost_coeff * AnAvPpt_in),
      min_ModelLabel.x == "MODEL4" ~ min_Intercept.x + (min_ro2d2_capcost_coeff * (ro2d_in)^2 + min_ro2d_capcost_coeff * ro2d_in),
      TRUE ~ min_mean_capcost),
    o_operations_2020USD = case_when(
      BMP.Type == "Grass_Swale_w_D" & ModelLabel.y == "MODEL0" ~ mean_omcost,
      ModelLabel.y == "MODEL0" ~ Intercept.y,
      PrecOrder_omcost==0 & Ro2dOrder_omcost==0 ~ mean_omcost,
      ModelLabel.y == "MODEL1" ~ Intercept.y + (Annav_omcost_coeff * AnAvPpt_in),
      ModelLabel.y == "MODEL2" ~ Intercept.y + (ro2d_omcost_coeff * ro2d_in),
      ModelLabel.y == "MODEL3" ~ Intercept.y + (Annav2_omcost_coeff * (AnAvPpt_in)^2) + (Annav_omcost_coeff * AnAvPpt_in),
      ModelLabel.y == "MODEL4" ~ Intercept.y + (ro2d2_omcost_coeff * (ro2d_in)^2) + (ro2d_omcost_coeff * ro2d_in),
      TRUE ~ mean_omcost),
    m_operations_2020USD = case_when(
      min_ModelLabel.y == "MODEL0" ~ min_Intercept.y,
      min_PrecOrder_omcost==0 & min_Ro2dOrder_omcost==0 ~ min_mean_omcost,
      min_ModelLabel.y == "MODEL1" ~ min_Intercept.y + (min_Annav_omcost_coeff * AnAvPpt_in),
      min_ModelLabel.y == "MODEL2" ~ min_Intercept.y + (min_ro2d_omcost_coeff * ro2d_in),
      min_ModelLabel.y == "MODEL3" ~ min_Intercept.y + (min_Annav2_omcost_coeff * (AnAvPpt_in)^2) + (min_Annav_omcost_coeff * AnAvPpt_in),
      min_ModelLabel.y == "MODEL4" ~ min_Intercept.y + (min_ro2d2_omcost_coeff * (ro2d_in)^2) + (min_ro2d_omcost_coeff * ro2d_in),
      TRUE ~ min_mean_omcost
    ))



### Decide whether to use standard costs or minimum costs and replace porous pavement w/UD with Type A soils with zero costs -----
allHRUs_costs <- allHRUs %>% 
  mutate(
    capital_2020USD = case_when(
      is.na(m_capital_2020USD) ~ o_capital_2020USD,
      o_capital_2020USD >= m_capital_2020USD ~ o_capital_2020USD,
      m_capital_2020USD > o_capital_2020USD ~ m_capital_2020USD,
    ),
    operations_2020USD = case_when(
      is.na(m_operations_2020USD) ~ o_operations_2020USD,
      o_operations_2020USD >= m_operations_2020USD ~ o_operations_2020USD,
      m_operations_2020USD > o_operations_2020USD ~ m_operations_2020USD
    ),
    cap_rmse = case_when(
      is.na(m_capital_2020USD) ~ cap_rmse,
      o_capital_2020USD >= m_capital_2020USD ~ cap_rmse,
      m_capital_2020USD > o_capital_2020USD ~ min_min_cap_rmse
    ),
    om_rmse = case_when(
      is.na(m_operations_2020USD) ~ om_rmse,
      o_operations_2020USD >= m_operations_2020USD ~ om_rmse,
      m_operations_2020USD > o_operations_2020USD ~ min_min_om_rmse
    )
  ) %>%
  select(-c(min_min_cap_rmse, min_min_om_rmse))

#* Assign Porous pavement BMP with HSG A a capital cost of $0, according to assumption set by Naomi 
allHRUs_costs <- allHRUs_costs %>% mutate(capital_2020USD = case_when(
  BMP.Type == "Porous_Pavement_w_UD" & HSG == "A" ~ 0, 
  TRUE ~ capital_2020USD
))


# 5. Add in Rain Garden Costs -----


### Make rain garden data frames compatible -----
RG_10in_pond <- RG_10in_pond %>% rename(BMP.Type = BMP)
RG_6in_pond <- RG_6in_pond %>% rename(BMP.Type = BMP)

rain_garden_cost <- rbind(RG_10in_pond, RG_6in_pond)

rain_garden_cost <- rain_garden_cost %>%
  select(BMP.Type, PondDepth_in, HSG:ro2d_in, capital_2020USD = capital_2020USDacreWQ, operations_2020USD = operations_2020USDacryrWQ)
rain_garden_cost$BMP.Type <- gsub(" ", "_", rain_garden_cost$BMP.Type)
rain_garden_cost$BMP.Type <- gsub("/", "_", rain_garden_cost$BMP.Type)

### Prepare subset of cost data frame -----
# Prepare a subset of the costs data frame to combine with the rain garden cost data to be able to row bind the data frames to each other
rain_garden_temp <- allHRUs_costs %>% filter(BMP.Type == "Rain_Garden")

RG_costs_temp <- merge(rain_garden_temp, rain_garden_cost, by = c("BMP.Type", "COMID", "Land_Use", "HSG"), all.y = T)

RG_costs <- RG_costs_temp %>% mutate(BMP.Type = paste0(BMP.Type, PondDepth_in)) %>% 
  select(-capital_2020USD.x,-operations_2020USD.x,-PondDepth_in,-AnAvPpt_in.y,-ro2d_in.y) %>%
  rename(AnAvPpt_in = AnAvPpt_in.x, ro2d_in = ro2d_in.x, capital_2020USD = capital_2020USD.y, operations_2020USD = operations_2020USD.y)

allHRUs_cost_noRG <- allHRUs_costs %>% filter(!BMP.Type == "Rain_Garden")

### Row bind rain garden costs to previous cost data frame of all other HRUs -----
#* NOTE: There are two new BMPs now - Rain Garden6 and Rain Garden10. There is no "Rain Garden" anymore. The same efficiencies are used, this will need to be checked.
final_allHRUcosts <- rbind(allHRUs_cost_noRG, RG_costs)


# 6. Substitute Costs for remaining BMPs with neg costs -----

### Read in substitution files -----
inf_basin_capcosts <- read.xlsx("./Data/Inbncapcostparam_statswq12.xlsx", sheet = "#LN00065", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error,
                Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
                bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

inf_basin_capcosts$BMP.Type <- gsub(" ", "_", inf_basin_capcosts$BMP.Type)
inf_basin_capcosts$BMP.Type <- gsub("/", "_", inf_basin_capcosts$BMP.Type)

inf_basin_omcosts <- read.xlsx("./Data/InfBasinOMmincostequations_Models1and2.xls.xlsx", sheet = "#LN02535", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, Annav_omcost_coeff = AnnavPrecipin, om_rmse = Root.mean.squared.error,
                ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, AIC1:AIC4,
                bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel)

inf_basin_omcosts$BMP.Type <- gsub(" ", "_", inf_basin_omcosts$BMP.Type)
inf_basin_omcosts$BMP.Type <- gsub("/", "_", inf_basin_omcosts$BMP.Type)

porpav_capcosts <- read.xlsx("./Data/Capcostparamppwid_statswq2.xlsx", sheet = "#LN00058", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_capcost, min_capcost, max_capcost, cap_rmse = Root.mean.squared.error,
                Annav_capcost_coeff = AnnavPrecipin, ro2d_capcost_coeff = ro2d_in, Annav2_capcost_coeff = AnnavPrecipin2, ro2d2_capcost_coeff = ro2d_in2, AIC1:AIC4,
                bestAIC_capcost = bestAIC, bestmodel_capcost = bestmodel, PrecOrder_capcost = PrecOrder, Ro2dOrder_capcost = Ro2dOrder)

porpav_capcosts$BMP.Type <- gsub(" ", "_", porpav_capcosts$BMP.Type)
porpav_capcosts$BMP.Type <- gsub("/", "_", porpav_capcosts$BMP.Type)

porpav_omcosts <- read.xlsx("./Data/Omcostparamppwid_statswq2.xlsx", sheet = "#LN00061", startRow = 3) %>%
  dplyr::select(BMP.Type, Land_Use, HSG = Hydgrp, ModelLabel = Label.of.model, mean_omcost, min_omcost, max_omcost, om_rmse = Root.mean.squared.error,
                Annav_omcost_coeff = AnnavPrecipin, ro2d_omcost_coeff = ro2d_in, Annav2_omcost_coeff = AnnavPrecipin2, ro2d2_omcost_coeff = ro2d_in2, 
                bestAIC_omcost = bestAIC, bestmodel_omcost = bestmodel, PrecOrder_omcost = PrecOrder, Ro2dOrder_omcost = Ro2dOrder)

porpav_omcosts$BMP.Type <- gsub(" ", "_", porpav_omcosts$BMP.Type)
porpav_omcosts$BMP.Type <- gsub("/", "_", porpav_omcosts$BMP.Type)


### Pull out parts of final all hru to merge -----

##### Fixing Infiltration Basin Cap Costs  -----

inf_neg <- final_allHRUcosts %>% filter(BMP.Type == "Infiltration_Basin")

inf_neg_cap_sub <- merge(inf_neg, inf_basin_capcosts, by = c("BMP.Type", "Land_Use", "HSG"))

inf_cap_cost <- inf_neg_cap_sub %>% mutate(
  new_cap_cost = case_when(
    ModelLabel == "MODEL1" ~ (Annav_capcost_coeff.y * AnAvPpt_in),
    ModelLabel == "MODEL2" ~ (ro2d_capcost_coeff.y * ro2d_in),
    TRUE ~ mean_capcost.y)) %>% filter(ModelLabel == "MODEL1")


inf_cost <- inf_cap_cost %>% mutate(
  ModelLabel.x = case_when(capital_2020USD < 0 ~ ModelLabel, TRUE ~ ModelLabel.x),
  mean_capcost.x = case_when(capital_2020USD < 0 ~ mean_capcost.y, TRUE ~ mean_capcost.x),
  min_capcost.x = case_when(capital_2020USD < 0 ~ min_capcost.y, TRUE ~ min_capcost.x),
  max_capcost.x = case_when(capital_2020USD < 0 ~ max_capcost.y, TRUE ~ max_capcost.x),
  Annav_capcost_coeff.x = case_when(capital_2020USD < 0 ~ Annav_capcost_coeff.y, TRUE ~ Annav_capcost_coeff.x),
  ro2d_capcost_coeff.x = case_when(capital_2020USD < 0 ~ ro2d_capcost_coeff.y, TRUE ~ ro2d2_capcost_coeff.x),
  Annav2_capcost_coeff.x = case_when(capital_2020USD < 0 ~ Annav2_capcost_coeff.y, TRUE ~ Annav2_capcost_coeff.x),
  ro2d2_capcost_coeff.x = case_when(capital_2020USD < 0 ~ ro2d2_capcost_coeff.y, TRUE ~ ro2d2_capcost_coeff.x),
  bestAIC_capcost.x = case_when(capital_2020USD < 0 ~ bestAIC_capcost.y, TRUE ~ bestAIC_capcost.x),
  bestmodel_capcost.x = case_when(capital_2020USD < 0 ~ bestmodel_capcost.y, TRUE ~ bestmodel_capcost.x),
  PrecOrder_capcost.x = case_when(capital_2020USD < 0 ~ PrecOrder_capcost.y, TRUE ~ PrecOrder_capcost.x),
  Ro2dOrder_capcost.x = case_when(capital_2020USD < 0 ~ Ro2dOrder_capcost.y, TRUE ~ Ro2dOrder_capcost.x),
  capital_2020USD = case_when(capital_2020USD < 0 ~ new_cap_cost, TRUE ~ capital_2020USD)
) %>% select(BMP.Type:operations_2020USD) %>%
  rename(mean_capcost = mean_capcost.x, 
         min_capcost = min_capcost.x,
         max_capcost = max_capcost.x,
         cap_rmse = cap_rmse.x,
         Annav_capcost_coeff = Annav_capcost_coeff.x,
         ro2d_capcost_coeff = ro2d_capcost_coeff.x,
         Annav2_capcost_coeff = Annav2_capcost_coeff.x,
         ro2d2_capcost_coeff = ro2d2_capcost_coeff.x,
         AIC1 = AIC1.x,
         AIC2 = AIC2.x,
         AIC3 = AIC3.x,
         AIC4 = AIC4.x,
         bestAIC_capcost = bestAIC_capcost.x,
         bestmodel_capcost = bestmodel_capcost.x,
         PrecOrder_capcost = PrecOrder_capcost.x,
         Ro2dOrder_capcost = Ro2dOrder_capcost.x)

no_inf_costs <- final_allHRUcosts %>% filter(!BMP.Type == "Infiltration_Basin")

all_HRU_temp <- rbind(no_inf_costs, inf_cost)


##### Fixing Infiltration Basin OM Costs -----

inf_pos <- all_HRU_temp %>% filter(BMP.Type == "Infiltration_Basin") %>% filter(operations_2020USD >= 0)
inf_neg <- all_HRU_temp %>% filter(BMP.Type == "Infiltration_Basin") %>% filter(operations_2020USD <0)

inf_neg_om_sub <- merge(inf_neg, inf_basin_omcosts, by = c("BMP.Type", "Land_Use", "HSG"))

inf_om_cost_temp <- inf_neg_om_sub %>% mutate(
  new_om_cost = case_when(
    ModelLabel == "MODEL1" ~ (Annav_omcost_coeff.y * AnAvPpt_in),
    ModelLabel == "MODEL2" ~ (ro2d_omcost_coeff.y * ro2d_in),
    TRUE ~ operations_2020USD))

inf_om_cost <- inf_om_cost_temp %>% group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
  slice_max(new_om_cost, n = 1) %>% ungroup()

inf_cost_temp <- inf_om_cost %>% mutate(
  ModelLabel.y = case_when(operations_2020USD < 0 ~ ModelLabel, TRUE ~ ModelLabel.y),
  Annav_omcost_coeff.x = case_when(operations_2020USD < 0 ~ Annav_omcost_coeff.y, TRUE ~ Annav_omcost_coeff.x),
  ro2d_omcost_coeff.x = case_when(operations_2020USD < 0 ~ ro2d_omcost_coeff.y, TRUE ~ ro2d2_omcost_coeff.x),
  Annav2_omcost_coeff.x = case_when(operations_2020USD < 0 ~ Annav2_omcost_coeff.y, TRUE ~ Annav2_omcost_coeff.x),
  ro2d2_omcost_coeff.x = case_when(operations_2020USD < 0 ~ ro2d2_omcost_coeff.y, TRUE ~ ro2d2_omcost_coeff.x),
  bestAIC_omcost.x = case_when(operations_2020USD < 0 ~ bestAIC_omcost.y, TRUE ~ bestAIC_omcost.x),
  bestmodel_omcost.x = case_when(operations_2020USD < 0 ~ bestmodel_omcost.y, TRUE ~ bestmodel_omcost.x),
  operations_2020USD = case_when(operations_2020USD < 0 ~ new_om_cost, TRUE ~ operations_2020USD)
) %>% select(BMP.Type:operations_2020USD) %>%
  rename(om_rmse = om_rmse.x,
         Annav_omcost_coeff = Annav_omcost_coeff.x,
         ro2d_omcost_coeff = ro2d_omcost_coeff.x,
         Annav2_omcost_coeff = Annav2_omcost_coeff.x,
         ro2d2_omcost_coeff = ro2d2_omcost_coeff.x,
         AIC1 = AIC1.x,
         AIC2 = AIC2.x,
         AIC3 = AIC3.x,
         AIC4 = AIC4.x,
         bestAIC_omcost = bestAIC_omcost.x,
         bestmodel_omcost = bestmodel_omcost.x)

inf_cost <- rbind(inf_pos, inf_cost_temp)

no_inf_costs <- all_HRU_temp %>% filter(!BMP.Type == "Infiltration_Basin")

all_HRU_temp <- rbind(no_inf_costs, inf_cost)

##### Fixing Porous Pavement Capital Costs -----

pp_pos <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>% filter(capital_2020USD >= 0)
pp_neg <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>% filter(capital_2020USD < 0)

pp_neg_cap_sub <- merge(pp_neg, porpav_capcosts, by = c("BMP.Type", "Land_Use", "HSG"))

pp_cap_cost_temp <- pp_neg_cap_sub %>% mutate(
  new_cap_cost = case_when(
    ModelLabel == "MODEL1" ~ (Annav_capcost_coeff.y * AnAvPpt_in),
    ModelLabel == "MODEL2" ~ (ro2d_capcost_coeff.y * ro2d_in),
    TRUE ~ mean_capcost.y))

pp_cap_cost <- pp_cap_cost_temp %>% group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
  slice_max(new_cap_cost, n = 1) %>% ungroup()

pp_cost_temp <- pp_cap_cost %>% mutate(
  ModelLabel.x = case_when(capital_2020USD < 0 ~ ModelLabel, TRUE ~ ModelLabel.x),
  mean_capcost.x = case_when(capital_2020USD < 0 ~ mean_capcost.y, TRUE ~ mean_capcost.x),
  min_capcost.x = case_when(capital_2020USD < 0 ~ min_capcost.y, TRUE ~ min_capcost.x),
  max_capcost.x = case_when(capital_2020USD < 0 ~ max_capcost.y, TRUE ~ max_capcost.x),
  Annav_capcost_coeff.x = case_when(capital_2020USD < 0 ~ Annav_capcost_coeff.y, TRUE ~ Annav_capcost_coeff.x),
  ro2d_capcost_coeff.x = case_when(capital_2020USD < 0 ~ ro2d_capcost_coeff.y, TRUE ~ ro2d2_capcost_coeff.x),
  Annav2_capcost_coeff.x = case_when(capital_2020USD < 0 ~ Annav2_capcost_coeff.y, TRUE ~ Annav2_capcost_coeff.x),
  ro2d2_capcost_coeff.x = case_when(capital_2020USD < 0 ~ ro2d2_capcost_coeff.y, TRUE ~ ro2d2_capcost_coeff.x),
  bestmodel_capcost.x = case_when(capital_2020USD < 0 ~ bestmodel_capcost.y, TRUE ~ bestmodel_capcost.x),
  PrecOrder_capcost.x = case_when(capital_2020USD < 0 ~ PrecOrder_capcost.y, TRUE ~ PrecOrder_capcost.x),
  Ro2dOrder_capcost.x = case_when(capital_2020USD < 0 ~ Ro2dOrder_capcost.y, TRUE ~ Ro2dOrder_capcost.x),
  capital_2020USD = case_when(capital_2020USD < 0 ~ new_cap_cost, TRUE ~ capital_2020USD)
) %>% select(BMP.Type:operations_2020USD) %>%
  rename(mean_capcost = mean_capcost.x, 
         min_capcost = min_capcost.x,
         max_capcost = max_capcost.x,
         cap_rmse = cap_rmse.x,
         Annav_capcost_coeff = Annav_capcost_coeff.x,
         ro2d_capcost_coeff = ro2d_capcost_coeff.x,
         Annav2_capcost_coeff = Annav2_capcost_coeff.x,
         ro2d2_capcost_coeff = ro2d2_capcost_coeff.x,
         AIC1 = AIC1.x,
         AIC2 = AIC2.x,
         AIC3 = AIC3.x,
         AIC4 = AIC4.x,
         bestAIC_capcost = bestAIC_capcost.x,
         bestmodel_capcost = bestmodel_capcost.x,
         PrecOrder_capcost = PrecOrder_capcost.x,
         Ro2dOrder_capcost = Ro2dOrder_capcost.x)

pp_cost <- rbind(pp_pos, pp_cost_temp)

no_pp_costs <- all_HRU_temp %>% filter(!BMP.Type == "Porous_Pavement_w_UD")

all_HRU_temp <- rbind(no_pp_costs, pp_cost)


##### Fixing Porous Pavement OM Costs -----

pp_pos <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>% filter(operations_2020USD >= 0)
pp_neg <- all_HRU_temp %>% filter(BMP.Type == "Porous_Pavement_w_UD") %>% filter(operations_2020USD < 0 | is.na(operations_2020USD))

pp_neg_om_sub <- merge(pp_neg, porpav_omcosts, by = c("BMP.Type", "Land_Use", "HSG"))

pp_om_cost_temp <- pp_neg_om_sub %>% mutate(
  new_om_cost = case_when(
    ModelLabel == "MODEL1" ~ (Annav_omcost_coeff.y * AnAvPpt_in),
    ModelLabel == "MODEL2" ~ (ro2d_omcost_coeff.y * ro2d_in),
    TRUE ~ mean_omcost.y))

pp_om_cost <- pp_om_cost_temp %>% group_by(BMP.Type, Land_Use, HSG, COMID) %>% 
  slice_max(new_om_cost, n = 1) %>% ungroup()

pp_cost_temp <- pp_om_cost %>% mutate(
  ModelLabel.y = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ ModelLabel, TRUE ~ ModelLabel.y),
  mean_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ mean_omcost.y, TRUE ~ mean_omcost.x),
  min_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ min_omcost.y, TRUE ~ min_omcost.x),
  max_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ max_omcost.y, TRUE ~ max_omcost.x),
  Annav_omcost_coeff.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ Annav_omcost_coeff.y, TRUE ~ Annav_omcost_coeff.x),
  ro2d_omcost_coeff.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ ro2d_omcost_coeff.y, TRUE ~ ro2d2_omcost_coeff.x),
  Annav2_omcost_coeff.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ Annav2_omcost_coeff.y, TRUE ~ Annav2_omcost_coeff.x),
  ro2d2_omcost_coeff.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ ro2d2_omcost_coeff.y, TRUE ~ ro2d2_omcost_coeff.x),
  bestmodel_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ bestmodel_omcost.y, TRUE ~ bestmodel_omcost.x),
  PrecOrder_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ PrecOrder_omcost.y, TRUE ~ PrecOrder_omcost.x),
  Ro2dOrder_omcost.x = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ Ro2dOrder_omcost.y, TRUE ~ Ro2dOrder_omcost.x),
  operations_2020USD = case_when(operations_2020USD < 0 | is.na(operations_2020USD) ~ new_om_cost, TRUE ~ operations_2020USD)
) %>% select(BMP.Type:operations_2020USD) %>%
  rename(mean_omcost = mean_omcost.x,
         min_omcost = min_omcost.x,
         max_omcost = max_omcost.x,
         om_rmse = om_rmse.x,
         Annav_omcost_coeff = Annav_omcost_coeff.x,
         ro2d_omcost_coeff = ro2d_omcost_coeff.x,
         Annav2_omcost_coeff = Annav2_omcost_coeff.x,
         ro2d2_omcost_coeff = ro2d2_omcost_coeff.x,
         bestAIC_omcost = bestAIC_omcost.x,
         bestmodel_omcost = bestmodel_omcost.x,
         PrecOrder_omcost = PrecOrder_omcost.x,
         Ro2dOrder_omcost = Ro2dOrder_omcost.x)

pp_cost <- rbind(pp_pos, pp_cost_temp)

no_pp_costs <- all_HRU_temp %>% filter(!BMP.Type == "Porous_Pavement_w_UD")

all_HRU <- rbind(no_pp_costs, pp_cost)

all_HRU <- all_HRU %>% mutate(effic_N = case_when(is.na(effic_N) ~ 0, TRUE ~ effic_N),
                              effic_P = case_when(is.na(effic_P) ~ 0, TRUE ~ effic_P)) #@# Where we have no data, assigns efficiency of 0 so RBEROST is detered from selecting


# 7. Initial QA -----

cost_capqa <- all_HRU %>% filter(capital_2020USD < 0) 
cost_omqa <- all_HRU %>% filter(operations_2020USD < 0)

### Costs -----
hist(all_HRU$capital_2020USD)
hist(all_HRU$operations_2020USD)

summary(all_HRU$capital_2020USD)
summary(all_HRU$operations_2020USD)

om_qa <- all_HRU %>% filter(is.na(operations_2020USD))
om_count <- all_HRU %>% group_by(BMP.Type) %>% tally()

### Efficiencies -----
hist(all_HRU$effic_N)
hist(all_HRU$effic_P)

summary(all_HRU$effic_N)
summary(all_HRU$effic_P)

wetpond_qa <- all_HRU %>% filter(BMP.Type == "Wet_Pond") 
summary(wetpond_qa$effic_N)


# 8. Implementing Threshold Values & Subsequent QA -----


### Combined Threshold Values (EPA preferred) -----

all_HRU_ce <- all_HRU %>% mutate(cap_p = capital_2020USD/(effic_P*100),
                                 cap_n = capital_2020USD/(effic_N*100),
                                 op_p = operations_2020USD/(effic_P*100),
                                 op_n = operations_2020USD/(effic_N*100))

cap_p_thresh <- quantile(all_HRU_ce$cap_p, 0.75)
cap_n_thresh <- quantile(all_HRU_ce$cap_n, 0.75)
op_p_thresh <- quantile(all_HRU_ce$op_p, 0.75)
op_n_thresh <- quantile(all_HRU_ce$op_n, 0.75)

### Applying thresholds defined aboved -----
filter_qa <- all_HRU_ce %>% filter(cap_n <= cap_n_thresh & cap_p <= cap_p_thresh & op_n <= op_n_thresh & op_p <= op_p_thresh)

### New QA check  -----
thresh_qa <- filter_qa %>% group_by(BMP.Type) %>% tally()
hru_thresh <- filter_qa %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
length(unique(hru_thresh$index))

summary(filter_qa$capital_2020USD)
summary(filter_qa$operations_2020USD)
summary(filter_qa$effic_N)
summary(filter_qa$effic_P)

effic_qa <- filter_qa %>% filter(effic_N < 0.05)
effic_qaP <- filter_qa %>% filter(effic_P < 0.05)

##### Cost comparisons -----
hist(filter_qa$capital_2020USD)
hist(filter_qa$operations_2020USD)

summary(filter_qa$capital_2020USD)
summary(filter_qa$operations_2020USD)

##### Efficiency comparisons -----
hist(filter_qa$effic_N)
hist(filter_qa$effic_P)

summary(filter_qa$effic_N)
summary(filter_qa$effic_P)


### Individual Threshold Values -----

p_thresh <- 0.05
n_thresh <- 0.05
capital_thresh <- quantile(all_HRU$capital_2020USD, 0.75)
operations_thresh <- quantile(all_HRU$operations_2020USD, 0.75)

effic_drop <- all_HRU %>% filter(effic_N < 0.05 & effic_P < 0.05)

bound_HRU_temp <- anti_join(all_HRU, effic_drop, by = c("BMP.Type", "Land_Use", "HSG", "COMID"))

all_HRU_bounded <- bound_HRU_temp %>% filter(capital_2020USD <= capital_thresh & operations_2020USD <= operations_thresh)

thresh_qa <- all_HRU_bounded %>% group_by(BMP.Type) %>% tally()

effic_qa_old <- all_HRU_bounded %>% filter(effic_N < 0.05)
effic_qaP_old <- all_HRU_bounded %>% filter(effic_P < 0.05)

##### Cost comparisons -----
hist(all_HRU_bounded$capital_2020USD)
hist(all_HRU_bounded$operations_2020USD)

summary(all_HRU_bounded$capital_2020USD)
summary(all_HRU_bounded$operations_2020USD)

##### Efficiency comparisons -----
hist(all_HRU_bounded$effic_N)
hist(all_HRU_bounded$effic_P)

summary(all_HRU_bounded$effic_N)
summary(all_HRU_bounded$effic_P)


### Applying only cost thresholds from individual threshold values -----
trial_thresh <- all_HRU %>% filter(capital_2020USD <= capital_thresh, operations_2020USD <= operations_thresh)
dropped2 <- anti_join(all_HRU, trial_thresh, by = c("BMP.Type", "Land_Use", "HSG", "COMID"))
dropped_qa2 <- dropped2 %>% group_by(BMP.Type) %>% tally()
summary(dropped2$capital_2020USD)
summary(dropped2$operations_2020USD)

cost_thresh_qa <- trial_thresh %>% group_by(BMP.Type) %>% tally()

### Compare COMID, Land Use, and HSG combinations -----
# Comparing comid x land use x HSG combinations and what is dropped with the application of thresholds (compare to hru_thresh from combined thresholds)
hru_indexed <- all_HRU %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
length(unique(hru_indexed$index))

hru_trimmed <- all_HRU_bounded %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
length(unique(hru_trimmed$index))

hru_trim2 <- trial_thresh %>% mutate(index = paste0(COMID, "&", Land_Use, "&", HSG))
length(unique(hru_trim2$index))



##### Cost comparison to WMOST technical papers -----
#Combined thresholds
cost_comp <- filter_qa %>% mutate(capital_2020USD = capital_2020USD/(43560*0.08), operations_2020USD = operations_2020USD/(43560*0.08))
summary(cost_comp$capital_2020USD)
summary(cost_comp$operations_2020USD)

cost_comp_qa <- cost_comp %>% group_by(BMP.Type) %>% summarise(capital_2020USD = mean(capital_2020USD), operations_2020USD = mean(operations_2020USD))

##### Cost comparison to WMOST technical papers -----
#Individual thresholds
cost_comp_old <- all_HRU_bounded %>% mutate(capital_2020USD = capital_2020USD/(43560*0.08), operations_2020USD = operations_2020USD/(43560*0.08))
summary(cost_comp_old$capital_2020USD)
summary(cost_comp_old$operations_2020USD)

comp_old_qa <- cost_comp_old %>% group_by(BMP.Type) %>% summarise(capital_2020USD = mean(capital_2020USD), operations_2020USD = mean(operations_2020USD))


# 9. Print Final Dataset -----

#### Write out dataset -----
# Choose data set to write depending on which threshold method EPA moves forward with (currently using combined thresholds)
final_data <- filter_qa %>% select(BMP.Type:HSG, Area, effic_N, effic_P, std_efficN, std_efficP, n_efficN, n_efficP,
                                   capital_2020USD, cap_rmse, operations_2020USD, om_rmse) %>% rename(comid = COMID, BMP = BMP.Type)

write.csv(final_data, paste0(InPath,"UrbanBMPData.csv"), row.names = F)

## Summary stats
summary_table <- filter_qa %>%
  group_by(BMP.Type, Land_Use, HSG) %>%
  summarize(avg_efficN = mean(effic_N),
            avg_efficP = mean(effic_P),
            avg_cap = mean(capital_2020USD),
            avg_om = mean(operations_2020USD))
write.csv(summary_table, "./Data/BMP_cost_summary.csv", row.names = F)
