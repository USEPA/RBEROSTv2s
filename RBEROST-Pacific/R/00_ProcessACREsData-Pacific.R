  
###########################################################################################
# PURPOSE: Calculate BMP Nitrogen & Phosphorus Removal Efficiencies  ####
# BY: Yishen Li, updated by Alyssa Le updated again by Cathy Chamberlin ####
# ORIGINAL DATE: 5/27/2020 ####
###########################################################################################

# The purpose of this code is to calculate removal efficiencies based on the ACRE database.
# Lines of code tagged with #*# may need to be editted by the user.

##########################################################
# 1. Setup #####
##########################################################

# install.packages("dplyr") #*# #May need to be run if packages are not installed
# install.packages("stringr") #*# #May need to be run if packages are not installed

library("tidyverse")
library("stringr")
options(stringsAsFactors = FALSE)

##########################################################
# 2. Read CSV files ####
##########################################################

# Working directory
setwd("./") #*# This defaults to the project directory folder if run in an RProject in RStudio

# This dataset is a subset of the ACRE database for HUC 0108.
ACRE_rawdata <- read.csv(
  "Data/acre_171100_cleaneddelimiters.csv", #*#
  header=TRUE,
  colClasses = c(
    "NULL",
    "character",
    "NULL",
    "character",
    rep("NULL",2),
    "character",
    rep("NULL",3),
    "character",
    rep("NULL",5),
    "character",
    rep("NULL",2),
    "character",
    "character",
    rep("NULL",31),
    "character",
    rep("NULL",11),
    "character",
    rep("NULL",5)
    )
  )

# Read in a full set of HUC12s for HUC 1711.
HUC12_PACSPARROW <- read.csv(
  "./RBEROST-Pacific/Preprocessing/Inputs/pac_sparrow_model_input.csv", #*#
  header=TRUE, 
  colClasses = c(rep("NULL",18), "character", rep("NULL", 60))
  )


##########################################################
# 3. Calculate nitrogen and phosphorus removal efficiency ####
##########################################################

# Rename column names
names(ACRE_rawdata) <- c(
  "HUC12", "IRRIGATED", "LULC", "Slope", "Scenario", "TP", "TN", "STD_Archive", "Hyd_Group"
  )

# TN column had some "<" symbols in it so need to convert it from character to numeric.
ACRE_rawdata$TP <- as.numeric(str_replace_all(ACRE_rawdata$TP, "<", ""))
ACRE_rawdata$TN <- as.numeric(str_replace_all(ACRE_rawdata$TN, "<", ""))
ACRE_rawdata$Hyd_Group <- str_replace_all(ACRE_rawdata$Hyd_Group, "<", "")

# Subset the data to just take out "no practice" & "baseline" runs 
baseline_subset_temp_bsln <- filter(ACRE_rawdata, Scenario == "Baseline")
baseline_subset_bsln <- baseline_subset_temp_bsln %>% select(HUC12, Scenario, TP, TN, STD_Archive)
names(baseline_subset_bsln) <- c(
  "HUC12", "Scenario_Baseline", "Baseline_TP", "Baseline_TN", "STD_Archive"
  )

baseline_subset_temp_noprac <- filter(ACRE_rawdata, Scenario == "No Practice")
baseline_subset_noprac <- baseline_subset_temp_noprac %>% select(HUC12, Scenario, TP, TN, STD_Archive)
names(baseline_subset_noprac) <- c(
  "HUC12", "Scenario_Baseline", "Baseline_TP", "Baseline_TN", "STD_Archive"
  )

# Subset the data for all runs except the "no practice" & "baseline" runs
scenarios_subset_bsln <- filter(ACRE_rawdata, !(Scenario == "Baseline")) %>%
  rename(Scenario_TP = TP, Scenario_TN = TN)

scenarios_subset_noprac <- filter(
  ACRE_rawdata, !(Scenario == "No Practice")
  ) %>%
  rename(Scenario_TP = TP, Scenario_TN = TN)


# Merge the two dataframes together, by STD_Archive and HUC12 
efficiency_subset_bsln <- merge(
  baseline_subset_bsln, 
  scenarios_subset_bsln, 
  by = c("STD_Archive", "HUC12"), 
  all = TRUE
  )

efficiency_subset_noprac <- merge(
  baseline_subset_noprac, 
  scenarios_subset_noprac, 
  by = c("STD_Archive", "HUC12"), 
  all = TRUE
  )

# Add BMP Efficieny for TN, named "TN_efcy" 
efficiency_subset_bsln$TP_efcy <- with(
  efficiency_subset_bsln, (Baseline_TP - Scenario_TP) / Baseline_TP
  )
efficiency_subset_bsln$TN_efcy <- with(
  efficiency_subset_bsln, (Baseline_TN - Scenario_TN) / Baseline_TN
  )

efficiency_subset_noprac$TP_efcy <- with(
  efficiency_subset_noprac, (Baseline_TP - Scenario_TP) / Baseline_TP
  )
efficiency_subset_noprac$TN_efcy <- with(
  efficiency_subset_noprac, (Baseline_TN - Scenario_TN) / Baseline_TN
  )

# Filter out the non-relevant scenarios from the ACRE database.
efficiency_subset_scen_bsln <- efficiency_subset_bsln %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(
    !Scenario %in% c(
      "Ponds 25%", "Ponds 50%", "Ponds 75%"
      )
    ) %>%
  filter(
    LULC %in% c(
      "ALFA", "BARL", "CORN", "CORN-WWHT", "PNUT", "SWHT", "SGBT", "WWHT"
      )
    ) #*# # Alfalfa, barley, corn, corn/winter wheat, peanuts, summer wheat, sugar beets and winter wheat are all grown in the Pacific nortwest


efficiency_subset_scen_noprac <- efficiency_subset_noprac %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(
    !Scenario %in% c(
      "Ponds 25%", "Ponds 50%", "Ponds 75%"
      )
    ) %>%
  filter(
    LULC %in% c(
      "ALFA", "BARL", "CORN", "CORN-WWHT", "PNUT", "SWHT", "SGBT", "WWHT"
      )
    ) #*# # Alfalfa, barley, corn, corn/winter wheat, peanuts, summer wheat, sugar beets and winter wheat are all grown in the Pacific nortwest

# No ponds in this case study

efficiency_subset_final_bsln <- dplyr::bind_rows(
  efficiency_subset_scen_bsln
  )

efficiency_subset_final_noprac <- dplyr::bind_rows(
  efficiency_subset_scen_noprac
  )

##########################################################
# 4. Summarize by HUC12, LULC, Slope, and Hyd_Group ####
##########################################################

Eff_byHUC12_HRU_bsln <- efficiency_subset_final_bsln %>%
  select(STD_Archive, HUC12, Scenario, TP_efcy, TN_efcy) %>%
  group_by(HUC12,Scenario) %>% 
  summarise(
    MeanTP_Effic = mean(TP_efcy), 
    MeanTN_Effic = mean(TN_efcy),
    MeanTP_Effic_se = sd(TP_efcy)/sqrt(n()), 
    MeanTN_Effic_se = sd(TN_efcy)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_bsln$HUC12 <- 
  str_pad(Eff_byHUC12_HRU_bsln$HUC12, width=12, pad="0")

summary(Eff_byHUC12_HRU_bsln)

Eff_byHUC12_HRU_noprac <- efficiency_subset_final_noprac %>%
  select(STD_Archive, HUC12, Scenario, TP_efcy, TN_efcy) %>%
  group_by(HUC12,Scenario) %>% 
  summarise(
    MeanTP_Effic = mean(TP_efcy), 
    MeanTN_Effic = mean(TN_efcy),
    MeanTP_Effic_se = sd(TP_efcy)/sqrt(n()), 
    MeanTN_Effic_se = sd(TN_efcy)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_noprac$HUC12 <- 
  str_pad(Eff_byHUC12_HRU_noprac$HUC12, width=12, pad="0")

summary(Eff_byHUC12_HRU_noprac)

# Summarize by HUC10
Eff_byHUC12_HRU_bsln$HUC10 <- substring(Eff_byHUC12_HRU_bsln$HUC12,1,10)

Eff_byHUC10_HRU_bsln <- Eff_byHUC12_HRU_bsln %>%
  ungroup() %>%
  select(HUC10, HUC10_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC10, HUC10_Scen) %>%
  summarise(
    HUC10TP_Effic = mean(MeanTP_Effic), 
    HUC10TN_Effic = mean(MeanTN_Effic),
    HUC10TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC10TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC10_HRU_bsln)

Eff_byHUC12_HRU_noprac$HUC10 <- substring(Eff_byHUC12_HRU_noprac$HUC12,1,10)

Eff_byHUC10_HRU_noprac <- Eff_byHUC12_HRU_noprac %>%
  ungroup() %>%
  select(HUC10, HUC10_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC10, HUC10_Scen) %>%
  summarise(
    HUC10TP_Effic = mean(MeanTP_Effic), 
    HUC10TN_Effic = mean(MeanTN_Effic),
    HUC10TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC10TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC10_HRU_noprac)

# Summarize by HUC8
Eff_byHUC12_HRU_bsln$HUC8 <- substring(Eff_byHUC12_HRU_bsln$HUC12,1,8)

Eff_byHUC8_HRU_bsln <- Eff_byHUC12_HRU_bsln %>%
  ungroup() %>%
  select(HUC8, HUC8_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC8, HUC8_Scen) %>%
  summarise(
    HUC8TP_Effic = mean(MeanTP_Effic),
    HUC8TN_Effic = mean(MeanTN_Effic),
    HUC8TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC8TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC8_HRU_bsln)

Eff_byHUC12_HRU_noprac$HUC8 <- substring(Eff_byHUC12_HRU_noprac$HUC12,1,8)

Eff_byHUC8_HRU_noprac <- Eff_byHUC12_HRU_noprac %>%
  ungroup() %>%
  select(HUC8, HUC8_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC8, HUC8_Scen) %>%
  summarise(
    HUC8TP_Effic = mean(MeanTP_Effic),
    HUC8TN_Effic = mean(MeanTN_Effic),
    HUC8TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC8TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC8_HRU_noprac)

# Summarize by HUC6
Eff_byHUC12_HRU_bsln$HUC6 <- substring(Eff_byHUC12_HRU_bsln$HUC12,1,6)

Eff_byHUC6_HRU_bsln <- Eff_byHUC12_HRU_bsln %>%
  ungroup() %>%
  select(HUC6, HUC6_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC6, HUC6_Scen) %>%
  summarise(
    HUC6TP_Effic = mean(MeanTP_Effic),
    HUC6TN_Effic = mean(MeanTN_Effic),
    HUC6TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC6TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC6_HRU_bsln)

Eff_byHUC12_HRU_noprac$HUC6 <- substring(Eff_byHUC12_HRU_noprac$HUC12,1,6)

Eff_byHUC6_HRU_noprac <- Eff_byHUC12_HRU_noprac %>%
  ungroup() %>%
  select(HUC6, HUC6_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC6, HUC6_Scen) %>%
  summarise(
    HUC6TP_Effic = mean(MeanTP_Effic),
    HUC6TN_Effic = mean(MeanTN_Effic),
    HUC6TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC6TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

summary(Eff_byHUC6_HRU_noprac)

##########################################################
# 5. Fill in any HUC12 gaps ####
##########################################################

# Add in leading zero if HUC12 ID isn't 12 characters.
HUC12_PACSPARROW$HUC12_Char <- str_pad(
  HUC12_PACSPARROW$huc12, width=12, pad="0"
  )

# Merge the HUC12 list and HUC12-level efficiency data.
HUC12_171100 <- HUC12_PACSPARROW[
  substr(HUC12_PACSPARROW$HUC12_Char,1,6)=="171100",2
  ]
HUC12_List <- data.frame(unique(HUC12_171100))
names(HUC12_List) <- c("HUC12")
Efficiency_HUC12_bsln <- 
  merge(HUC12_List, Eff_byHUC12_HRU_bsln, by="HUC12", all.x=TRUE)

summary(Efficiency_HUC12_bsln)

Efficiency_HUC12_noprac <- 
  merge(HUC12_List, Eff_byHUC12_HRU_noprac, by="HUC12", all.x=TRUE)

summary(Efficiency_HUC12_noprac)

#Merge the HUC10-level efficiency data.
Efficiency_HUC12_bsln$HUC10 <- substring(Efficiency_HUC12_bsln$HUC12,1,10)
Efficiency_HUC10_bsln <- merge(
  Efficiency_HUC12_bsln, Eff_byHUC10_HRU_bsln, by="HUC10", all.x = TRUE
  )

Efficiency_HUC10_bsln$MeanTP_Effic <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTP_Effic), HUC10TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC10_bsln$MeanTN_Effic <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTN_Effic), HUC10TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC10_bsln$MeanTP_Effic_se <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTP_Effic_se), HUC10TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC10_bsln$MeanTN_Effic_se <- with(
  Efficiency_HUC10_bsln,
  ifelse(is.na(MeanTN_Effic_se), HUC10TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC10_bsln$Scenario <- with(
  Efficiency_HUC10_bsln, ifelse(is.na(Scenario), HUC10_Scen, Scenario)
  )

summary(Efficiency_HUC10_bsln)

Efficiency_HUC12_noprac$HUC10 <- substring(Efficiency_HUC12_noprac$HUC12,1,10)
Efficiency_HUC10_noprac <- merge(
  Efficiency_HUC12_noprac, Eff_byHUC10_HRU_noprac, by="HUC10", all.x = TRUE
  )

Efficiency_HUC10_noprac$MeanTP_Effic <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTP_Effic), HUC10TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC10_noprac$MeanTN_Effic <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTN_Effic), HUC10TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC10_noprac$MeanTP_Effic_se <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTP_Effic_se), HUC10TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC10_noprac$MeanTN_Effic_se <- with(
  Efficiency_HUC10_noprac,
  ifelse(is.na(MeanTN_Effic_se), HUC10TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC10_noprac$Scenario <- with(
  Efficiency_HUC10_noprac, ifelse(is.na(Scenario), HUC10_Scen, Scenario)
  )

summary(Efficiency_HUC10_noprac)

# Merge the HUC8-level efficiency data.
Efficiency_HUC10_bsln$HUC8 <- substring(Efficiency_HUC10_bsln$HUC12,1,8)
Efficiency_HUC8_bsln <- merge(
  Efficiency_HUC10_bsln, Eff_byHUC8_HRU_bsln, by="HUC8", all.x = TRUE
  )

Efficiency_HUC8_bsln$MeanTP_Effic <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(MeanTP_Effic), HUC8TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC8_bsln$MeanTN_Effic <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(MeanTN_Effic), HUC8TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC8_bsln$MeanTP_Effic_se <- with(
  Efficiency_HUC8_bsln, 
  ifelse(is.na(MeanTP_Effic_se), HUC8TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC8_bsln$MeanTN_Effic_se <- with(
  Efficiency_HUC8_bsln, 
  ifelse(is.na(MeanTN_Effic_se), HUC8TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC8_bsln$Scenario <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(Scenario), HUC8_Scen, Scenario)
  )

summary(Efficiency_HUC8_bsln)

Efficiency_HUC10_noprac$HUC8 <- substring(Efficiency_HUC10_noprac$HUC12,1,8)
Efficiency_HUC8_noprac <- merge(
  Efficiency_HUC10_noprac, Eff_byHUC8_HRU_noprac, by="HUC8", all.x = TRUE
  )

Efficiency_HUC8_noprac$MeanTP_Effic <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTP_Effic), HUC8TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC8_noprac$MeanTN_Effic <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTN_Effic), HUC8TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC8_noprac$MeanTP_Effic_se <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTP_Effic_se), HUC8TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC8_noprac$MeanTN_Effic_se <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTN_Effic_se), HUC8TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC8_noprac$Scenario <- with(
  Efficiency_HUC8_noprac, ifelse(is.na(Scenario), HUC8_Scen, Scenario)
  )

summary(Efficiency_HUC8_noprac)

# Confirmed there are no more MeanTN_Effic "NA" values.
table(is.na(Efficiency_HUC8_bsln$MeanTN_Effic))
table(is.na(Efficiency_HUC8_bsln$MeanTN_Effic_se))
table(is.na(Efficiency_HUC8_bsln$MeanTP_Effic))
table(is.na(Efficiency_HUC8_bsln$MeanTP_Effic_se))

table(is.na(Efficiency_HUC8_noprac$MeanTN_Effic))
table(is.na(Efficiency_HUC8_noprac$MeanTN_Effic_se))
table(is.na(Efficiency_HUC8_noprac$MeanTP_Effic))
table(is.na(Efficiency_HUC8_noprac$MeanTP_Effic_se))

# Merge the HUC6-level efficiency data.
Efficiency_HUC8_bsln$HUC6 <- substring(Efficiency_HUC8_bsln$HUC12,1,6)
Efficiency_HUC6_bsln <- merge(
  Efficiency_HUC8_bsln, Eff_byHUC6_HRU_bsln, by="HUC6", all.x = TRUE
  )

Efficiency_HUC6_bsln$MeanTP_Effic <- with(
  Efficiency_HUC6_bsln, ifelse(is.na(MeanTP_Effic), HUC6TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC6_bsln$MeanTN_Effic <- with(
  Efficiency_HUC6_bsln, ifelse(is.na(MeanTN_Effic), HUC6TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC6_bsln$MeanTP_Effic_se <- with(
  Efficiency_HUC6_bsln, 
  ifelse(is.na(MeanTP_Effic_se), HUC6TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC6_bsln$MeanTN_Effic_se <- with(
  Efficiency_HUC6_bsln, 
  ifelse(is.na(MeanTN_Effic_se), HUC6TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC6_bsln$Scenario <- with(
  Efficiency_HUC6_bsln, ifelse(is.na(Scenario), HUC6_Scen, Scenario)
  )

summary(Efficiency_HUC6_bsln)

Efficiency_HUC8_noprac$HUC6 <- substring(Efficiency_HUC8_noprac$HUC12,1,6)
Efficiency_HUC6_noprac <- merge(
  Efficiency_HUC8_noprac, Eff_byHUC6_HRU_noprac, by="HUC6", all.x = TRUE
  )

Efficiency_HUC6_noprac$MeanTP_Effic <- with(
  Efficiency_HUC6_noprac, 
  ifelse(is.na(MeanTP_Effic), HUC6TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC6_noprac$MeanTN_Effic <- with(
  Efficiency_HUC6_noprac, 
  ifelse(is.na(MeanTN_Effic), HUC6TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC6_noprac$MeanTP_Effic_se <- with(
  Efficiency_HUC6_noprac, 
  ifelse(is.na(MeanTP_Effic_se), HUC6TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC6_noprac$MeanTN_Effic_se <- with(
  Efficiency_HUC6_noprac, 
  ifelse(is.na(MeanTN_Effic_se), HUC6TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC6_noprac$Scenario <- with(
  Efficiency_HUC6_noprac, ifelse(is.na(Scenario), HUC6_Scen, Scenario)
  )

summary(Efficiency_HUC6_noprac)

# Confirmed there are no more MeanTN_Effic "NA" values.
table(is.na(Efficiency_HUC6_bsln$MeanTN_Effic))
table(is.na(Efficiency_HUC6_bsln$MeanTN_Effic_se))
table(is.na(Efficiency_HUC6_bsln$MeanTP_Effic))
table(is.na(Efficiency_HUC6_bsln$MeanTP_Effic_se))

table(is.na(Efficiency_HUC6_noprac$MeanTN_Effic))
table(is.na(Efficiency_HUC6_noprac$MeanTN_Effic_se))
table(is.na(Efficiency_HUC6_noprac$MeanTP_Effic))
table(is.na(Efficiency_HUC6_noprac$MeanTP_Effic_se))


# Reduce the data
drop_col <- c(
  "HUC10_Scen", 
  "HUC10TP_Effic", 
  "HUC10TN_Effic", 
  "HUC10TP_Effic_se", 
  "HUC10TN_Effic_se", 
  "HUC8_Scen", 
  "HUC8TP_Effic",
  "HUC8TN_Effic",
  "HUC8TP_Effic_se",
  "HUC8TN_Effic_se",
  "HUC6_Scen",
  "HUC6TP_Effic",
  "HUC6TN_Effic",
  "HUC6TP_Effic_se",
  "HUC6TN_Effic_se"
  )
Efficiency_Sum_bsln <- 
  Efficiency_HUC6_bsln[,!names(Efficiency_HUC6_bsln) %in% drop_col]
Efficiency_Sum_bsln <- unique(Efficiency_Sum_bsln)
Efficiency_Sum_bsln <- Efficiency_Sum_bsln[order(Efficiency_Sum_bsln$HUC12),]

Efficiency_Sum_bsln <- Efficiency_Sum_bsln %>% 
  mutate(
    MeanTN_Effic_se = case_when(
        MeanTN_Effic_se > 0 ~ MeanTN_Effic_se, 
        MeanTN_Effic_se == 0 ~ 0.1 * abs(MeanTN_Effic)
        ),
    MeanTP_Effic_se = case_when(
        MeanTP_Effic_se > 0 ~ MeanTP_Effic_se, 
        MeanTP_Effic_se == 0 ~ 0.1 * abs(MeanTP_Effic)
        )
      )

Efficiency_Sum_noprac <- 
  Efficiency_HUC6_noprac[,!names(Efficiency_HUC6_noprac) %in% drop_col]
Efficiency_Sum_noprac <- unique(Efficiency_Sum_noprac)
Efficiency_Sum_noprac <- 
  Efficiency_Sum_noprac[order(Efficiency_Sum_noprac$HUC12),]

Efficiency_Sum_noprac <- Efficiency_Sum_noprac %>% 
  mutate(
    MeanTN_Effic_se = case_when(
        MeanTN_Effic_se > 0 ~ MeanTN_Effic_se, 
        MeanTN_Effic_se == 0 ~ 0.1 * abs(MeanTN_Effic)
        ),
    MeanTP_Effic_se = case_when(
        MeanTP_Effic_se > 0 ~ MeanTP_Effic_se, 
        MeanTP_Effic_se == 0 ~ 0.1 * abs(MeanTP_Effic)
        )
      )

##########################################################
# 6. Export the summarized database ####
##########################################################

write.csv(
  Efficiency_Sum_bsln, 
  "./RBEROST-Pacific/Preprocessing/Inputs/ACRE_HUC12_HRU_Summary_compareBaseline.csv", #*#
  row.names=FALSE
)

write.csv(
  Efficiency_Sum_noprac, 
  "./RBEROST-Pacific/Preprocessing/Inputs/ACRE_HUC12_HRU_Summary_compareNoPractice.csv", #*#
  row.names=FALSE
)

