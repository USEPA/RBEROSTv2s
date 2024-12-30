###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# PURPOSE: Determine Sizing and Costs of Urban BMPs for selected HRUs ####
# BY: Cathy Chamberlin ####
# DATE:  3/18/2022 ####
###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~###~

# Note: This file contains placeholder code that will need to be updated with the methodology for sizing and calculating costs. 
# It will also need to be updated to include efficiency calculations.
# Lines that need to be editted are tagged with #*#

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 1. Setup ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
packages <- c("readxl", "dplyr", "ggplot2", "tidyr")
lapply(packages, library, character.only = TRUE)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 2. Load Data -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
StormwaterDataPath <- paste0(
  "C:\\Users\\51243\\OneDrive - ICF\\Documents\\Desktop\\RBEROST\\GitHub\\Tier_1_Optimization-SSWR.5.3.2\\Data\\"
)
avgannpp <- fread(
  paste0(
    StormwaterDataPath, "PSppt_1983to2018anav_FULL.csv"
    )
  )

# onedayrunoffs <- read_excel(
#     paste0(
#     StormwaterDataPath, "DesignStormsBasedonInfiltrationExcessRunoff.xlsx"
#     )
# )

twodayrunoffs <- fread(
    paste0(
    StormwaterDataPath, "PS_1983to2018RO2d_in_FULL.csv"
    )
)

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 3. Format Precip Info for HRUs -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

avgannpp_formatted <- avgannpp %>%
  slice(-1:-2)
names(avgannpp_formatted) <- t(avgannpp[2,])

onedayrunoffs_formatted <- onedayrunoffs %>%
  slice(-1:-2)
names(onedayrunoffs_formatted) <- t(onedayrunoffs[2,])

twodayrunoffs_formatted <- twodayrunoffs %>%
  slice(-1:-2)
names(twodayrunoffs_formatted) <- t(twodayrunoffs[2,])

Precip_info <- full_join(
  onedayrunoffs_formatted,
  twodayrunoffs_formatted,
  by = c("huc12", "Land_Use", "Hydgrp", "_TYPE_", "_FREQ_"),
  suffix = c("_oneday", "_twoday")
  ) %>%
  select(
    huc12, Land_Use, Hydgrp, Onedayinfexrunoff_in = cum_infexcro_in, 
    Twodayinfexrunoff_in = ro2d_in
    ) %>%
  mutate(
    Onedayinfexrunoff_in = as.numeric(Onedayinfexrunoff_in),
    Twodayinfexrunoff_in = as.numeric(Twodayinfexrunoff_in)
    ) %>%
  left_join(., avgannpp_formatted, by = "huc12") %>%
  select(
    huc12, Land_Use, Hydgrp, Onedayinfexrunoff_in, Twodayinfexrunoff_in, 
    AnnavPrecip_in = AnnavPrecipin
    ) %>%
  mutate(AnnavPrecip_in = as.numeric(AnnavPrecip_in))


ggplot(Precip_info, aes(x = Onedayinfexrunoff_in, y = Twodayinfexrunoff_in)) + 
  geom_point()

ggplot(Precip_info, aes(x = AnnavPrecip_in, y = Twodayinfexrunoff_in)) + 
  geom_point()

ggplot(Precip_info, aes(x = AnnavPrecip_in, y = Onedayinfexrunoff_in)) + 
  geom_point()

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 4. Calculate Sizing -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

#*# This section needs to be adjusted

BMPs <- read.csv("./RBEROST-Pacific/Preprocessing/Inputs/01_UserSpecs_BMPs.csv") %>% #*# currently a placeholder
  filter(BMP_Category == "urban") %>%
  select(BMP) %>%
  pull()

PrecipSizing <- Precip_info %>% #*#
  crossing(., BMP = BMPs) %>%
  mutate(sizing = 1) #*#


###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 5. Calculate Costs  -----
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
#*# This section needs to be adjusted
PrecipSizingCosts <- PrecipSizing %>% #*#
  mutate(capital_2020USDacre = 1, operations_2020USDacreyr = 1) #*#

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 6. Incorporate Efficiencies  ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~
#*# This section needs to be adjusted
PrecipSizingCostsEffic <- PrecipSizingCosts %>% #*#
  mutate(effic_N = 1, effic_P = 1) #*#

###~###~###~###~###~###~###~###~###~###~###~###~###~###~
# 7. Write Data  ------
###~###~###~###~###~###~###~###~###~###~###~###~###~###~

write.csv(PrecipSizingCostsEffic, "./Data/Representative13HUC12s_UrbanBMPs.csv")
