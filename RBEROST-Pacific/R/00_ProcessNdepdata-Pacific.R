###########################################################################################
# PURPOSE: Calculate Change in N-deposition 
# BY: Cathy Chamberlin
# DATE:  3/1/2022
###########################################################################################

# This code analysis follows work in ArcGIS to calculate the centroid of each COMID polygon, and extract the values from the 2012 & 2017 archived TDEP raster grids from those points, as well as the current 2017 and 2020 raster grids
# Lines of code tagged with #*# may need to be editted by the user.

#######################################################
# 1. Setup
#######################################################
packages <- c('sf', 'tidyverse')
# lapply(packages, install.packages) #*# # Run this line of code if packages are not installed
lapply(packages, library, character.only = TRUE)

#######################################################
# 2. Load Data
#######################################################

ndep_postarcgis <- read_sf("./Data/PugetSoundwatershed_centroids_NDEP.dbf") #*# #The relative pathway should work if code is run from the RStudio Project.

#######################################################
# 3. Format Data
#######################################################

ndep <- as.data.frame(ndep_postarcgis) %>%
  select(
    comid = FEATUREID, 
    TDEP_TN_2012_arch = ndep_2012o, 
    TDEP_TN_2017_arch = ndep_2017o,
    TDEP_TN_2017_cur = ndep_2017n,
    TDEP_TN_2020_cur = ndep_2020n
  ) %>%
  mutate(
    Change_2012_2017 = (
      (TDEP_TN_2012_arch - TDEP_TN_2017_arch) / TDEP_TN_2012_arch
    ),
    Change_arch_cur = (
      (TDEP_TN_2017_arch - TDEP_TN_2017_cur) / TDEP_TN_2017_arch
    ),
    Change_2017_2020 = (
      (TDEP_TN_2017_cur - TDEP_TN_2020_cur) / TDEP_TN_2017_cur
    ),
    AdjChange2012_2020 = Change_2012_2017 + Change_2017_2020 - Change_2012_2017 * Change_2017_2020 
    # This corrects for the fact that there was a change in methodology, so values in the current version are higher than values in the archived version
    # Because the SPARROW coefficients were regressed on the archived versions, their relative inputs are dependent on this methodology
    # Even though the current version may better reflect actual deposition, the SPARROW coefficients are only telling us how much of N deposition makes it to the stream
    # These SPARROW coefficients therefore already account for inaccuracy in the Ndeposition data during the calibration step
    # BEcause SPARROW is calibrated to the archived version, we should use estimates made with this methodology
    # However, there are no archived versions for the most recent year, 2020
    # One of the years that these methodologies overlap is 2017
    # The difference between the two versions for this year gives us an idea of how different the two methodologies are from each other.
    # To exclude the changes induced by methodology, consider only the changes between years of the same methodology
    # c1_arch = (2012arch - 2017arch) / (2012arch) and c2_cur = (2017cur - 2020cur) / 2017cur
    # In the above equations, we ideally want c2_arch = (2017arch - 2020arch) / 2017arch. We can't calculate that directly, but we can assume that c2_arch = c2_cur
    # What we *really* want is c3_arch = (2012arch - 2020arch) / 2012arch
    # Rearranging the above equations:
    # 2020arch = 2017arch - (c2_arch * 2017arch)
    # 2020arch = 2017arch - (c2_cur * 2017arch)
    # 2020arch = 2017arch - ((2017cur - 2020cur) / 2017cur) * 2017arch)
    # So now solving for c3_arch:
    # c3_arch = (2012arch - (2017arch - ((2017cur - 2020cur) / 2017cur) * 2017arch)) / 2012arch
    # c3_arch = (2012arch - (2017arch - c2_cur * 2017arch)) / 2012arch
    # c3_arach = (2012arch - 2017arch * (1 - c2_cur)) / 2012arch
    # From above, we can replace 2017arch as:
    # 2017arch = 2012arch - c1_arch * 2012arch
    # And continuing to solve for c3_arch:
    # c3_arch = (2012arch - ((2012arch - c1_arch * 2012arch) * (1 - c2_cur))) / 2012arch
    # c3_arch = (2012arch - 2012arch * (1 - c1_arch) * (1 - c2_cur)) / 2012arch
    # c3_arch = (2012arch * (1 - (1 - c1_arch) * (1 - c2_cur))) / 2012arch
    # c3_arch = 1 - (1 - c1_arch) * (1 - c2_cur)
    # c3_arch = 1 - (1 - c2_cur - c1_arch + c1_arch*c2_cur)
    # c3_arch = c2_cur + c1_arch - c1_arch * c2_cur
  ) %>%
  arrange(comid)

########################################################
# 4. Write Data
########################################################

write.csv(
  ndep, 
  file = "./RBEROST-Pacific/Preprocessing/Inputs/NdepChange_2012_2020.csv", #*# The relative pathway should work if code is run from the RStudio Project
  row.names = FALSE
)

