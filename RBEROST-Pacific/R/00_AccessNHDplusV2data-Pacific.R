########################################################################
# PURPOSE: Download NHDplusV2 data and subset to region of interest ####
# BY: Cathy Chamberlin                                              ####
# ORIGINAL DATE: 11/17/2021                                         ####
########################################################################

# Code tagged with #*# should be edited by the user before running the script

##########################################################
# 1. User Inputs                                     #####
##########################################################

filepath_loadingtargets <- 
  "./RBEROST-Pacific/Preprocessing/Inputs/01_UserSpecs_loadingtargets.csv"#*#
filepath_NHDplusV2CONUS <- 
  "C:/Users/cchamber/Documents/" #*#
# L drive filepath: "//aa/ord/NAR/DATA/PRIV/WMOST/Tier1Optimization/NHDplusV2/"
# C drive filepath: "C:/Users/cchamber/Documents/"
# Using the L drive location is very slow and necessitates being on VPN. 
# A C drive location is preferable.
filename_geopackage <- 
  "PugetSoundwatershedNHDplusV2.gpkg" #*# as NAME.gpkg

##########################################################
# 2. Setup                                           #####
##########################################################

packages <- c(
  "ggplot2", 
  "readr", 
  "dplyr", 
  "nhdplusTools", 
  "archive", 
  "sf", 
  "maps", 
  "sp", 
  "purrr",
  "rnaturalearth"#'ggmap', 'nhdplusTools', 'tidyverse', 'sf', 'maps', 'sp', 'data.table'
)
lapply(packages, library, character.only = TRUE)

theme_set(theme_classic())

##########################################################
# 2. Read in Loading Targets                         #####
##########################################################
loading.targets <- read_csv(filepath_loadingtargets)
terminal.comid <- loading.targets %>% 
  filter(TermFlag_X == "X") %>% 
  .$ComID %>% 
  unique(.)

##########################################################
# 3. Download NHD+v2 CONUS data                      #####
##########################################################
# # # Note: This should only be run once as it takes a very long time.
# download_nhdplusv2(outdir = filepath_NHDplusV2CONUS)
# archive_extract(
#   paste0(
#     filepath_NHDplusV2CONUS,
#     "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
#     ), dir = filepath_NHDplusV2CONUS
#   ) # This line of code may, or may not work. User might need to unzip through a different program.

##########################################################
# 4. Subset NHD+v2 data to geographic region         #####
##########################################################
# Note: Expect this to take over an hour
nhdpluscomid <- purrr::map(
  .x = as.character(terminal.comid), 
  .f = ~list(featureSource = "comid", featureID = .x)
)

flowline <- purrr::map(
  .x = nhdpluscomid, 
  .f = ~navigate_nldi(
    ., mode = "upstreamTributaries", distance_km = 9999
  )
)

comids <- unlist(
  purrr::map(
    .x = flowline,
    .f = ~.x$UT_flowlines$nhdplus_comid
  )
) %>%
  unique(.)

nhdplus <- subset_nhdplus(
  comids = comids,
  output_file = paste0("./Data/", filename_geopackage),
  nhdplus_data = paste0(
    filepath_NHDplusV2CONUS,
    "NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
  ),
  overwrite = TRUE,
  return_data = FALSE,
  status = TRUE,
  flowline_only = FALSE
)

st_layers(nhdplus)

#############################
# 5. Plot to verify data ####
#############################

# Read data
flowline <- read_sf(paste0("./Data/", filename_geopackage), "NHDFlowline_Network")
catchments <- read_sf(paste0("./Data/", filename_geopackage), "CatchmentSP")

basin <- purrr::map(.x = nhdpluscomid, .f = ~get_nldi_basin(.x))

sf::sf_use_s2(FALSE)
all_basins <- do.call("rbind", basin) %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf()


states <- ne_states(country = "united states of america", returnclass = "sf")

# Make points for loading targets
target.comids <- merge(
  flowline %>% select(comid = COMID, geom), 
  loading.targets %>% rename(comid = ComID), 
  all.y = TRUE
)

target.points <- cbind(
  get_node(target.comids, position = "end"), 
  as.data.frame(target.comids) %>% 
    select(comid, Waterbody_Name, TN_or_TP, TermFlag_X)
)

# Check projections
all_basins <- st_transform(all_basins, st_crs(states))
flowline <- st_transform(flowline, st_crs(states))
catchments <- st_transform(catchments, st_crs(states))
target.comids <- st_transform(target.comids, st_crs(states))

st_crs(states) == st_crs(all_basins) 
st_crs(states) == st_crs(flowline)
st_crs(states) == st_crs(catchments)
st_crs(states) == st_crs(target.comids)


# Get bounding box
sp_bbox <- bbox(as_Spatial(all_basins))

sp_bbox[,1] <- sp_bbox[,1] - 0.1 # Expand the bounding box slightly
sp_bbox[,2] <- sp_bbox[,2] + 0.2 # Expand the bounding box slightly

# Plot flowlines

ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = all_basins,
    inherit.aes = FALSE, 
    color = "black", 
    fill = "white", 
    lwd = 2
  ) + 
  geom_sf(
    data = flowline %>% filter(StreamOrde == 1), 
    inherit.aes = FALSE, color = "grey90"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 2), 
    inherit.aes = FALSE, color = "grey80"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 3), 
    inherit.aes = FALSE, color = "grey70"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 4), 
    inherit.aes = FALSE, color = "grey60"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 5), 
    inherit.aes = FALSE, color = "grey50"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 6), 
    inherit.aes = FALSE, color = "grey40"
  ) +
  geom_sf(
    data = target.points,
    mapping = aes(color = TN_or_TP, size = TN_or_TP),
    inherit.aes = FALSE,
    shape = 18
  ) +
  scale_size_manual(values = c(6, 4)) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# Plot catchments
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = all_basins, 
    inherit.aes = FALSE,
    color = "black", 
    fill = "white", 
    lwd = 2
  ) + 
  geom_sf(
    data = catchments, 
    mapping = aes(fill = AreaSqKM),
    inherit.aes = FALSE, 
    color = NA,
    lwd = 0.01
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# Use revised basin boundaries
revised_basin <- catchments %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf()

# Plot flowlines

ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = revised_basin, 
    inherit.aes = FALSE,
    color = "black",
    fill = "white",
    lwd = 2
  ) + 
  geom_sf(
    data = flowline %>% filter(StreamOrde == 1), 
    inherit.aes = FALSE, color = "grey90"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 2), 
    inherit.aes = FALSE, color = "grey80"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 3), 
    inherit.aes = FALSE, color = "grey70"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 4), 
    inherit.aes = FALSE, color = "grey60"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 5), 
    inherit.aes = FALSE, color = "grey50"
  ) +
  geom_sf(
    data = flowline %>% filter(StreamOrde == 6), 
    inherit.aes = FALSE, color = "grey40"
  ) +
  geom_sf(
    data = target.points,
    mapping = aes(color = TN_or_TP, size = TN_or_TP),
    inherit.aes = FALSE,
    shape = 18
  ) +
  scale_size_manual(values = c(6, 4)) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# Plot catchments
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = revised_basin, 
    inherit.aes = FALSE, 
    color = "black", 
    fill = "white", 
    lwd = 2
  ) + 
  geom_sf(
    data = catchments, 
    mapping = aes(fill = AreaSqKM),
    inherit.aes = FALSE, 
    color = NA,
    lwd = 0.01
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

#########################################
### 6. Get Waterbody and Area Layers ####
#########################################
# The subset_nhdplus function is not picking up the waterbody or area layers because they are not in the list of provided comids, even though they overlap geographically
# These can be gathered separately and written to the geopackage
# The all_basins area is larger than the revised_basin. It will be better to use this for this purpose so nothing gets missed.


NHDArea <- get_nhdarea(all_basins)
NHDWaterbody <- get_waterbodies(all_basins) 

# Plot waterbodies and area
ggplot() + 
  geom_sf(
    data = states, inherit.aes = FALSE, color = "black", fill = "white"
  ) +
  geom_sf(
    data = revised_basin, 
    inherit.aes = FALSE, 
    color = "black", 
    fill = "white", 
    lwd = 2
  ) + 
  geom_sf(
    data = NHDArea, 
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = NHDWaterbody, 
    inherit.aes = FALSE,
    fill = "steelblue",
    color = "steelblue4"
  ) +
  lims(x = sp_bbox[1,], y = sp_bbox[2,]) 

# add layers to the .gpkg

st_layers(paste0("./Data/", filename_geopackage))

# from nhdplusTools:
clean_bbox <- function(x) {
  if("bbox" %in% names(x) && class(x$bbox[1]) == "list") {
    x$bbox <- sapply(x$bbox, paste, collapse = ",")
  }

  return(x)
}

write_sf(
  obj = clean_bbox(NHDArea), 
  dsn = paste0("./Data/", filename_geopackage),
  layer = "NHDArea"
  )

write_sf(
  obj = clean_bbox(NHDWaterbody), 
  dsn = paste0("./Data/", filename_geopackage),
  layer = "NHDWaterbody"
  )

st_layers(paste0("./Data/", filename_geopackage))
