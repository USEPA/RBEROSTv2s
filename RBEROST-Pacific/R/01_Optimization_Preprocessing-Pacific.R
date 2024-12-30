### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PURPOSE: Develop AMPL files for WMOST scaled-up optimization model screening tool 
# BY: Cathy Chamberlin, Kelly-Anne Moffa (ICF), Hunter Parker (ICF), Sam Ennett (ICF)
# DATE: 03/14/2022
# Updated DATE: November 2024
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Based on: RBEROST-Northeast preprocessor, initially created by ICF under contract to EPA
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Notes, Introduction, and Set up -----

# General Notes:
# This Preprocessor is sourced through the RunRBEROST-Pacific.Rmd file.
# Installation of necessary packages is done through this RunRBEROST-Pacific.Rmd file as well.
# If more advanced R users would like to assess different locations/data, they would need to search for and update lines of code that end in "#*#" 

# Install and open packages

packages <- c("tidyverse", "reshape2", "data.table", "stringr", "foreach", "units")
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
  )
)

# This option prevents numeric data from being printed in scientific notation
options(scipen = 999)

# source helper functions
source("./RBEROST-Pacific/R/Optimization_HelperFunctions-Pacific.R")

### ### ### ### ### ### ### ### ### 
# PART 1: USER SPECIFICATIONS ------
### ### ### ### ### ### ### ### ###

### I. Read in user specifications ------

# BMPS specifications: If BMP_Selection field = X, retain BMP for optimization. Otherwise ignore. 

user_specs_BMPs <- fread(
  paste(InPath, "01_UserSpecs_BMPs.csv", sep = ""), data.table = FALSE
) 

### II. Read in data ------
#base target data
adjusted_targets <- read.csv(paste0(InPath,"01_UserSpecs_loadingtargets.csv")) %>% 
  select(-watershed_ID)

if (MODE=="All") {
  target_selection <- as.vector(adjusted_targets$watershed_name)
} else if (MODE=="Select") {
  target_selection <- watershed_choices
}

#terminal catchments/comids
terminal_comid <- read.csv(paste0(InPath, "01_Preprocessing_Terminal_COMID.csv")) %>% 
  dplyr::rename("terminal_comid"="comid") %>%
  mutate(
    conc=paste0(watershed_name, terminal_comid))

#all upstream/terminal catchments/comids
upstream_comid <- read.csv(paste0(InPath, "01_Preprocessing_Upstream_COMID.csv")) %>% 
  select(watershed_name, catchment_comid) %>%
  mutate(
    conc=paste0(watershed_name, catchment_comid))

#final dataframe used to pull data for the watersheds that the user wants
final_target_list <- upstream_comid %>%
  mutate(
    TerminalFlag=ifelse(conc %in% terminal_comid$conc, "X", NA)
  ) %>%
  select(-conc) %>%
  merge(., adjusted_targets, by="watershed_name", all.x=T) %>%
  filter(watershed_name %in% target_selection)

### III. Adjust loading targets based on user specified selection ------

user_specs_loadingtargets <- adjusted_targets %>%
  filter(watershed_name %in% target_selection)

### CTC edited 11/8/2024 - start

adjusted_targets_2 <- 
  read.csv(paste0(InPath,"01_UserSpecs_loadingtargets.csv")) %>%
  filter(watershed_name %in% target_selection) %>%
  mutate(Number = row_number())

write.csv(adjusted_targets_2, paste0(InPath,"01_UserSpecs_loadingtargets_selected.csv"), row.names = F)

### CTC edited - end

### IV. Establish load reduction goal ------
### Specify loading target goal. This percent reduction will be applied to total baseline loadings at specified pore point.
# Note to user: If TMDL is associated with a particular load value, calculate percent reduction required to meet target at pore point.
# load_perc_reduc is now specified in user_specs_loadingtargets

load_target_tn <- (
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN")
)$adjusted_annual_target_kgperyr

load_target_tp <- (
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP")
)$adjusted_annual_target_kgperyr

### ### ### ### ### ### ### ### ### 
# PART 2: PREPROCESS DATA INPUTS ------
### ### ### ### ### ### ### ### ### 

### I. Read raw data inputs ------

# USGS Pacific SPARROW Model Input Data, 2020: https://www.sciencebase.gov/catalog/item/5d407318e4b01d82ce8d9b3c
sparrow_in <- fread(paste(InPath,"pac_sparrow_model_input.txt",sep="")) #*#
sparrow_in$HUC_12_Rev <- str_pad(sparrow_in$huc12, width=12, pad="0")  #*#

# USGS Pacific SPARROW Model Output Data, 2020: https://www.sciencebase.gov/catalog/item/5d407318e4b01d82ce8d9b3c
sparrow_cons_out_tn_pre <- fread(  #AK: modified to call tables "_pre" to preserve original
  paste(InPath, "pac_sparrow_model_output_tn.txt",sep=""), data.table = FALSE
) #*#

sparrow_cons_out_tp_pre <- fread(
  paste(InPath, "pac_sparrow_model_output_tp.txt",sep=""), data.table = FALSE
) #*#

# 60 COMIDS are updated by PARIS - old files are retained for all other COMIDS
paris_fed <- fread(
  paste(InPath, "MUN_FED_ALL_USGS.csv",sep=""), data.table = FALSE
)  %>% 
  mutate_all(as.character) #*#

paris_state <- fread(
  paste(InPath,"MUN_STATE_ALL_USGS.csv", sep=""), data.table = FALSE
)  %>% 
  mutate_all(as.character) #*#

paris <- paris_state %>% 
  bind_rows(paris_fed) %>%
  filter(paste0(year, "_", month) %in% c("2011_10", "2011_11", "2011_12", "2012_1", "2012_2", "2012_3", "2012_4", "2012_5", "2012_6", "2011_7", "2012_8", "2012_9"),
         COMID != "None",
         TN_load_kg_month != "None") %>%
  rename(comid = COMID) %>%
  select(comid, TP_load_kg_month, TN_load_kg_month) %>%
  mutate_all(as.numeric) %>%
  group_by(comid) %>%
  filter(n() == 12) %>%
  summarize_all(sum, na.rm=T) %>%
  ungroup()

sparrow_cons_out_tp <- sparrow_cons_out_tp_pre %>%
  left_join(paris, by = c("comid")) %>%
  mutate(il_tp_wwtp = ifelse(!is.na(TP_load_kg_month), TP_load_kg_month, il_tp_wwtp)) %>% # left join replaces old with new for 60 COMIDs that have new data
  select(-TN_load_kg_month, -TP_load_kg_month)

sparrow_cons_out_tn <- sparrow_cons_out_tn_pre %>%
  left_join(paris, by = c("comid")) %>%
  mutate(il_tn_wwtp = ifelse(!is.na(TN_load_kg_month), TN_load_kg_month, il_tn_wwtp)) %>%
  select(-TN_load_kg_month, -TP_load_kg_month)

# StreamCat Land Use Data, 2011: https://www.epa.gov/national-aquatic-resource-surveys/streamcat 
# Percent Cropland Data (NLCD2011)
croplandfiles <- lapply(
  str_subset(list.files(path = InPath, full.names = TRUE), "cropland"), fread
)
names(croplandfiles) <- str_subset(list.files(path = InPath), "cropland")

# Percent Imperviousness Data (ImperviousSurfaces2011)
impervfiles <- lapply(
  str_subset(list.files(path = InPath, full.names = TRUE), "imperv"), fread
)
names(impervfiles) <- str_subset(list.files(path = InPath), "imperv")

# Preprocessed Riparian Data
riparian.loadings <- fread(paste0(InPath, "RiparianLoadings_update0425.csv"))
riparian.existingbuffer <- fread(paste0(InPath, "LengthinBuffer_2016.csv"))
riparian.efficiencies <- fread(paste0(InPath, "RiparianEfficiencies_update0425.csv"))

# Note to user: Make sure that both the variable names and associated data match the state(s) that your watershed falls within.

# Combine state-specific datasets

# Combine StreamCat land use and imperviousness datasets for associated states

cropland <- imap_dfr(
  croplandfiles, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
) %>% arrange(COMID, StateAbbrev) %>%
  drop_na()

imperv <- imap_dfr(
  impervfiles, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
) %>% arrange(COMID, StateAbbrev) %>%
  drop_na()

# Perform data modifications for all reaches

# Change all COMID columns to lower case
names(sparrow_in)[1] <- "comid"
names(cropland)[1] <- "comid"
names(imperv)[1] <- "comid"

### Specify incremental loads as six categories: Point sources, urban, roads, agricultural (which is cropland), grazing, and "other"
# Note to user: if making adjustment to SPARROW region, must identify column names that reflect agricultural, point, road, grazing, urban, and "other" 
# nutrient sources based on SPARROW regional model metadata. For example, the northeast model used the USGS metadata from the Northeast SPARROW model: 
# https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477 

### II. Specify N loads ------

temp_inc_tn <- sparrow_cons_out_tn %>% 
  select(c("comid","il_tn_scg":"il_tn_wwtp")) #*#
temp_inc_tn$in_ag <- with(temp_inc_tn, il_tn_fer) #*#
temp_inc_tn$in_urb <- with(temp_inc_tn, il_tn_urb)
temp_inc_tn$in_road <- with(temp_inc_tn, il_tn_atm) #*#
temp_inc_tn$in_poin <- with(temp_inc_tn, il_tn_wwtp)
temp_inc_tn$in_graz <- 0

# Adjust 2012 SPARROW loadings for N deposition based on percentage decreases to represent present conditions
ndep_change <- fread(paste(InPath, "NdepChange_2012_2020.csv", sep = "")) %>% 
  filter(comid == 947110128) #comid manually identified as have missing data in updated file 

ndep_change_update <- fread(paste(InPath, "NdepChange_2012_2020_update.csv", sep = "")) %>% filter(comid != 947110128) %>%
  bind_rows(ndep_change)

temp_inc_tn_rev <- merge(
  temp_inc_tn,
  ndep_change_update[, c("comid", "AdjChange2012_2020")],
  by = "comid",
  all.x = TRUE
)

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

temp_inc_tn_rev[c("il_tn_atm")] <-  temp_inc_tn_rev[c("il_tn_atm")] -
  temp_inc_tn_rev[c("il_tn_atm")] * temp_inc_tn_rev[["AdjChange2012_2020"]] #*#

temp_inc_tn_rev[,ncol(temp_inc_tn_rev)] <- NULL

temp_inc_tn_rev$in_road <- with(temp_inc_tn_rev, il_tn_atm)

temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, il_tn_scg + il_tn_spr + il_tn_ald) #*#

inc_tn <- temp_inc_tn_rev %>% 
  select(c("comid", "in_poin", "in_urb", "in_road", "in_ag", "in_graz", "in_other")) #*#

### III. Specify P loads ------

temp_inc_tp <- sparrow_cons_out_tp %>% select(c("comid","il_tp_chan":"il_tp_wwtp")) #*#
temp_inc_tp$ip_ag <- with(temp_inc_tp, il_tp_fer) #*#
temp_inc_tp$ip_urb <- with(temp_inc_tp, il_tp_urb)
temp_inc_tp$ip_poin <- with(temp_inc_tp, il_tp_wwtp)
temp_inc_tp$ip_graz <- with(temp_inc_tp, il_tp_graz)
temp_inc_tp$ip_other <- with(temp_inc_tp, il_tp_chan+il_tp_geo+il_tp_spr)
temp_inc_tp$ip_road <- 0 #*#

inc_tp <- temp_inc_tp %>% 
  select(c("comid", "ip_poin", "ip_urb", "ip_ag", "ip_graz", "ip_other", "ip_road")) #*#

# Adjust 2012 SPARROW loadings for WWTPs based on percentage decreases to represent present conditions

wwtp_rem <- fread(paste(InPath,"WWTP_BaselineRemoval.csv", sep="")) #*#

### IV. N point source loading ------
inc_tn_rev <- merge(
  inc_tn, 
  wwtp_rem[, c("COMID", "Rem_2012oroldest_2021ornewest_load_ch_TN")] %>% 
    rename(comid = COMID), 
  by = "comid", 
  all.x = TRUE
) #*#

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[c("in_poin")] <-  
  inc_tn_rev[c("in_poin")] -
  inc_tn_rev[c("in_poin")] * inc_tn_rev[["Rem_2012oroldest_2021ornewest_load_ch_TN"]] #*#

inc_tn_rev[, ncol(inc_tn_rev)] <- NULL

### V. P point source loading ------
inc_tp_rev <- merge(
  inc_tp, 
  wwtp_rem[, c("COMID", "Rem_2012oroldest_2021ornewest_load_ch_TP")] %>% 
    rename(comid = COMID), 
  by = "comid", 
  all.x = TRUE
) #*#

inc_tp_rev[is.na(inc_tp_rev)] <- 0

inc_tp_rev[c("ip_poin")] <-  
  inc_tp_rev[c("ip_poin")] -
  inc_tp_rev[c("ip_poin")] * inc_tp_rev[["Rem_2012oroldest_2021ornewest_load_ch_TP"]] #*#

inc_tp_rev[,ncol(inc_tp_rev)] <- NULL

### VI. Specify urban, road, and agricultural areas, change units from km2 to acres ------

# Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_in %>% 
  select(comid, IncAreaKm2, urban_km2 = developed11_km2) %>%
  mutate(IncAreaKm2 = set_units(IncAreaKm2, "km^2"),
         urban_km2 = set_units(urban_km2, "km^2"))

# Select percentage of incremental area that is cropland
streamcat_crop <- cropland %>% select(c("comid", "PctCrop2011Cat", "PctHay2011Cat")) #*#

temp_sparrow_area <- temp_sparrow_area %>%
  mutate(urban_ac = urban_km2 %>% set_units("acres"),
         inc_ac = IncAreaKm2 %>% set_units("acres"))

temp_area <- merge(temp_sparrow_area, streamcat_crop,by = c("comid"), all.y=TRUE) # limits dataframe to the COMIDs available in state-specific streamcat datasets
temp_area$ag_ac <- with(temp_area, (PctCrop2011Cat/100)*inc_ac) #*#
temp_area$graz_ac <- with(temp_area, (PctHay2011Cat/100)*inc_ac)

distinct_area <- temp_area %>% 
  select(c("comid","urban_ac","ag_ac", "graz_ac")) %>% 
  group_by(comid) %>%
  summarize(urban_ac = mean(urban_ac), ag_ac = mean(ag_ac), graz_ac = mean(graz_ac))

# Read in the Ag baseline BMP %
ag_bmp_baseline_perc   <- fread(paste(InPath, "USGS_AgBMP_PugetSound.csv", sep = "")) #*#

# Calculate Ag baseline BMP Area
area <- distinct_area %>%
  merge(., ag_bmp_baseline_perc, by = "comid") %>%
  mutate(ag_ac_bmpadj = ag_ac - ((TOT_IMPV12_PERC / 100) * ag_ac)) %>%
  select(-TOT_IMPV12_PERC, -ag_ac) %>%
  relocate(ag_ac_bmpadj, .after = urban_ac)

distinct_area <- distinct(area) # Removes duplicates

# Calculate road area (percent impervious)
road.area <- fread(
  paste0(InPath, "road_areas.csv"), data.table = FALSE
)
road_port <- road.area %>% select(comid, HSG, area_port) %>% distinct()

road_area_temp <- imperv %>% select(c("comid","CatAreaSqKm","PctImp2011Cat")) %>% 
  merge(., road_port, by = "comid")
road_area <- road_area_temp %>% mutate(road_sqkm = CatAreaSqKm * (PctImp2011Cat/100) * area_port)

road_area$road_sqkm <- set_units(road_area$road_sqkm, km2)
road_area$road_ac <- set_units(road_area$road_sqkm, acre)
road_area <- road_area %>%
  select(comid, HSG, road_ac) %>%
  distinct()


### VII. Specify the length of stream bank already buffered ------
RiparianBuffer_BMPs <- with(
  user_specs_BMPs, BMP[which(BMP_Category == "ripbuf"& BMP_Selection == "X")]
) 

RiparianBuffer_Widths <- with(
  user_specs_BMPs, UserSpec_width_ft[
    which(BMP_Category == "ripbuf"& BMP_Selection == "X")
  ]
) 

UserSpecs_bufferwidth_nearest <- custom.round(
  x = RiparianBuffer_Widths, breaks = c(20, 40, 60, 80, 100)
)

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_maximp <- foreach(
    i = 1:length(RiparianBuffer_BMPs), .combine = "merge"
  ) %do% {
    RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[i]
    
    riparian.existingbuffer %>%
      select(
        comid, 
        totalbanklength_ft, 
        contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      rename_with(
        .fn = ~gsub(
          paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
        ), 
        .col = contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      mutate(
        maximp = case_when(
          RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
            totalbanklength_ft - Forest_buffer,
          RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
            totalbanklength_ft - Forest_buffer - Grass_buffer
        )
      ) %>%
      mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
      select(comid, totalbanklength_ft, maximp) %>%
      rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")
  }
}

### VIII. Calculate runoff coefficient for urban area ------

# Specify column for impervious dataset
temp_runoffcoeff <- imperv %>% select(c("comid","PctImp2011Cat")) #*#
temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2011Cat
) #*#

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff"))
runoffcoeff <- as.data.frame(distinct(runoffcoeff)) # Removes duplicates

# Remove temporary datasets
rm(list = ls(pattern = "^temp"))

### IX. Limit dataframe to the COMIDs within the specified watershed ------
reaches_all <- sparrow_in %>% 
  filter(
    comid %in% final_target_list$catchment_comid
  )

reaches_TN_target <- final_target_list %>%
  filter(TN_or_TP == "TN")

reaches_TP_target <- final_target_list %>%
  filter(TN_or_TP == "TP")

# Message regarding SPARROW/StreamCat mismatch

streamcat_subset_all <- as.data.frame(runoffcoeff)[
  runoffcoeff$comid %in% 
    unique(final_target_list$catchment_comid), 
] %>%
  select(comid) %>%
  arrange(comid)

streamcat_subset_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  streamcat_subset_tn_tmp <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% watershed_comid$catchment_comid, 
  ] %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tn_tmp
  
}

streamcat_subset_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  streamcat_subset_tp_tmp <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% watershed_comid$catchment_comid, 
  ] %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tp_tmp
  
}

Message.tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  if(
    length(unique(streamcat_subset_tn[[i]]$comid)) > 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads from the remaining reaches will be included in", 
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tn[[i]]$comid)) < 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(i == 1) {"st"} else if(i == 2) {"nd"} else {"rd"}, 
        " TN target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tn, cat, sep = "\n"))
}

Message.tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if(
    length(unique(streamcat_subset_tp[[i]]$comid)) > 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tp[[i]]$comid)) < 
    length(unique(watershed_comid$catchment_comid))
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(11, 12, 13)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tp, cat, sep = "\n"))
}

### X. Determine riparian loadings ------

riparian.loadings_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, N_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tn %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>% 
        select(comid, il_tn), 
      by = "comid"
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > il_tn ~ il_tn, 
        is.na(N_riparian_kgyr) ~ 0, 
        TRUE ~ N_riparian_kgyr
      )
    ) %>%
    select(-il_tn)
}

riparian.loadings_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, P_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tp %>% 
        filter(comid %in% watershed_comid$catchment_comid) %>% 
        select(comid, il_tp), 
      by = "comid"
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > il_tp ~ il_tp, 
        is.na(P_riparian_kgyr) ~ 0, 
        TRUE ~ P_riparian_kgyr
      )
    )  %>%
    select(-il_tp)
}

### XI. Adjust delivery fraction to pore point ------
# Specify maximum delivery fraction per watershed
max_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      sparrow_cons_out_tn %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarise(max(DEL_FRAC)) 
    } else { 1 }
    
  }
)

temp_delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[i]
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>% select(c("comid", "delfrac_rev"))
  
}

max_delfrac_tp <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      
      sparrow_cons_out_tp %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarise(max(DEL_FRAC))
      
    } else { 1 } 
    
  }
)

temp_delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tp %>% filter(comid %in% watershed_comid$catchment_comid)  %>% 
    select(c("comid", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[i]
    )
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
  
  temp_delfrac_rev_tp[[i]] %>% select(c("comid", "delfrac_rev"))
  
}

### XII. Define costs ------

ft2_to_ac <- measurements::conv_unit(1, "acre", "ft2")
yd2_to_ac <- measurements::conv_unit(1, "acre", "yd2")
km2_to_ac <- measurements::conv_unit(1, "acre", "km2") 

temp_bmp_costs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X") %>%
  select(
    BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_width_ft
  )

### XIII. Grazing, ag, urban, road & riparian buffer costs ------

##### a. Convert area-specific costs to costs per acre for ag and grazing BMPs and to costs per linear foot for Riparian BMPs ------

temp_bmp_costs_ag_rev <- temp_bmp_costs %>%
  filter(BMP_Category != "point" & BMP_Category != "urban" & BMP_Category != "road") %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~as.numeric(.)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units == "ft2" ~ . * UserSpec_width_ft,
          capital_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
      ), 
      .names = "{.col}_rev"
    ),
    across(
      .cols = c(contains(c("operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          operations_units == "ft2" ~ . * ft2_to_ac,
          operations_units == "km2" ~ . * km2_to_ac,
          operations_units == "yd2" ~ . * yd2_to_ac,
          operations_units == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          operations_units == "ft2" ~ . * UserSpec_width_ft,
          operations_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          operations_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          operations_units == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
        BMP_Category == "graz" ~ case_when(
          operations_units == "ft2" ~ . * ft2_to_ac,
          operations_units == "km2" ~ . * km2_to_ac,
          operations_units == "yd2" ~ . * yd2_to_ac,
          operations_units == "ac" ~ .
        ),
      ), 
      .names = "{.col}_rev"
    )
  )

bmp_nonptsrc_costs <- temp_bmp_costs_ag_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  ) 

##### b. Specify point source costs by COMID ------

if(
  "point" %in% 
  (user_specs_BMPs %>% filter(BMP_Selection == "X") %>% .$BMP_Category)
) {
  point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) #*#
  # Note to user: Can update this file if more WWTPs desired for analysis; must also update UserSpecs_BMPs.csv with costs
  
  bmp_costs_ptsrc <- fread(paste(InPath, "WWTP_Costs.csv", sep=""))
  
  temp_bmp_costs_point <- merge(
    bmp_costs_ptsrc, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(-contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "CostType", "Costunits"), 
      names_pattern = "(.*)_(Capital|OM)_Cost_([[:digit:]]{4}dollars)",
      values_to = "Cost"
    )
  
  UserSelected_pointBMPs <- user_specs_BMPs %>%
    filter(BMP_Category == "point") %>%
    filter(BMP_Selection == "X") %>%
    mutate(
      BMP = case_when(
        BMP == "Fourstage_Bardenpho" ~ "FOURBDP",
        BMP == "Chem_add" ~ "C",
        BMP == "Denitrification_filters" ~ "DNF",
        BMP == "Tertiary_filters" ~ "F",
        BMP == "MeOH_addition" ~ "M",
        BMP == "Membrane_bioreactors" ~ "MBR",
        BMP == "Modified_Ludzack_Ettinger" ~ "MLE",
        BMP == "Sequencing_batch_reactor" ~ "SBR"
      )
    )
  
  temp_bmp_costs_point_selected <- temp_bmp_costs_point %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_costs_point$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) %>% # Annualize capital costs based on planning horizon and interest rate
    mutate(
      Cost = case_when(
        CostType == "Capital" ~ (
          Cost * (
            interest_rate * ((1 + interest_rate) ^ horizon) / 
              (((1 + interest_rate) ^ horizon) - 1)
          )
        ),
        CostType == "OM" ~ Cost
      )
    )
  
  Point_BMPs <- unique(temp_bmp_costs_point_selected$BMP)
  
  if(length(Point_BMPs) > 0) {
    print(
      paste0(
        "Based on User Selections, the following combinations of BMPs are ",
        "considered for WWTP upgrades: ", 
        paste0(Point_BMPs, collapse = ", "),
        ". FOURBDP = Fourstage_Bardenpho, C = Chem_add, ",
        "DNF = Denitrification_filters, F = Tertiary_filters, M = MeOH_addition, ",
        "MBR = Membrane_bioreactors, MLE = Modified_Ludzack_Ettinger, and ",
        "SBR = Sequencing_batch_reactor."
      )
    )
    
    bmp_costs_point <- temp_bmp_costs_point_selected
  } else {
    print(
      paste0(
        "Based on User Selections, no WWTP upgrade BMPs will be considered. ",
        "The BMPs the User has selected may only be implemented in conjunction with unselected BMPs."
      )
    )
    
    bmp_costs_point <- data.frame(
      COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
      CostType = NA, Costunits = NA, Cost = NA
    )
  }
  
} else {
  bmp_costs_point <- data.frame(
    COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
    CostType = NA, Costunits = NA, Cost = NA
  )
  
  Point_BMPs <- user_specs_BMPs %>%
    filter(BMP_Selection == "X", BMP_Category == "point") %>%
    .$BMP
} 

##### c. Specify urban source costs by COMID ------
temp_Urban_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "urban")

Urban_BMPs <- paste0(temp_Urban_BMPs$BMP)

if(
  length(Urban_BMPs) > 0
) {
  
  urban.dat <- fread(
    paste0(InPath, "/UrbanBMPData.csv"), data.table = FALSE
  )
  
  #Transform urban areas from m^2 to acres 
  urban.dat$Area <- set_units(urban.dat$Area, m^2)
  urban.dat$acre_area <- set_units(urban.dat$Area, acres)
  
  # Create proportions of urban area that are of a certain land use and HSG combo
  urban.proportions.temp <- urban.dat %>% select(Land_Use, HSG, comid, acre_area) %>% distinct() %>%
    group_by(comid) %>% mutate(urban_area = sum(acre_area)) %>% ungroup()
  
  urban.proportions <- urban.proportions.temp %>% mutate(area_port = acre_area/urban_area)
 
  # Generate urban areas using proportions and streamcat data
  urban_areas_tmp <- distinct_area %>% select(comid, urban_ac) %>% merge(., urban.proportions)
  urban_areas <- urban_areas_tmp %>% mutate(urban_ac = urban_ac*area_port) %>% select(comid, Land_Use, HSG, urban_ac) 
  urban_areas <- urban_areas %>% mutate(urban_ac = drop_units(urban_ac))
  
  #Format urban costs
  temp_bmp_costs_urban <- urban.dat %>%
    select(
      comid, Land_Use, HSG, bmp = BMP, capital = capital_2020USD, operations = operations_2020USD
    ) %>% 
  drop_na() %>%
    filter(comid %in% c(unlist(streamcat_subset_all, use.names = FALSE)))
  
  
  bmp_costs_urban <- temp_bmp_costs_urban %>% 
    filter(bmp %in% Urban_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital = (as.numeric(capital) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations = as.numeric(operations)
    )
  
} else {
  bmp_costs_urban <- data.frame(
    comid = NA, bmp = NA, capital = NA, operations = NA
  )
} 

##### d. Specify road source costs by COMID ------
temp_road_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "road")

road_BMPs <- paste0(temp_road_BMPs$BMP)

if(
  length(road_BMPs) > 0
) {
  
  road.dat <- fread(
    paste0(InPath, "/road_bmps.csv"), data.table = FALSE
  )
  
  #Formatting urban costs
  temp_bmp_costs_road <- road.dat %>%
    select(
      comid, bmp = BMP, HSG, capital = capital_2020USD, operations = operations_2020USD
    ) %>%
  drop_na() %>%
    filter(comid %in% c(unlist(streamcat_subset_all, use.names = FALSE)))
  
  
  bmp_costs_road <- temp_bmp_costs_road %>% 
    filter(bmp %in% road_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital = (as.numeric(capital) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations = as.numeric(operations)
    )
  
  if (length(bmp_costs_road$comid) == 0) {
    bmp_costs_road <- data.frame(
      comid = NA, bmp = NA, capital = NA, operations = NA
    )
  }
  
} else {
  bmp_costs_road <- data.frame(
    comid = NA, bmp = NA, capital = NA, operations = NA
  )
} 

##### e. Define the fraction of agricultural costs that reflect base payment versus actual agricultural BMP costs ------
agcost_frac <- 100/75

# Remove temporary datasets
rm(list = ls(pattern = "^temp"))


### XIV. Define efficiencies ------

##### a. Agricultural BMPs ------

# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_nonACRE.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency"
) #*#

# Read in efficiency data for ACRE database BMPs
temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#
temp_acre$bmp <- with(
  temp_acre,
  ifelse(
    Scenario=="CONSERVATION",
    "Conservation",
    ifelse(
      Scenario=="Contour Farming",
      "Contour_Farming",
      ifelse(
        Scenario=="Fert 75%",
        "Fert_75",
        ifelse(
          Scenario=="Fert 90%",
          "Fert_90",
          ifelse(
            Scenario == "Terraces and Waterway",
            "Terrace_Waterway",
            ifelse(
              Scenario == "Terraces Only",
              "Terrace_Only",
              ifelse(
                Scenario=="Waterway Only", 
                "Waterway_Only", 
                as.character(Scenario)
              )
            )
          )
        )
      )
    )
  )
) #*#


temp_acre$Scenario <- NULL
temp_acre$HUC12_Rev <- str_pad(temp_acre$HUC12, width=12, pad="0")
temp_acre$HUC10_Rev <- str_pad(temp_acre$HUC10, width=10, pad="0")
temp_acre$HUC8_Rev <- str_pad(temp_acre$HUC8, width=8, pad="0")

temp_acre_cast_tn <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12 <- temp_acre_cast_tn %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))


ACRE_BMPs <- names(temp_acre_cast_tn[, -c(1:3)])

reaches_huc12_tn <- sparrow_in %>% 
  filter(comid %in% reaches_TN_target$catchment_comid) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC12")],
  temp_acre_cast_tn_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC10")],
  temp_acre_cast_tn_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC8")],
  temp_acre_cast_tn_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn[ , ACRE_BMPs] <- NA

acre_reaches_tn <- temp_acre_reaches_tn %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

temp_acre_cast_tp <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12 <- temp_acre_cast_tp %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

reaches_huc12_tp <- sparrow_in %>% 
  filter(comid %in% reaches_TP_target$catchment_comid) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC12")],
  temp_acre_cast_tp_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC10")],
  temp_acre_cast_tp_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC8")],
  temp_acre_cast_tp_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp[ , ACRE_BMPs] <- NA

acre_reaches_tp <- temp_acre_reaches_tp %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

##### b. Combine ACRE BMPs with non-ACRE BMPs ------
temp_Ag_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X" & BMP_Category == "ag")

Ag_BMPs <- paste0(temp_Ag_BMPs$BMP)

TileAg_BMPs <- user_specs_BMPs %>% 
  filter(
    BMP_Selection == "X" & BMP_Category == "ag" & TileDrainRestricted == "X"
  ) %>%
  .$BMP

CustomBMPmissingdata <- temp_ag_effic_fert_man %>% 
  filter(grepl("CustomAgBMP", BMP) & BMP %in% Ag_BMPs) %>%
  mutate(
    datacheck = map2(.x = N_Efficiency, .y = P_Efficiency, .f = ~sum(.x, .y))
  ) %>%
  filter(is.na(datacheck))

if(nrow(CustomBMPmissingdata) != 0) {
  stop(
    paste0(
      paste0(CustomBMPmissingdata$BMP, collapse = ", "), " is/are included by the user, ",
      "however there are missing efficiency data in the file AgBMPEffic_nonACRE.csv. ",
      "Please enter the relevant TN and/or TP removal efficiencies to this file. ",
      "If only one nutrient is being analyzed, the efficiency data for the other ",
      "may be set to 0."
    )
  )
}

ag_effic_bycomid_tn <- add_column(
  acre_reaches_tn, 
  temp_ag_effic_fert_man_cast_tn %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

ag_effic_bycomid_tp <- add_column(
  acre_reaches_tp, 
  temp_ag_effic_fert_man_cast_tp %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

##### c. Read tiled rained cropland ------
pct_tiled <- fread(paste0(InPath, "/PctCroplandTileDrained_ICF24.csv"))

##### d. Grazing BMPs ------

Graz_BMPs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X", BMP_Category == "graz") %>%
  .$BMP

if(length(Graz_BMPs) > 0) {
  graz_effic_bycomid_tn <- data.frame(
    comid = sparrow_in %>% 
      filter(comid %in% reaches_TN_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tn %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  graz_effic_bycomid_tp <- data.frame(
    comid = sparrow_in %>% 
      filter(comid %in% reaches_TP_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tp %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
}


##### e. Point source BMPs ------
if(length(Point_BMPs) > 0) {
  temp_point_effic <- fread(
    paste(InPath, "WWTP_RemovalEffic.csv", sep = "")
  ) #*#
  
  temp_bmp_effic_point <- merge(
    temp_point_effic, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(-contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "Nutrient"), 
      names_pattern = "(.*)_(TN|TP)_Efficiency",
      values_to = "Efficiency"
    )
  
  temp_bmp_effic_point_selected <- temp_bmp_effic_point %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_effic_point$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) 
  
  point_effic <- temp_bmp_effic_point_selected %>%
    mutate(across(contains("Efficiency"), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  
  point_effic_bycomid_tn <- point_effic %>% 
    filter(Nutrient == "TN") %>%
    select(comid = COMID, BMP, effic = Efficiency)
  
  point_effic_bycomid_tp <- point_effic %>% 
    filter(Nutrient == "TP") %>%
    select(comid = COMID, BMP, effic = Efficiency)
  
} else {
  point_effic_bycomid_tn <- data.frame(
    comid = NA, BMP = NA, effic = NA
  )
  point_effic_bycomid_tp <- data.frame(
    comid = NA, BMP = NA, effic = NA
  )
}


##### f. Urban BMPs ------

if(length(Urban_BMPs) > 0) {
  urban_effic.n <- merge(
    data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)),
    urban.dat,
    by = "comid"
  )
  
  urban_effic_bycomid_tn <- urban_effic.n %>%
    select(comid, Land_Use, HSG, BMP, effic_N) %>%
    filter(BMP %in% Urban_BMPs) %>%
    mutate(category = "urban") %>%
    select(
      category, comid, Land_Use, HSG, bmp = BMP, effic = effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, Land_Use, HSG), names_from = bmp, values_from = effic
    ) %>%
    fill(any_of(Urban_BMPs)) %>% unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, Land_Use, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  
  urban_effic.p <- merge(
    data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)),
    urban.dat,
    by = "comid"
  )
  
  if (length(urban_effic.p$comid > 0)) {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, Land_Use, HSG, BMP, effic_P) %>%
      filter(BMP %in% Urban_BMPs) %>%
      mutate(category = "urban") %>%
      select(
        category, comid, Land_Use, HSG, bmp = BMP, effic = effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, Land_Use, HSG), names_from = bmp, values_from = effic
      ) %>%
      fill(any_of(Urban_BMPs)) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, Land_Use, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  } else {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, Land_Use, HSG)
  }
}

##### g. Road BMPs ------

if(length(road_BMPs) > 0) {
  
  road_effic.n <- road.dat %>%
    select(comid, HSG, BMP, effic_N) %>%
    filter(BMP %in% road_BMPs) %>%
    mutate(category = "road") %>%
    select(
      category, comid, HSG, bmp = BMP, effic = effic_N
    ) %>% 
    pivot_wider(
      id_cols = c(comid, HSG), names_from = bmp, values_from = effic
    ) %>%
    fill(any_of(road_BMPs)) %>% unnest(cols = 1:ncol(.)) %>% unique()
  
  
  road_effic_bycomid_tn <- merge(
    data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)), 
    road_effic.n,
    by = 'comid'
  ) %>%
    mutate(across(-c(comid, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
  
  
  road_effic.p <- road.dat %>%
    select(comid, HSG, BMP, effic_P) %>%
    filter(BMP %in% road_BMPs) %>%
    mutate(category = "road") %>%
    select(
      category, comid, HSG, bmp = BMP, effic = effic_P
    ) %>% 
    pivot_wider(
      id_cols = c(comid, HSG), names_from = bmp, values_from = effic
    ) %>%
    fill(any_of(road_BMPs)) %>% unnest(cols = 1:ncol(.)) %>% unique()
  
  
  road_effic_bycomid_tp <- merge(
    data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)), 
    road_effic.p,
    by = 'comid'
  ) %>%
    mutate(across(-c(comid, HSG), ~replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.
}

# Remove temporary datasets 
rm(list = ls(pattern = "^temp"))

### XV. Riparian buffer removals ------

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_N <- foreach(
    i = 1: length(streamcat_subset_tn)
  ) %do% {
    riparian_buffer_efficiencies_N_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("P"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_N_tmp %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
        
      )
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_P <- foreach(
    i = 1: length(streamcat_subset_tp)
  ) %do% {
    riparian_buffer_efficiencies_P_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("N"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_P_tmp %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
      )
  }
  
  riparian_buffer_removal_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    left_join(
      riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, N_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ 1e30, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
  
  riparian_buffer_removal_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    left_join(
      riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, P_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ 1e30, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
}

### XVI. Create dummy variables if modules aren't used ------
if(length(Urban_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  urban_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(road_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    road_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    road_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  road_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}


  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ripbuf_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(Ag_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ag_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(Point_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    point_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    point_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  point_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(Graz_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    graz_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    graz_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]])) %>%
        mutate(none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  graz_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

### ### ### ### ### ### ### ### ### 
# PART 3: WRITE AMPL FILES ------
### ### ### ### ### ### ### ### ### 
if(
  length(Ag_BMPs) == 0 & length(Urban_BMPs) == 0 & length(Point_BMPs) == 0 & 
  length(RiparianBuffer_BMPs) == 0 & length(Graz_BMPs) == 0
) {
  stop(
    paste0(
      "No BMPs have been selected. ",
      "Please select at least one BMP with an 'X' in the third column in the ",
      "file 01_UserSpecs_BMPs.csv."
    )
  )
} else if(length(load_target_tn) == 0 & length(load_target_tp) == 0) {
  stop(
    paste0(
      "No Loading Targets have been selected. ",
      "Please enter at least one TN or TP target into the file ",
      "01_UserSpecs_loadingtargets.csv"
    )
  )
} else {

  ### I. Develop command file ------
  
  cat(
    "#WMOST Optimization Screening Tool AMPL command file
solve;
option display_precision 10;
option presolve_warnings -1;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option display_width 100000000000;
option display_transpose -10000;
option omit_zero_rows 1;
option omit_zero_cols 1;
option display_1col 0;
display point_dec;
display urban_frac;
display road_frac;
display ag_frac;
display graz_frac;
display ripbuf_length;",
    file = paste(OutPath, "STcommand.amp", sep=""),
    sep = "\n"
  )
  
  
  # II. Develop data file ------
  
  ##### a. Write urban, land use x HSG, road, and agricultural BMPs as vectors ------

  #Urban BMP vectors
  bmp_urban_user <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & 
      (user_specs_BMPs$BMP_Category == "urban"),
  ]
  bmp_urban_user$BMP <- gsub("/", "", bmp_urban_user$BMP)
  
  bmp_urban_direct <- unique(bmp_urban_user$BMP)
  
  bmp_urban <- gsub("/", "", bmp_urban_direct)
  bmp_urban_vec_comma <- paste0("'", bmp_urban, "'")
  bmp_urban_vec_comma[-length(bmp_urban_vec_comma)] <- paste0(
    bmp_urban_vec_comma[-length(bmp_urban_vec_comma)], ','
  )
  
  bmp_urban_vec <- as.character(gsub(",", "  ", bmp_urban_vec_comma))
  bmp_urban_vec_direct <- bmp_urban
  
  pervbmp_urban_vec_comma <- paste0(
    "'", with(bmp_urban_user, BMP[which(grepl("Porous_Pavement_w_UD", BMP))]), "'"
  )
  pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)] <- paste0(
    pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)], ','
  )
  pervbmp_urban_vec <- as.character(gsub(",", "  ", pervbmp_urban_vec_comma))
  
  # Land use by HSG possibilities as vectors
  land_hsg <- urban_areas %>% mutate(land_hsg = paste0(Land_Use, "_", HSG)) %>% select(land_hsg) %>% unique()
  land_hsg <- land_hsg$land_hsg
  
  land_hsg_vec_comma <- paste0("'", land_hsg, "'")
  land_hsg_vec_comma[-length(land_hsg_vec_comma)] <- paste0(
    land_hsg_vec_comma[-length(land_hsg_vec_comma)], ','
  )
  
  land_hsg_vec <- as.character(gsub(",", "  ", land_hsg_vec_comma))
  land_hsg_vec_direct <- land_hsg
  
  # road HSG possibilities as vectors
  hsg <- c("A", "B", "C", "D", "NA")
  
  hsg_vec_comma <- paste0("'", hsg, "'")
  hsg_vec_comma[-length(hsg_vec_comma)] <- paste0(
    hsg_vec_comma[-length(hsg_vec_comma)], ','
  )
  
  hsg_vec <- as.character(gsub(",", "  ", hsg_vec_comma))
  hsg_vec_direct <- hsg
  
  # Land use as vectors
  land_use <- urban_areas %>% select(Land_Use) %>% unique()
  na_use <- data.frame(Land_Use = "NA")
  land_use <- rbind(land_use, na_use)
  land_use <- land_use$Land_Use
  
  land_use_vec_comma <- paste0("'", land_use, "'")
  land_use_vec_comma[-length(land_use_vec_comma)] <- paste0(
    land_use_vec_comma[-length(land_use_vec_comma)], ','
  )
  
  land_use_vec <- as.character(gsub(",", "  ", land_use_vec_comma))
  land_use_vec_direct <- land_use
  
  # Road BMP vectors
  bmp_road_user <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & 
      (user_specs_BMPs$BMP_Category == "road"),
  ]
  bmp_road_user$BMP <- gsub("/", "", bmp_road_user$BMP)
  
  bmp_road_direct <- unique(bmp_road_user$BMP)
  
  bmp_road <- gsub("/", "", bmp_road_direct)
  bmp_road_vec_comma <- paste0("'", bmp_road, "'")
  bmp_road_vec_comma[-length(bmp_road_vec_comma)] <- paste0(
    bmp_road_vec_comma[-length(bmp_road_vec_comma)], ','
  )
  
  bmp_road_vec <- as.character(gsub(",", "  ", bmp_road_vec_comma))
  bmp_road_vec_direct <- bmp_road
  
  # Ag BMP vectors
  bmp_ag <- user_specs_BMPs[
    user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
  ]
  bmp_ag$BMP <- gsub("/", "", bmp_ag$BMP)
  bmp_ag_vec_comma <- paste0("'", bmp_ag$BMP, "'")
  bmp_ag_vec_comma[-length(bmp_ag_vec_comma)] <- paste0(
    bmp_ag_vec_comma[-length(bmp_ag_vec_comma)], ','
  )
  
  bmp_ag_vec <- as.character(gsub(",","  ", bmp_ag_vec_comma))
  bmp_ag_vec_direct <- bmp_ag$BMP
  
  
  if (is_empty(TileAg_BMPs)) {
    tilebmp_ag_vec_comma <- vector()
    tilebmp_ag_vec <- vector()
  } else{
    tilebmp_ag_vec_comma <- paste0(
      "'", with(bmp_ag, BMP[which(TileDrainRestricted == "X")]), "'"
    )
    tilebmp_ag_vec_comma[-length(tilebmp_ag_vec_comma)] <- paste0(
      tilebmp_ag_vec_comma[-length(tilebmp_ag_vec_comma)], ','
    )
    tilebmp_ag_vec <- as.character(gsub(",", "  ", tilebmp_ag_vec_comma))
  }
  
  # Riparian buffer BMP vectors
  bmp_ripbuf_vec_comma <- paste0("'", gsub("/", "",RiparianBuffer_BMPs), "'")
  bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)] <- paste0(
    bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)], ','
  )
  bmp_ripbuf_vec <- as.character(gsub(",","  ", bmp_ripbuf_vec_comma))
  
  # Point BMP vectors
  bmp_point_vec_comma <- paste0("'", gsub("/", "",Point_BMPs), "'")
  bmp_point_vec_comma[-length(bmp_point_vec_comma)] <- paste0(
    bmp_point_vec_comma[-length(bmp_point_vec_comma)], ','
  )
  bmp_point_vec <- as.character(gsub(",","  ", bmp_point_vec_comma))
  
  #Grazing BMP vectors
  bmp_graz_vec_comma <- paste0("'", gsub("/", "", Graz_BMPs), "'")
  bmp_graz_vec_comma[-length(bmp_graz_vec_comma)] <- paste0(
    bmp_graz_vec_comma[-length(bmp_graz_vec_comma)], ','
  )
  bmp_graz_vec <- as.character(gsub(",","  ", bmp_graz_vec_comma))
  
  ##### b. Subset dataframes to reaches in the analysis ------

  
  temp_area_dat <- distinct_area[distinct_area$comid %in% reaches_all$comid,]
  temp_area_dat <- temp_area_dat[order(temp_area_dat$comid),]
  temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
  area_dat <- temp_area_dat %>% select(c("comid_form", "urban_ac", "ag_ac_bmpadj", "graz_ac"))
  
  temp_urban_area <- urban_areas[urban_areas$comid %in% reaches_all$comid,]
  temp_urban_area <- temp_urban_area[order(temp_urban_area$comid),]
  temp_urban_area <- temp_urban_area %>% mutate(comid_combo = paste0(comid, "_", Land_Use, "_", HSG)) %>%
    mutate(combo_form = paste0("'", comid_combo, "'"))
  temp_urban_area$comid_form <- paste0("'", temp_urban_area$comid, "'")
  urban_area_dat <- temp_urban_area %>% select(combo_form, urban_ac)
  
  urban_crosswalk <- temp_urban_area %>% select(combo_form, comid_form)
  
  temp_road_area <- road_area[road_area$comid %in% bmp_costs_road$comid,]
  temp_road_area <- temp_road_area[order(temp_road_area$comid),]
  temp_road_area <- temp_road_area %>% mutate(comid_combo = paste0(comid, "_", HSG)) %>%
    mutate(combo_form = paste0("'", comid_combo, "'"))
  temp_road_area$comid_form <- paste0("'", temp_road_area$comid, "'")
  road_area_dat <- temp_road_area %>% select(combo_form, road_ac)
  
  if(length(RiparianBuffer_BMPs) > 0) {
    streambanklength_total_dat <- riparian_buffer_maximp %>% 
      full_join(., streamcat_subset_all, by = "comid") %>%
      filter(comid %in% streamcat_subset_all$comid) %>%
      arrange(comid) %>%
      mutate(
        comid_form = paste0("'", comid, "'"), 
        totalbanklength_ft = case_when(
          is.na(totalbanklength_ft) ~ 0, 
          !is.na(totalbanklength_ft) ~ totalbanklength_ft
        )
      ) %>%
      select(comid_form, totalbanklength_ft)
    
    streambanklength_available_dat <-  riparian_buffer_maximp %>% 
      full_join(., streamcat_subset_all, by = "comid") %>%
      filter(comid %in% streamcat_subset_all$comid) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fns = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .)
        )
      ) %>%
      select(comid_form, any_of(RiparianBuffer_BMPs))
  }
  
  temp_runoffcoeff_dat <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% streamcat_subset_all$comid,
  ]
  temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
  temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
  runoffcoeff_dat <- temp_runoffcoeff_dat %>% 
    select(c("comid_form", "runoffcoeff"))
  

  ##### c. Specify state COMIDs ------

  COMID_State <- distinct(
    imperv[
      imperv$comid %in% streamcat_subset_all$comid, c("comid", "StateAbbrev")
    ]
  ) %>% 
    group_by(comid) %>%
    arrange(StateAbbrev) %>%
    summarize(State = StateAbbrev[1]) #*#
  

  ##### d. Multiply all loads by revised del_frac ------
  
  temp_inc_tn_dat <- foreach(i = 1:length(delfrac_rev_tn)) %do% {
    
    temp_inc_tn_dat_tmp <- merge(
      inc_tn_rev[inc_tn_rev$comid %in% delfrac_rev_tn[[i]]$comid,],
      delfrac_rev_tn[[i]],
      by = "comid",
      all.x = TRUE
    ) %>%
      unique() # Remove duplicates
    
    temp_inc_tn_dat_tmp[-c(1,7)] <- temp_inc_tn_dat_tmp[-c(1, 7)] * 
      temp_inc_tn_dat_tmp[["delfrac_rev"]]
    temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
      order(temp_inc_tn_dat_tmp$comid), 
    ]
    
  }
  
  temp_inc_tp_dat <- foreach(i = 1:length(delfrac_rev_tp)) %do% {
    
    temp_inc_tp_dat_tmp <- merge(
      inc_tp[inc_tp$comid %in% delfrac_rev_tp[[i]]$comid,],
      delfrac_rev_tp[[i]],
      by = "comid",
      all.x = TRUE
    ) %>%
      unique() # Remove duplicates
    
    temp_inc_tp_dat_tmp[-c(1,7)] <- temp_inc_tp_dat_tmp[-c(1,7)] * 
      temp_inc_tp_dat_tmp[["delfrac_rev"]]
    temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
      order(temp_inc_tp_dat_tmp$comid), 
    ]
    
  }
  
  if(length(RiparianBuffer_BMPs) > 0) {
    
    temp_riparian_tn_dat <- foreach(i = 1:length(riparian.loadings_tn)) %do% {
      
      temp_riparian_tn_dat_tmp <- merge(
        riparian.loadings_tn[[i]],
        delfrac_rev_tn[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        mutate(in_riparian = N_riparian_kgyr * delfrac_rev) %>%
        arrange(comid) %>%
        select(comid, in_riparian) %>% unique()
      
    }
    
    temp_riparian_tp_dat <- foreach(i = 1:length(riparian.loadings_tp)) %do% {
      
      temp_riparian_tp_dat_tmp <- merge(
        riparian.loadings_tp[[i]],
        delfrac_rev_tp[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        mutate(ip_riparian = P_riparian_kgyr * delfrac_rev) %>%
        arrange(comid) %>%
        select(comid, ip_riparian) %>% unique()
      
    }
    
    temp_riparian_tn_removal <- foreach(i = 1:length(riparian_buffer_removal_N)) %do% {
      
      temp_riparian_tn_removal_tmp <- merge(
        riparian_buffer_removal_N[[i]],
        delfrac_rev_tn[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        mutate(
          across(
            .col = any_of(RiparianBuffer_BMPs), 
            .fn =  ~case_when(is.na(.) ~ 0, !is.na(.) ~ . * delfrac_rev) # Set to 0 for missing values
          )
        ) %>%
        arrange(comid) %>%
        select(comid, any_of(RiparianBuffer_BMPs))
      
    }
    
    temp_riparian_tp_removal <- foreach(i = 1:length(riparian_buffer_removal_P)) %do% {
      
      temp_riparian_tp_removal_tmp <- merge(
        riparian_buffer_removal_P[[i]],
        delfrac_rev_tp[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        mutate(
          across(
            .col = any_of(RiparianBuffer_BMPs), 
            .fn =  ~case_when(is.na(.) ~ 0, !is.na(.) ~ . * delfrac_rev)
          )
        ) %>%
        arrange(comid) %>%
        select(comid, any_of(RiparianBuffer_BMPs))
      
    }
  }
  
  ##### e. Specify data to COMIDs within/ not within StreamCat Dataset ------

  temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    temp_inc_tn_dat[[i]][
      temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid, 
    ]
  }
  
  temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
    temp_inc_tn_dat[[i]][
      !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid), 
    ]
  }
  
  temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    temp_inc_tp_dat[[i]][
      temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
    ]
  }
  
  temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
    temp_inc_tp_dat[[i]][
      !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
    ]
  }
  

##### f. Calculate sums of loads for optimization ------
  
  # Define list of TN and TP targets for use in setting the target numerals
  param_loads_lim <- user_specs_loadingtargets %>% select(TN_or_TP, adjusted_annual_target_kgperyr)
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    param_loads_lim_tn <- user_specs_loadingtargets %>% filter(TN_or_TP == "TN") %>% pull(adjusted_annual_target_kgperyr)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    param_loads_lim_tp <- user_specs_loadingtargets %>% filter(TN_or_TP == "TP") %>% pull(adjusted_annual_target_kgperyr)
  }
  
  param_other_loads_tn <- unlist(
    foreach(i = 1:length(temp_inc_tn_dat_other)) %do% {
      sum(temp_inc_tn_dat_SC[[i]]$in_other) + sum(
        if(nrow(temp_inc_tn_dat_other[[i]]) > 0) {
          temp_inc_tn_dat_other[[i]][
            , c("in_poin", "in_urb", "in_road", "in_ag", "in_graz", "in_other")
          ] 
        } else {0}
      )
    }
  )
  
  param_other_loads_tp <- unlist(
    foreach(i = 1:length(temp_inc_tp_dat_other)) %do% {
      sum(temp_inc_tp_dat_SC[[i]]$ip_other) + sum(
        if(nrow(temp_inc_tp_dat_other[[i]]) > 0) {
          temp_inc_tp_dat_other[[i]][
            , c("ip_poin", "ip_urb", "ip_ag", "ip_graz", "ip_other", "ip_road")
          ] 
        } else {0}
      )
    }
  )
  

  ##### g. Format data  ------
  
    # Format loadings data 
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    inc_tn_dat <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_opt %>% mutate(comid_form = paste0("'", temp_inc_tn_dat_opt$comid, "'")) %>% 
        select(c("comid_form", "in_poin", "in_urb", "in_road", "in_ag", "in_graz"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    inc_tp_dat <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_opt %>% mutate(comid_form = paste0(
        "'", temp_inc_tp_dat_opt$comid, "'"
      )) %>% 
        select(c("comid_form", "ip_poin", "ip_urb", "ip_ag", "ip_graz", "ip_road"))
    }
  }
  
  if(length(RiparianBuffer_BMPs) > 0) {
    riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
      
      temp_riparian_tn_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, in_riparian)
      
    }
    
    riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
      
      temp_riparian_tp_dat[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, ip_riparian)
      
    }
    
    riparian_tn_removal <- foreach(i = 1:length(temp_riparian_tn_removal)) %do% {
      
      temp_riparian_tn_removal[[i]] %>%
        filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, any_of(RiparianBuffer_BMPs))
      
    }
    
    riparian_tp_removal <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
      
      temp_riparian_tp_removal[[i]] %>%
        filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>%
        select(comid_form, any_of(RiparianBuffer_BMPs))
      
    }
  }

  # Format urban BMP costs
  
  if(length(Urban_BMPs) > 0) {
    
    print(
      paste0(
        "Urban costs are assigned based on landuse and hydrologic soil group in each comid. They are calculated using specifications that meet design standards. Urban BMPs with no cost information available are displayed as (1 x e12) to deter the NEOS server from selecting BMPs with missing data."
      )
    )
    urban_costs_dat <- bmp_costs_urban %>%
      merge(
        ., 
        streamcat_subset_all,
        by = c("comid")
      ) %>% unique()
    
    urban_costs_dat$comid_form  <- paste0("'", urban_costs_dat$comid, "_", urban_costs_dat$Land_Use, "_", urban_costs_dat$HSG, "'") 
    urban_costs_dat$land_form  <- paste0("'", urban_costs_dat$Land_Use, "'") 
    urban_costs_dat$hsg_form  <- paste0("'", urban_costs_dat$HSG, "'") 
    
    
    # Calculate average urban capital and operations costs
    mean_urban_bmp_capital_cost <- urban_costs_dat %>%
      group_by(bmp) %>%
      summarise(mean_cap_cost = mean(capital)) %>%
      pivot_wider(names_from = bmp, values_from = c(mean_cap_cost)) %>%
      rename_with(~paste0(., "_mean"))
    
    mean_urban_bmp_operations_cost <- urban_costs_dat %>%
      group_by(bmp) %>%
      summarise(mean_op_cost = mean(operations)) %>%
      pivot_wider(names_from = bmp, values_from = c(mean_op_cost)) %>%
      rename_with(~paste0(., "_mean"))
    
    urban_costs_capital_dat <- urban_costs_dat %>% 
      pivot_wider(id_cols = c(comid_form), names_from = bmp, values_from = c(capital)) %>%
      cbind(., mean_urban_bmp_capital_cost) %>%
      # Replace NA values with the average cost within the model to avoid AMPL errors
      mutate(across(any_of(Urban_BMPs), ~map2(., mean_urban_bmp_capital_cost[grepl(cur_column(), names(mean_urban_bmp_capital_cost))], ~replace_na(.x, .y)))) %>%
      unique() %>%
      unnest(cols = 1:ncol(.)) %>%
      select(comid_form, any_of(bmp_urban_direct))
    
    #Fill in all BMPs if some are dropped due to no data
    bmp_cols <- urban_costs_capital_dat %>%
      select(!comid_form)
    
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        if (nrow(urban_costs_capital_dat) > 0) {
          urban_costs_capital_dat[[col]] <- 1e30
        } else {
          urban_costs_capital_dat[[col]] <- vector("numeric", length = 0)
        }
      }
    }
    
    urban_costs_capital_dat <- urban_costs_capital_dat %>%
      mutate(across(any_of(Urban_BMPs), ~replace_na(., 1e30))) %>%
      unique()
    
    urban_costs_operations_dat <- urban_costs_dat %>%
      pivot_wider(id_cols = c(comid_form), names_from = bmp, values_from = operations) %>%
      cbind(., mean_urban_bmp_operations_cost) %>%
      unnest(cols = 1:ncol(.)) %>%
      # Replace NA values with the average cost within the model to avoid AMPL errors
      mutate(across(any_of(Urban_BMPs), ~map2(., mean_urban_bmp_operations_cost[grepl(cur_column(), names(mean_urban_bmp_operations_cost))], ~replace_na(.x, .y)))) %>%
      # mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # Replace NA values with 0, as no NA value available in AMPL
      unique() %>%
      select(comid_form, any_of(bmp_urban_direct))
    
    #Fill in all BMPs if some are dropped due to no data
    bmp_cols <- urban_costs_operations_dat %>%
      select(!comid_form)
    
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        if (nrow(urban_costs_operations_dat) > 0) {
          urban_costs_operations_dat[[col]] <- 1e30
        } else {
          urban_costs_operations_dat[[col]] <- vector("numeric", length = 0)
        }
      }
    }
    
    urban_costs_operations_dat <- urban_costs_operations_dat %>%
      mutate(across(any_of(Urban_BMPs), ~replace_na(., 1e30))) %>%
      unique()
  }
  
  # Format road BMP costs
  
  if(length(road_BMPs) > 0) {
    
    road_costs_dat <- bmp_costs_road %>%
      merge(
        ., 
        streamcat_subset_all, 
        by = c("comid")
      ) 
    
    if (length(road_costs_dat$comid) > 0) {
      road_costs_dat$comid_form  <- paste0("'", road_costs_dat$comid, "'")
      road_costs_dat$combo_form  <- paste0("'", road_costs_dat$comid, "_", road_costs_dat$HSG, "'")
      road_costs_dat$hsg_form <- paste0("'", road_costs_dat$HSG, "'")
    } else {
      road_costs_dat$comid_form <- vector("character", length = 0)
      road_costs_dat$combo_form  <- vector("character", length = 0)
    }

    
    # road_costs_dat <- road_costs_dat %>% mutate(
    #   capital = case_when(hsg_form == "'NA'" ~ 1e30, TRUE ~ capital),
    #   operations = case_when(hsg_form == "'NA'" ~ 1e30, TRUE ~ operations),
    #   bmp = case_when(is.na(bmp) ~ "None", TRUE ~ bmp)
    # )

    # road_costs_capital_dat <- road_costs_dat %>%
    #   pivot_wider(id_cols = c(combo_form), names_from = bmp, values_from = capital) %>%
    #   unnest(cols = 1:ncol(.)) %>%
    #   mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    #   unique()
    # 
    # road_costs_operations_dat <- road_costs_dat %>% 
    #   pivot_wider(id_cols = c(combo_form), names_from = bmp, values_from = operations) %>%
    #   unnest(cols = 1:ncol(.)) %>%
    #   mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    #   unique()
    
    road_crosswalk <- road_costs_dat %>% select(combo_form, comid_form)
    
    # Calculate average urban capital and operations costs
    if (length(road_costs_dat$comid) > 0) {
      mean_road_bmp_capital_cost <- road_costs_dat %>%
        group_by(bmp) %>%
        summarise(mean_cap_cost = mean(capital)) %>%
        pivot_wider(names_from = bmp, values_from = c(mean_cap_cost)) %>%
        rename_with(~paste0(., "_mean"))
      
      mean_road_bmp_operations_cost <- road_costs_dat %>%
        group_by(bmp) %>%
        summarise(mean_op_cost = mean(operations)) %>%
        pivot_wider(names_from = bmp, values_from = c(mean_op_cost)) %>%
        rename_with(~paste0(., "_mean"))
      
      road_costs_capital_dat <- road_costs_dat %>% 
        pivot_wider(id_cols = c(combo_form), names_from = bmp, values_from = c(capital)) %>%
        cbind(., mean_road_bmp_capital_cost) %>%
        # Replace NA values with the average cost within the model to avoid AMPL errors
        mutate(across(any_of(road_BMPs), ~map2(., mean_road_bmp_capital_cost[grepl(cur_column(), names(mean_road_bmp_capital_cost))], ~replace_na(.x, .y)))) %>%
        unique() %>%
        unnest(cols = 1:ncol(.)) %>%
        select(combo_form, any_of(road_BMPs))
      
      #Fill in all BMPs if some are dropped due to no data
      bmp_cols <- road_costs_capital_dat %>%
        select(!combo_form)
      
      missing_columns <- setdiff(road_BMPs, names(bmp_cols))
      
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          if (nrow(road_costs_capital_dat) > 0) {
            road_costs_capital_dat[[col]] <- 1e30
          } else {
            road_costs_capital_dat[[col]] <- vector("numeric", length = 0)
          }
        }
      }
      
      road_costs_capital_dat <- road_costs_capital_dat %>%
        mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>%
        unique()
      
      road_costs_operations_dat <- road_costs_dat %>%
        pivot_wider(id_cols = c(combo_form), names_from = bmp, values_from = operations) %>%
        cbind(., mean_road_bmp_operations_cost) %>%
        unnest(cols = 1:ncol(.)) %>%
        # Replace NA values with the average cost within the model to avoid AMPL errors
        mutate(across(any_of(road_BMPs), ~map2(., mean_road_bmp_operations_cost[grepl(cur_column(), names(mean_road_bmp_operations_cost))], ~replace_na(.x, .y)))) %>%
        # mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # Replace NA values with 0, as no NA value available in AMPL
        unique() %>%
        select(combo_form, any_of(road_BMPs)) %>%
        mutate(across(any_of(road_BMPs), as.numeric))
      
      #Fill in all BMPs if some are dropped due to no data
      bmp_cols <- road_costs_operations_dat %>%
        select(!combo_form)
      
      missing_columns <- setdiff(road_BMPs, names(bmp_cols))
      
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          if (nrow(road_costs_operations_dat) > 0) {
            road_costs_operations_dat[[col]] <- 1e30
          } else {
            road_costs_operations_dat[[col]] <- vector("numeric", length = 0)
          }
        }
      }
      
      road_costs_operations_dat <- road_costs_operations_dat %>%
        mutate(across(any_of(road_BMPs), ~replace_na(., 1e30))) %>%
        unique()
      
    } else {
      road_costs_capital_dat <- road_costs_dat %>%
        select(combo_form)
      for (bmp in road_BMPs) {
        road_costs_capital_dat[[bmp]] <- vector("numeric", length = 0)
      }
      
      road_costs_operations_dat <- road_costs_dat %>%
        select(combo_form)
      for (bmp in road_BMPs) {
        road_costs_operations_dat[[bmp]] <- vector("numeric", length = 0)
      }
    }
  }
  
  
  # Separate parameters for ag_capital and ag_operations and format costs data
  
  if(length(Ag_BMPs) > 0) {
    
    temp_bmp_costs_ag <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "ag") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_ag$capital)) | 
      any(is.na(temp_bmp_costs_ag$operations))
    ) {
      stop(
        paste0(
          "Ag ",
          if(
            any(is.na(temp_bmp_costs_ag$capital)) & 
            any(is.na(temp_bmp_costs_ag$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_ag$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_ag %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_ag %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_ag_capital <- reshape2::dcast(
      temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    bmp_costs_ag_operations <- reshape2::dcast(
      temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_ag_capital_rev <- bmp_costs_ag_capital[
      order(bmp_costs_ag_capital$comid),
    ]
    ag_costs_cap_dat <- bmp_costs_ag_capital_rev[
      ,names(bmp_costs_ag_capital_rev) != "comid"
    ]
    ag_costs_cap_dat <- ag_costs_cap_dat %>% select(comid_form, everything())
    
    bmp_costs_ag_operations_rev <- bmp_costs_ag_operations[
      order(bmp_costs_ag_operations$comid),
    ]
    ag_costs_op_dat <- bmp_costs_ag_operations_rev[ 
      ,names(bmp_costs_ag_operations_rev) != "comid"
    ]
    ag_costs_op_dat <- ag_costs_op_dat %>% select(comid_form, everything())
  }
  
  # Format riparian BMP costs
  
  if(length(RiparianBuffer_BMPs) > 0) {
    temp_bmp_costs_ripbuf <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "ripbuf") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_ripbuf$capital)) | 
      any(is.na(temp_bmp_costs_ripbuf$operations))
    ) {
      stop(
        paste0(
          "Riparian Buffer ",
          if(
            any(is.na(temp_bmp_costs_ripbuf$capital)) & 
            any(is.na(temp_bmp_costs_ripbuf$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_ripbuf$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_ripbuf %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_ripbuf %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_ripbuf_capital <- reshape2::dcast(
      temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    
    bmp_costs_ripbuf_operations <- reshape2::dcast(
      temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_ripbuf_capital_rev <- bmp_costs_ripbuf_capital[
      order(bmp_costs_ripbuf_capital$comid),
    ]
    ripbuf_costs_cap_dat <- bmp_costs_ripbuf_capital_rev[
      ,names(bmp_costs_ripbuf_capital_rev) != "comid"
    ]
    ripbuf_costs_cap_dat <- ripbuf_costs_cap_dat %>% 
      select(comid_form, everything())
    
    bmp_costs_ripbuf_operations_rev <- bmp_costs_ripbuf_operations[
      order(bmp_costs_ripbuf_operations$comid),
    ]
    ripbuf_costs_op_dat <- bmp_costs_ripbuf_operations_rev[ 
      ,names(bmp_costs_ripbuf_operations_rev) != "comid"
    ]
    ripbuf_costs_op_dat <- ripbuf_costs_op_dat %>% 
      select(comid_form, everything())
    
  }
  
  
  # Format point source BMP efficiency data
  
  if(length(Point_BMPs) > 0) {
    
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      temp_point_effic_dat_tn <- point_effic_bycomid_tn %>%
        filter(comid %in% unlist(streamcat_subset_tn)) %>% 
        pivot_wider(id_cols = comid, names_from = BMP, values_from = effic) %>%
        select(comid, any_of(Point_BMPs)) # forces the order
      
      temp_point_effic_other_tn <- data.frame(
        comid = unlist(
          streamcat_subset_tn, use.names = FALSE
        )[
          !(
            unlist(streamcat_subset_tn, use.names = FALSE) %in% 
              temp_point_effic_dat_tn$comid
          )
        ]
      )
      
      temp_point_effic_other_tn[2:(1+length(Point_BMPs))] <- 0
      names(temp_point_effic_other_tn) <- c("comid", Point_BMPs)
      
      if(nrow(temp_point_effic_dat_tn) > 0) {
        temp_point_effic_dat_rev_tn <- rbind(
          temp_point_effic_dat_tn, temp_point_effic_other_tn
        ) %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        temp_point_effic_dat_rev_tn <- temp_point_effic_other_tn %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      }
      
      point_effic_dat_tn <- temp_point_effic_dat_rev_tn %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        unique() # function used to ensure duplicates don't creep in
    }
    
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      
      temp_point_effic_dat_tp <- point_effic_bycomid_tp %>%
        filter(comid %in% unlist(streamcat_subset_tp)) %>% 
        pivot_wider(id_cols = comid, names_from = BMP, values_from = effic) %>%
        select(comid, any_of(Point_BMPs)) # forces the order
      
      temp_point_effic_other_tp <- data.frame(
        comid = unlist(
          streamcat_subset_tp, use.names = FALSE
        )[
          !(
            unlist(streamcat_subset_tp, use.names = FALSE) %in% 
              temp_point_effic_dat_tp$comid
          )
        ]
      )
      
      temp_point_effic_other_tp[2:(1+length(Point_BMPs))] <- 0
      names(temp_point_effic_other_tp) <- c("comid", Point_BMPs)
      
      if(nrow(temp_point_effic_dat_tp) > 0) {
        temp_point_effic_dat_rev_tp <- rbind(
          temp_point_effic_dat_tp, temp_point_effic_other_tp
        ) %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        temp_point_effic_dat_rev_tp <- temp_point_effic_other_tp %>% 
          arrange(comid) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      }
      
      point_effic_dat_tp <- temp_point_effic_dat_rev_tp %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        unique() # function used to ensure duplicates don't creep in
    }
  }
  # Format grazing BMP costs
  
  if(length(Graz_BMPs) > 0) {
    temp_bmp_costs_graz <- merge(
      bmp_nonptsrc_costs %>%
        filter(category == "graz") %>%
        select(
          bmp, contains(c("capital", "operations"))
        ),
      COMID_State
    ) %>%
      mutate(
        capital = my_key_fun(., "State", ~paste0("capital_", .x)),
        operations = my_key_fun(., "State", ~paste0("operations_", .x))
      ) %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    if(
      any(is.na(temp_bmp_costs_graz$capital)) | 
      any(is.na(temp_bmp_costs_graz$operations))
    ) {
      stop(
        paste0(
          "Grazing ",
          if(
            any(is.na(temp_bmp_costs_graz$capital)) & 
            any(is.na(temp_bmp_costs_graz$operations))
          ) {paste("capital and operations ")} else if (
            any(is.na(temp_bmp_costs_graz$capital))
          ) {paste("capital ")} else {paste("operations ")},
          "costs and/or units for ", 
          paste(
            temp_bmp_costs_graz %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(bmp) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " in ",
          paste(
            temp_bmp_costs_graz %>% 
              filter(is.na(capital) | is.na(operations)) %>% 
              select(State) %>% 
              unique() %>%
              pull(), 
            collapse = ", "
          ),
          " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`, and that units are one of: ft2, km2, yd2, ac."
        )
      )
    }
    
    bmp_costs_graz_capital <- reshape2::dcast(
      temp_bmp_costs_graz[, c("comid", "comid_form", "bmp", "capital")], 
      comid_form + comid ~ bmp,
      value.var = "capital"
    )
    
    bmp_costs_graz_operations <- reshape2::dcast(
      temp_bmp_costs_graz[, c("comid", "comid_form", "bmp", "operations")],
      comid_form + comid ~ bmp,
      value.var = "operations"
    )
    
    bmp_costs_graz_capital_rev <- bmp_costs_graz_capital[
      order(bmp_costs_graz_capital$comid),
    ]
    graz_costs_cap_dat <- bmp_costs_graz_capital_rev[
      ,names(bmp_costs_graz_capital_rev) != "comid"
    ]
    graz_costs_cap_dat <- graz_costs_cap_dat %>% 
      select(comid_form, everything())
    
    bmp_costs_graz_operations_rev <- bmp_costs_graz_operations[
      order(bmp_costs_graz_operations$comid),
    ]
    graz_costs_op_dat <- bmp_costs_graz_operations_rev[ 
      ,names(bmp_costs_graz_operations_rev) != "comid"
    ]
    graz_costs_op_dat <- graz_costs_op_dat %>% 
      select(comid_form, everything())
    
  }
  
  # Format agricultural BMP efficiency data
  
  if(length(Ag_BMPs) > 0) {
    temp_ag_effic_dat_tn <- ag_effic_bycomid_tn[
      ag_effic_bycomid_tn$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    ag_effic_dat_tn <- temp_ag_effic_dat_tn %>% 
      select(-comid) %>% 
      select(comid_form, everything())
    
    temp_ag_effic_dat_tp <- ag_effic_bycomid_tp[
      ag_effic_bycomid_tp$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    ag_effic_dat_tp <- temp_ag_effic_dat_tp %>% 
      select(-comid) %>% 
      select(comid_form, everything())
  }
  
  # Format grazing BMP efficiency data
  
  if(length(Graz_BMPs) > 0) {
    temp_graz_effic_dat_tn <- graz_effic_bycomid_tn[
      graz_effic_bycomid_tn$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    graz_effic_dat_tn <- temp_graz_effic_dat_tn %>% 
      select(-comid) %>% 
      select(comid_form, everything())
    
    temp_graz_effic_dat_tp <- graz_effic_bycomid_tp[
      graz_effic_bycomid_tp$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    graz_effic_dat_tp <- temp_graz_effic_dat_tp %>% 
      select(-comid) %>% 
      select(comid_form, everything())
  }
  
  # Format urban BMP efficiency data
  
  if(length(Urban_BMPs) > 0) {
    temp_urban_effic_dat_tn <- urban_effic_bycomid_tn[
      urban_effic_bycomid_tn$comid %in% 
        unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "_", Land_Use, "_", HSG, "'"))
    
    urban_effic_dat_tn <- temp_urban_effic_dat_tn %>% 
      select(-comid, -Land_Use, -HSG) %>% 
      select(comid_form, everything()) 
    
    temp_urban_effic_dat_tp <- urban_effic_bycomid_tp[
      urban_effic_bycomid_tp$comid %in% 
        unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "_", Land_Use, "_", HSG, "'"))
    
    urban_effic_dat_tp <- temp_urban_effic_dat_tp %>% 
      select(-comid, -Land_Use, -HSG) %>%
      select(comid_form, everything())
    
    # Add in missing BMP columns for TN
    bmp_cols <- urban_effic_dat_tn %>%
      select(!comid_form)
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(urban_effic_dat_tn) > 0) {
        urban_effic_dat_tn[[col]] <- 0
      } else {
        urban_effic_dat_tn[[col]] <- vector("numeric", length = 0)
      }
    }

    # Add in missing BMP columns for TP    
    bmp_cols <- urban_effic_dat_tp %>%
      select(!comid_form)
    missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(urban_effic_dat_tp) > 0) {
        urban_effic_dat_tp[[col]] <- 0
      } else {
        urban_effic_dat_tp[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  # Format road BMP efficiency data
  
  if(length(road_BMPs) > 0) {
    temp_road_effic_dat_tn <- road_effic_bycomid_tn[
      road_effic_bycomid_tn$comid %in% 
        unlist(streamcat_subset_tn, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "_", HSG, "'"))
    
    road_effic_dat_tn <- temp_road_effic_dat_tn %>% 
      select(-comid, -HSG) %>% 
      select(comid_form, everything())
    
    temp_road_effic_dat_tp <- road_effic_bycomid_tp[
      road_effic_bycomid_tp$comid %in% 
        unlist(streamcat_subset_tp, use.names = FALSE),
    ] %>%
      arrange(comid) %>%
      mutate(comid_form = paste0("'", comid, "_", HSG, "'"))
    
    road_effic_dat_tp <- temp_road_effic_dat_tp %>% 
      select(-comid, -HSG) %>% 
      select(comid_form, everything())

    # Add in missing BMP columns for TN
    bmp_cols <- road_effic_dat_tn %>%
      select(!comid_form)
    missing_columns <- setdiff(road_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(road_effic_dat_tn) > 0) {
        road_effic_dat_tn[[col]] <- 0
      } else {
        road_effic_dat_tn[[col]] <- vector("numeric", length = 0)
      }
    }
    
    # Add in missing BMP columns for TP    
    bmp_cols <- road_effic_dat_tp %>%
      select(!comid_form)
    missing_columns <- setdiff(road_BMPs, names(bmp_cols))
    
    for (col in missing_columns) {
      if (nrow(road_effic_dat_tp) > 0) {
        road_effic_dat_tp[[col]] <- 0
      } else {
        road_effic_dat_tp[[col]] <- vector("numeric", length = 0)
      }
    }    
    
  } 
  
  # Format point source BMP costs data
  
  temp_point_costs_dat <- bmp_costs_point[
    bmp_costs_point$COMID %in% streamcat_subset_all$comid,
  ] %>% 
    select(comid = COMID, State, BMP, CostType, Cost)
  
  temp_point_costs_capital <- temp_point_costs_dat %>%
    filter(CostType == "Capital") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(.cols = everything(), .fns = ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_operations <- temp_point_costs_dat %>%
    filter(CostType == "OM") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(.cols = everything(), .fns = ~replace_na(., 1e30))) %>% # Extremely high cost (1e^30) deters model from selecting BMPs without data
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_other <- as.data.frame(
    streamcat_subset_all[
      !(streamcat_subset_all$comid %in% temp_point_costs_dat$comid),
    ]
  )
  names(temp_point_costs_other) <- "comid"
  temp_point_costs_other_rev <- merge(
    temp_point_costs_other, COMID_State, by = c("comid"), all.x = TRUE
  )
  
  temp_point_costs_other_rev[3:(2+length(Point_BMPs))] <- 0
  names(temp_point_costs_other_rev) <- c(
    "comid", "State", Point_BMPs
  )
  
  if(nrow(temp_point_costs_capital) > 0) {
    temp_point_capital_costs_dat_rev <- rbind(
      temp_point_costs_capital, temp_point_costs_other_rev
    )
  } else {
    temp_point_capital_costs_dat_rev <- temp_point_costs_other_rev
  }
  
  temp_point_capital_costs_dat_rev <- temp_point_capital_costs_dat_rev[
    order(temp_point_capital_costs_dat_rev$comid),
  ]
  temp_point_capital_costs_dat_rev$comid_form <- paste0(
    "'", temp_point_capital_costs_dat_rev$comid, "'"
  )
  point_costs_capital_dat <- temp_point_capital_costs_dat_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  if(nrow(temp_point_costs_operations) > 0) {
    temp_point_operations_costs_dat_rev <- rbind(
      temp_point_costs_operations, temp_point_costs_other_rev
    )
  } else {
    temp_point_operations_costs_dat_rev <- temp_point_costs_other_rev
  }
  
  temp_point_operations_costs_dat_rev <- temp_point_operations_costs_dat_rev[
    order(temp_point_operations_costs_dat_rev$comid),
  ]
  temp_point_operations_costs_dat_rev$comid_form <- paste0(
    "'", temp_point_operations_costs_dat_rev$comid, "'"
  )
  point_costs_operations_dat <- temp_point_operations_costs_dat_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  # Format user defined limits on BMP implementation
  
  temp_bmp_imp <- user_specs_BMPs %>% 
    select(BMP_Category, BMP, BMP_Selection, frac_min, frac_max) %>%
    filter(BMP_Selection == "X") 
  
  urban_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "urban") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = gsub("/", "", BMP)) %>%
    filter(BMP %in% bmp_urban_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  road_bmp_imp <- temp_bmp_imp %>%
    filter(BMP_Category == "road") %>%
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = gsub("/", "", BMP)) %>%
    filter(BMP %in% bmp_road_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  ag_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "ag") %>% 
    select(BMP, frac_min, frac_max) %>%
    filter(BMP %in% bmp_ag_vec_direct) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  graz_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "graz") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  
  ripbuf_bmp_imp <- temp_bmp_imp %>% 
    filter(BMP_Category == "ripbuf") %>% 
    select(BMP, frac_min, frac_max) %>%
    mutate(BMP = paste0("'",BMP,"'"))
  

  ##### h. Make urban matrix of implementability ------
  
  if(length(Urban_BMPs) > 0) {
    urban_bmp_implementationpotential_dat <- setNames(
      data.frame(
        matrix(
          ncol = length(bmp_urban_vec_direct) + 1, nrow = length(urban_costs_capital_dat$comid_form)
        )
      ), 
      c("comid", bmp_urban_vec_direct)
    ) %>%
      mutate(
        comid_form = urban_costs_capital_dat$comid_form
      ) %>%
      mutate(across(.cols = any_of(bmp_urban_vec_direct), .fns = ~1)) %>%
      select(comid_form, any_of(bmp_urban_vec_direct))
  }
  
  ##### i. Make ag matrix of implementability ------  
  # Drainage_Management can only be implemented on tile-drained areas
  # Users also can restrict the custom BMPs to tile-drained areas
  
  if(length(Ag_BMPs) > 0) {
    
    ag_bmp_implementationpotential_dat <- setNames(
      data.frame(
        matrix(
          ncol = length(Ag_BMPs) + 1, nrow = length(streamcat_subset_all$comid)
        )
      ), 
      c("comid", Ag_BMPs)
    ) %>%
      mutate(
        comid = streamcat_subset_all$comid,
        comid_form = paste0("'", comid, "'")
      ) %>%
      left_join(., pct_tiled, by = "comid") %>%
      mutate(
        pct_tiled_adj = replace_na(pct_tiled_adj, 0),
        across(
          .cols = all_of(Ag_BMPs), 
          .fns = ~case_when(
            cur_column() %in% TileAg_BMPs ~ pct_tiled_adj,
            !(cur_column() %in% TileAg_BMPs)  ~ 1
          )
        )
      ) %>%
      select(comid_form, all_of(Ag_BMPs))
    
  }
  
  
  ##### j. Write AMPL data file ------
  
  write( "# Tier 1 Data AMPL File", file = paste(OutPath,"STdata.dat",sep = ""))
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(inc_tn_dat)) %do% {
        write(
          paste0("\nparam baseloads_N", i, " : 'point' 'urban' 'road' 'ag' 'graz':="), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tn_dat[[i]] %>% 
            select(
              comid_form, point = in_poin, urban = in_urb, road = in_road, ag = in_ag, 
              graz = in_graz
            ) %>%
            arrange(comid_form) %>%
            unique(), # renaming also forces the order in case they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(inc_tp_dat)) %do% {
        write(
          paste0("\nparam baseloads_P", i, " : 'point' 'urban' 'ag' 'graz' 'road' :="), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tp_dat[[i]] %>% 
            select(
              comid_form, point = ip_poin, urban = ip_urb, ag = ip_ag, 
              graz = ip_graz, road = ip_road
            ) %>%
            arrange(comid_form) %>%
            unique(), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(param_loads_lim[i,1] == "TN") {
          write(
            paste0("\nparam: riparianload_N", i, " :="), 
            file = paste(OutPath, "STdata.dat", sep = ""),
            append = T
          )
          if(length(RiparianBuffer_BMPs) > 0) {
            write.table(
              riparian_tn_dat[[i]] %>% 
                select(comid_form, riparian = in_riparian) %>%
                arrange(comid_form) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""),
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          } else {
            write.table(
              ripbuf_bmp_dummy_tn[[i]] %>% 
                select(comid, none = none) %>%
                arrange(comid) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          }
          write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T) 
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TP") {
          write(
            paste0("\nparam: riparianload_P", i, " :="), 
            file = paste(OutPath, "STdata.dat", sep = ""),
            append = T
          )
          if(length(RiparianBuffer_BMPs) > 0) {
            write.table(
              riparian_tp_dat[[i]] %>% 
                select(comid_form, riparian = ip_riparian) %>%
                arrange(comid_form) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          } else {
            write.table(
              ripbuf_bmp_dummy_tp[[i]] %>% 
                select(comid, none = none) %>%
                arrange(comid) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          }
          write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
        }
      }
    }
  )
  
  write(
    "\nparam urban_comid_xwalk :=",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
  write.table(
    urban_crosswalk %>%
      arrange(comid_form) %>%
      unique(),
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write(
    "\nparam road_comid_xwalk :=",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
  write.table(
    road_crosswalk %>%
      arrange(comid_form) %>%
      unique(),
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  
  write( 
    "\nparam area : 'ag' 'graz' :=", 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  write.table(
    area_dat %>%
      select(comid_form, ag = ag_ac_bmpadj, graz = graz_ac) %>%
      arrange(comid_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  write( "\n", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    paste0("param urban_area : 'urban' :="), 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  write.table(
    urban_area_dat %>%
      select(combo_form, urban_ac) %>%
      arrange(combo_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  write( "\n", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    paste0("param road_area : 'road' :="), 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  write.table(
    road_area_dat %>%
      select(combo_form, road_ac) %>%
      arrange(combo_form) %>%
      unique(), # renaming also forces the order in case they are disordered in processing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  if(length(Urban_BMPs) > 0) {
    write(
      paste0(
        "\nparam urban_bmp_implementationpotential : ", 
        paste(bmp_urban_vec, collapse = "  "), 
        " :="
      ),
      file = paste0(OutPath, "STdata.dat"),
      append = TRUE
    )
    write.table(
      urban_bmp_implementationpotential_dat %>% 
        select(comid_form, any_of(bmp_urban_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      paste0("\nparam urban_bmp_implementationpotential : 'none' :="),
      file = paste0(OutPath, "STdata.dat"),
      append = TRUE
    )
    write.table(
      urban_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  if(length(Ag_BMPs) > 0) {
    write(
      paste0(
        "\nparam ag_bmp_implementationpotential : ", 
        paste(bmp_ag_vec, collapse = "  "), 
        " :="
      ),
      file = paste0(OutPath, "STdata.dat"),
      append = TRUE
    )
    write.table(
      ag_bmp_implementationpotential_dat %>% 
        select(comid_form, any_of(Ag_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      paste0("\nparam ag_bmp_implementationpotential : 'none' :="),
      file = paste0(OutPath, "STdata.dat"),
      append = TRUE
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  if(length(RiparianBuffer_BMPs) > 0) {
    write( 
      paste0(
        "\nparam unbuffered_banklength : ",  
        paste(bmp_ripbuf_vec, collapse  = "  "), 
        " :="
      ), 
      file = paste(OutPath,"STdata.dat",sep = ""), 
      append = T
    )
    write.table(
      streambanklength_available_dat %>% 
        select(comid_form, any_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write( 
      paste0("\nparam unbuffered_banklength: 'none' :="), 
      file = paste(OutPath,"STdata.dat",sep = ""), 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    paste0("\nparam total_banklength :="), 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    write.table(
      streambanklength_total_dat %>% 
        select(comid_form, totalbanklength_ft) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>% 
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  if(length(Ag_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_N : ",bmp_ag_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        ag_effic_dat_tn %>% 
          select(comid_form, all_of(bmp_ag_vec_direct)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_P : ",bmp_ag_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        ag_effic_dat_tp %>%
          select(comid_form, all_of(bmp_ag_vec_direct)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", ag_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam ag_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", ag_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  }
  
  if(length(Graz_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_N : ",bmp_graz_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        graz_effic_dat_tn %>% 
          select(comid_form, all_of(Graz_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_P : ",bmp_graz_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        graz_effic_dat_tp %>% 
          select(comid_form, all_of(Graz_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", graz_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam graz_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", graz_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  }
  
  
  if(length(Point_BMPs) > 0) {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_N : ",bmp_point_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        point_effic_dat_tn %>% 
          select(comid_form, all_of(Point_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_P : ",bmp_point_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        point_effic_dat_tp %>% 
          select(comid_form, all_of(Point_BMPs)) %>%
          arrange(comid_form) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  } else {
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_N : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", point_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_P : 'none' :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", point_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    }
  }
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(Urban_BMPs) > 0) {
      cat(
        "\nparam urban_effic_N : ",bmp_urban_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        urban_effic_dat_tn %>%
          select(comid_form, all_of(bmp_urban_direct)) %>%
          arrange(comid_form)%>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam urban_effic_N : 'none' :=", 
        file = paste0(OutPath, "STdata.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(Urban_BMPs) > 0) {
      cat(
        "\nparam urban_effic_P : ",bmp_urban_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        urban_effic_dat_tp %>%
          select(comid_form, all_of(bmp_urban_direct)) %>%
          arrange(comid_form) %>%
          unique(),  # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam urban_effic_P: 'none' :=", 
        file = paste0(OutPath, "STdata.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(road_BMPs) > 0) {
      cat(
        "\nparam road_effic_N : ",bmp_road_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        road_effic_dat_tn %>%
          select(comid_form, all_of(road_BMPs)) %>%
          arrange(comid_form)%>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam road_effic_N : 'none' :=", 
        file = paste0(OutPath, "STdata.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", road_bmp_dummy_tn)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    if(length(road_BMPs) > 0) {
      cat(
        "\nparam road_effic_P : ",bmp_road_vec," :=", 
        file = paste(OutPath,"STdata.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        road_effic_dat_tp %>%
          select(comid_form, all_of(road_BMPs)) %>%
          arrange(comid_form) %>%
          unique(),  # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        "\nparam road_effic_P: 'none' :=", 
        file = paste0(OutPath, "STdata.dat"), 
        sep = "", 
        append = T
      )
      cat(
        "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
      write.table(
        unique(do.call("rbind", urban_bmp_dummy_tp)) %>% 
          select(comid, none) %>%
          arrange(comid) %>%
          unique(), # renaming also forces the order in case they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = "") , 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(length(RiparianBuffer_BMPs) > 0) {
        if(param_loads_lim[i,1] == "TN") {
          write(
            paste0(
              "\nparam riparianremoval_N", 
              i, 
              " : ", 
              paste(bmp_ripbuf_vec, collapse  = "  "), 
              " :="
            ), 
            file = paste(OutPath, "STdata.dat", sep = ""),
            append = T
          )
          write.table(
            riparian_tn_removal[[i]] %>% 
              select(comid_form, any_of(RiparianBuffer_BMPs)) %>%
              arrange(comid_form) %>%
              unique(), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          write(
            paste0("\nparam riparianremoval_N", i, " : 'none' :="), 
            file = paste(OutPath, "STdata.dat", sep = ""),
            append = T
          )
          write.table(
            ripbuf_bmp_dummy_tn[[i]] %>% 
              select(comid, none) %>%
              arrange(comid) %>%
              unique(), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
          write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T) 
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(length(RiparianBuffer_BMPs) > 0) {
          if(param_loads_lim[i,1] == "TP") {
            write(
              paste0(
                "\nparam riparianremoval_P", 
                i, 
                " : ", 
                paste(bmp_ripbuf_vec, collapse  = "  "), 
                " :="
              ), 
              file = paste(OutPath, "STdata.dat", sep = ""),
              append = T
            )
            write.table(
              riparian_tp_removal[[i]] %>% 
                select(comid_form, any_of(RiparianBuffer_BMPs)) %>%
                arrange(comid_form) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          } else {
            write(
              paste0("\nparam riparianremoval_P", i, " : 'none' :="), 
              file = paste(OutPath, "STdata.dat", sep = ""),
              append = T
            )
            write.table(
              ripbuf_bmp_dummy_tp[[i]] %>% 
                select(comid, none) %>%
                arrange(comid) %>%
                unique(), # renaming also forces the order in case they are disordered in processing code above
              file = paste(OutPath, "STdata.dat", sep = ""), 
              append = T,
              sep = "\t",
              row.names = F,
              col.names = F,
              na = "",
              quote = F
            )
          }
          write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T) 
        }
      }
      }
  )
  
  if(length(Graz_BMPs) > 0) {
    cat(
      "\nparam graz_costs_capital : ",bmp_graz_vec," :=	",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_costs_cap_dat %>% 
        select(comid_form, all_of(Graz_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam graz_costs_operations : ",bmp_graz_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_costs_op_dat %>% 
        select(comid_form, all_of(Graz_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam graz_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam graz_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      graz_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if(length(Ag_BMPs) > 0) {
    cat(
      "\nparam ag_costs_capital : ",bmp_ag_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_costs_cap_dat %>% 
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam ag_costs_operations : ",bmp_ag_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_costs_op_dat %>% 
        select(comid_form, all_of(bmp_ag_vec_direct)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam ag_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam ag_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ag_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  
  if(length(Point_BMPs) > 0) {
    cat(
      "\nparam point_costs_capital : ",bmp_point_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_costs_capital_dat %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam point_costs_operations : ",bmp_point_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_costs_operations_dat %>% 
        select(comid_form, all_of(Point_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam point_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam point_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      point_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }

  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_costs_capital : ",bmp_urban_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_costs_capital_dat %>% 
        select(comid_form, all_of(bmp_urban_direct)) %>%
        arrange(comid_form) %>%
        mutate(across(where(is.list), unlist)) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam urban_costs_operations : ",bmp_urban_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_costs_operations_dat %>% 
        select(comid_form, all_of(bmp_urban_direct))%>%
        arrange(comid_form) %>%
        mutate(across(where(is.list), unlist)) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam urban_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam urban_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      urban_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  

  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_costs_capital : ",bmp_road_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_costs_capital_dat %>% 
        select(combo_form, all_of(bmp_road_direct)) %>%
        arrange(combo_form) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam road_costs_operations : ",bmp_road_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_costs_operations_dat %>%
        select(combo_form, all_of(bmp_road_direct))%>%
        arrange(combo_form) %>%
        unique(), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam road_costs_capital : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam road_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      road_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if(length(RiparianBuffer_BMPs) > 0) {
    cat(
      "\nparam ripbuf_costs_capital : ", bmp_ripbuf_vec, " :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_costs_cap_dat %>% 
        select(comid_form, all_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam ripbuf_costs_operations : ", bmp_ripbuf_vec," :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_costs_op_dat %>% 
        select(comid_form, all_of(RiparianBuffer_BMPs)) %>%
        arrange(comid_form) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  } else {
    cat(
      "\nparam ripbuf_costs_capital : 'none' :=	",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>%
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
    
    cat(
      "\nparam ripbuf_costs_operations : 'none' :=",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      ripbuf_bmp_dummy %>% 
        select(comid, none) %>%
        arrange(comid) %>%
        unique(), # renaming also forces the order in case they are disordered in processing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(param_loads_lim[i,1] == "TN") {
          cat(
            paste0("\nparam loads_lim_N", i, " :="),
            file = paste(OutPath, "STdata.dat", sep = ""),
            sep = "", 
            append = T
          )
          cat(
            param_loads_lim[i,2], 
            file = paste(OutPath, "STdata.dat", sep = ""), 
            sep = "", 
            append = T
          )
          cat(
            ";", 
            file = paste(OutPath, "STdata.dat", sep = ""), 
            sep = "",
            append = T
          ) 
        }
      }
    }
  )

  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TP") {
          cat(
            paste0("\nparam loads_lim_P", i, " :="),
            file = paste(OutPath, "STdata.dat", sep = ""),
            sep = "", 
            append = T
          )
          cat(
            param_loads_lim[i,2], 
            file = paste(OutPath, "STdata.dat", sep = ""), 
            sep = "", 
            append = T
          )
          cat(
            ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T)
  
  invisible(
    foreach(i = 1:length(param_other_loads_tn)) %do% {
      cat(
        paste0("\nparam other_loads_N", i, " :="),
        file = paste(OutPath, "STdata.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        param_other_loads_tn[i], 
        file = paste(OutPath, "STdata.dat", sep = ""), 
        sep = "", 
        append = T
      )
      cat(
        ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    }
  )
  
  invisible(
    foreach(i = 1:length(param_other_loads_tp)) %do% {
      cat(
        paste0("\nparam other_loads_P", i, " :="),
        file = paste(OutPath, "STdata.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        param_other_loads_tp[i], 
        file = paste(OutPath, "STdata.dat", sep = ""), 
        sep = "", 
        append = T
      )
      cat(
        ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    }
  )
  
  cat("\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T)
  
  cat(
    "\nparam agcost_frac :=",
    file = paste(OutPath, "STdata.dat", sep = ""), 
    sep = "", 
    append = T
  )
  cat(
    agcost_frac, 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    sep = "", 
    append = T
  )
  cat(
    ";",file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
  )
  
  cat(
    "
param agBMP_minarea := 1;
param grazBMP_minarea := 1;
param urbanBMP_minarea := 1;
param roadBMP_minarea := 1;",
    file = paste(OutPath, "STdata.dat", sep = ""), sep = "\n", append = T
  )
  
  write( 
    "\nparam: urban_frac_min urban_frac_max :=", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    write.table(
      urban_bmp_imp,
      file = paste(OutPath, "STdata.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    "\nparam: road_frac_min road_frac_max :=", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  if(length(road_BMPs) > 0) {
    write.table(
      road_bmp_imp,
      file = paste(OutPath, "STdata.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    "\nparam: ag_frac_min ag_frac_max :=", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    write.table(
      ag_bmp_imp,
      file = paste(OutPath, "STdata.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata.dat", sep = ""),
      append = T
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    "\nparam: graz_frac_min graz_frac_max :=", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  if(length(Graz_BMPs) > 0) {
    write.table(
      graz_bmp_imp,
      file = paste(OutPath, "STdata.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata.dat", sep = ""),
      append = T
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  write( 
    "\nparam: ripbuf_frac_min ripbuf_frac_max :=", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    write.table(
      ripbuf_bmp_imp,
      file = paste(OutPath, "STdata.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    write(
      "'none'     0     0",
      file = paste(OutPath, "STdata.dat", sep = ""),
      append = T
    )
  }
  
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  

  ### III. Develop model file ------
  
  ##### a. Save COMIDs as vector ------
  
  comid_vec_all <- paste0("'", unique(streamcat_subset_all$comid),"'")
  comid_vec_all[-length(comid_vec_all)] <- paste0(
    comid_vec_all[-length(comid_vec_all)], ','
  )
  
  urban_comid_vec_all <- paste0(unique(urban_area_dat$combo_form))
  urban_comid_vec_all[-length(urban_comid_vec_all)] <- paste0(
    urban_comid_vec_all[-length(urban_comid_vec_all)], ','
  )
  match_urban_comid_all <- paste0(unique(urban_crosswalk$comid_form))
  match_urban_comid_all[-length(match_urban_comid_all)] <- paste0(
    match_urban_comid_all[-length(match_urban_comid_all)], ','
  )
  
  road_comid_vec_all <- paste0(unique(road_costs_dat$combo_form))
  road_comid_vec_all[-length(road_comid_vec_all)] <- paste0(
    road_comid_vec_all[-length(road_comid_vec_all)], ','
  ) 
  match_road_comid_all <- paste0(unique(road_crosswalk$comid_form))
  match_road_comid_all[-length(match_road_comid_all)] <- paste0(
    match_road_comid_all[-length(match_road_comid_all)], ','
  )
  
  comid_vec_all_N <- paste0(
    "'", unique(unlist(streamcat_subset_tn, use.names = FALSE)), "'"
  )
  comid_vec_all_N[-length(comid_vec_all_N)] <- paste0(
    comid_vec_all_N[-length(comid_vec_all_N)], ','
  )
  
  urban_all_N_tmp <- urban_areas %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tn, use.names = FALSE)) %>% unique() %>%
    mutate(comid_form = paste0("'", comid, "'"))
  urban_all_N <- urban_crosswalk %>% filter(comid_form %in% urban_all_N_tmp$comid_form) %>% unique()
  urban_vec_all_N <- urban_all_N$combo_form
  urban_vec_all_N[-length(urban_vec_all_N)] <- paste0(
    urban_vec_all_N[-length(urban_vec_all_N)], ','
  )
  
  road_all_N_tmp <- road_area %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tn, use.names = FALSE)) %>% unique() %>%
    mutate(comid_form = paste0("'", comid, "'"))
  road_all_N <- road_crosswalk %>% filter(comid_form %in% road_all_N_tmp$comid_form) %>% unique()
  road_vec_all_N <- road_all_N$combo_form
  road_vec_all_N[-length(road_vec_all_N)] <- paste0(
    road_vec_all_N[-length(road_vec_all_N)], ','
  )
  
  comid_vec_all_P <- paste0(
    "'", unique(unlist(streamcat_subset_tp, use.names = FALSE)), "'"
  )
  comid_vec_all_P[-length(comid_vec_all_P)] <- paste0(
    comid_vec_all_P[-length(comid_vec_all_P)], ','
  )
  
  urban_all_P_tmp <- urban_areas %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tp, use.names = FALSE)) %>% unique() %>%
    mutate(comid_form = paste0("'", comid, "'"))
  urban_all_P <- urban_crosswalk %>% filter(comid_form %in% urban_all_P_tmp$comid_form) %>% unique()
  urban_vec_all_P <- urban_all_P$combo_form
  urban_vec_all_P[-length(urban_vec_all_P)] <- paste0(
    urban_vec_all_P[-length(urban_vec_all_P)], ','
  )
  
  road_all_P_tmp <- road_area %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tp, use.names = FALSE)) %>% unique() %>%
    mutate(comid_form = paste0("'", comid, "'"))
  road_all_P <- road_crosswalk %>% filter(comid_form %in% road_all_P_tmp$comid_form) %>% unique()
  road_vec_all_P <- road_all_P$combo_form
  road_vec_all_P[-length(road_vec_all_P)] <- paste0(
    road_vec_all_P[-length(road_vec_all_P)], ','
  )  
  
  
  comid_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    if (length(unlist(streamcat_subset_tn[[i]])) > 0) {
      comid_vec_N_tmp <- paste0("'", streamcat_subset_tn[[i]]$comid, "'")
      comid_vec_N_tmp[-length(comid_vec_N_tmp)] <- paste0(
        comid_vec_N_tmp[-length(comid_vec_N_tmp)], ','
      )
      comid_vec_N_tmp} else {
        comid_vec_N_tmp <- ""
        comid_vec_N_tmp
      }
  }
  
  urban_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    if (length(unlist(streamcat_subset_tn[[i]])) > 0) {
      urban_vec_N_tmp <- urban_areas %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tn[[i]], use.names = FALSE)) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>% unique()
      urban_N <- urban_crosswalk %>% filter(comid_form %in% urban_vec_N_tmp$comid_form) %>% unique()
      urban_N_tmp <- urban_N$combo_form
      urban_N_tmp[-length(urban_N_tmp)] <- paste0(
        urban_N_tmp[-length(urban_N_tmp)], ','
      )
      urban_N_tmp} else {
        urban_N_tmp <- ""
        urban_N_tmp
      }
  }   
  
  road_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    if (length(unlist(streamcat_subset_tn[[i]])) > 0) {
      road_vec_N_tmp <- road_area %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tn[[i]], use.names = FALSE)) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>% unique()
      road_N <- road_crosswalk %>% filter(comid_form %in% road_vec_N_tmp$comid_form) %>% unique()
      road_N_tmp <- road_N$combo_form
      road_N_tmp[-length(road_N_tmp)] <- paste0(
        road_N_tmp[-length(road_N_tmp)], ','
      )
      road_N_tmp} else {
        road_N_tmp <- ""
        road_N_tmp
      }
  }
  
  comid_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    if (length(unlist(streamcat_subset_tp[[i]])) > 0) {
      comid_vec_P_tmp <- paste0("'", streamcat_subset_tp[[i]]$comid, "'")
      comid_vec_P_tmp[-length(comid_vec_P_tmp)] <- paste0(
        comid_vec_P_tmp[-length(comid_vec_P_tmp)], ','
      )
      comid_vec_P_tmp} else {
        comid_vec_P_tmp <- ""
        comid_vec_P_tmp
      }
  }
  
  urban_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    if (length(unlist(streamcat_subset_tp[[i]])) > 0) {
      urban_vec_P_tmp <- urban_areas %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tp[[i]], use.names = FALSE)) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>% unique()
      urban_P <- urban_crosswalk %>% filter(comid_form %in% urban_vec_P_tmp$comid_form) %>% unique()
      urban_P_tmp <- urban_P$combo_form
      urban_P_tmp[-length(urban_P_tmp)] <- paste0(
        urban_P_tmp[-length(urban_P_tmp)], ','
      )
      urban_P_tmp} else {
        urban_P_tmp <- ""
        urban_P_tmp
      }
  }   
  
  road_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    if (length(unlist(streamcat_subset_tp[[i]])) > 0) {
      road_vec_P_tmp <- road_area %>% select(comid) %>% filter(comid %in% unlist(streamcat_subset_tp[[i]], use.names = FALSE)) %>%
        mutate(comid_form = paste0("'", comid, "'")) %>% unique()
      road_P <- road_crosswalk %>% filter(comid_form %in% road_vec_P_tmp$comid_form) %>% unique()
      road_P_tmp <- road_P$combo_form
      road_P_tmp[-length(road_P_tmp)] <- paste0(
        road_P_tmp[-length(road_P_tmp)], ','
      )
      road_P_tmp} else {
        road_P_tmp <- ""
        road_P_tmp
      }
  } 
  
  
  ##### b. Write AMPL model file ------
  
  write(
    "#STmodel.mod
      \nset comid_all :=", 
    file = paste(OutPath, "STmodel.mod", sep = "")
  )
  cat(
    "{",comid_vec_all,"};",
    file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
  )
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      paste0("\nset comid_all_N within comid_all := "),
      file = paste0(OutPath, "STmodel.mod"),
      sep = "",
      append = T
    )
    cat(
      "{",unique(comid_vec_all_N),"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      paste0("\nset comid_all_P within comid_all := "),
      file = paste0(OutPath, "STmodel.mod"),
      sep = "",
      append = T
    )
    cat(
      "{",unique(comid_vec_all_P),"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
  }
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        
        cat(
          paste0("\nset comid_N", i, " within comid_all_N := "),
          file = paste0(OutPath, "STmodel.mod"),
          sep = "",
          append = T
        )
        cat(
          "{", comid_vec_N[[i]], "};",
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "",
          append = T
        )
        cat(
          "\n", 
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "", 
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        
        cat(
          paste0("\nset comid_P", i, " within comid_all_P := "),
          file = paste0(OutPath, "STmodel.mod"),
          sep = "",
          append = T
        )
        cat(
          "{", comid_vec_P[[i]], "};",
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "",
          append = T
        )
        cat(
          "\n",
          file = paste(OutPath, "STmodel.mod", sep = ""), 
          sep = "", 
          append = T
        )
      }
    }
  )
  
  if (length(Urban_BMPs) > 0) {  
    cat(
      "\nset urban_comid_all :=", 
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "{",urban_comid_vec_all,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
    
    cat(
      "\nset matching_urban_comid :=",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "{",match_urban_comid_all,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
    
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\nset urban_comid_all_N within urban_comid_all := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{",unique(urban_vec_all_N),"};",
        file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
      cat(
        "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\nset urban_comid_all_P within urban_comid_all := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{",unique(urban_vec_all_P),"};",
        file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
      cat(
        "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
    }
    
    invisible(
      if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(urban_vec_N)) %do% {
          
          cat(
            paste0("\nset urban_comid_N", i, " within urban_comid_all_N := "),
            file = paste0(OutPath, "STmodel.mod"),
            sep = "",
            append = T
          )
          cat(
            "{", urban_vec_N[[i]], "};",
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "",
            append = T
          )
          cat(
            "\n", 
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "", 
            append = T
          )
        }
      }
    )
    
    invisible(
      if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(urban_vec_P)) %do% {
          
          cat(
            paste0("\nset urban_comid_P", i, " within urban_comid_all_P := "),
            file = paste0(OutPath, "STmodel.mod"),
            sep = "",
            append = T
          )
          cat(
            "{", urban_vec_P[[i]], "};",
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "",
            append = T
          )
          cat(
            "\n",
            file = paste(OutPath, "STmodel.mod", sep = ""), 
            sep = "", 
            append = T
          )
        }
      }
    ) }
  
  if (length(road_BMPs) > 0) {  
    cat(
      "\nset road_comid_all :=", 
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "{",road_comid_vec_all,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
    
    cat(
      "\nset matching_road_comid :=",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat(
      "{",match_road_comid_all,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
    cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
    
    
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\nset road_comid_all_N within road_comid_all := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{",unique(road_vec_all_N),"};",
        file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
      cat(
        "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
    }
    
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\nset road_comid_all_P within road_comid_all := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{",unique(road_vec_all_P),"};",
        file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
      cat(
        "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
      )
    }
    
    invisible(
      if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(road_vec_N)) %do% {
          
          cat(
            paste0("\nset road_comid_N", i, " within road_comid_all_N := "),
            file = paste0(OutPath, "STmodel.mod"),
            sep = "",
            append = T
          )
          cat(
            "{", road_vec_N[[i]], "};",
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "",
            append = T
          )
          cat(
            "\n", 
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "", 
            append = T
          )
        }
      }
    )
    
    invisible(
      if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
        foreach(i = 1:length(road_vec_P)) %do% {
          
          cat(
            paste0("\nset road_comid_P", i, " within road_comid_all_P := "),
            file = paste0(OutPath, "STmodel.mod"),
            sep = "",
            append = T
          )
          cat(
            "{", road_vec_P[[i]], "};",
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "",
            append = T
          )
          cat(
            "\n",
            file = paste(OutPath, "STmodel.mod", sep = ""), 
            sep = "", 
            append = T
          )
        }
      }
    ) }
  
  cat(
    "\n\nset urban_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    cat(
      "{",bmp_urban_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }
  
  
  cat(
    "\n\nset urban_pervbmp within urban_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))) {
      cat(
        "{",pervbmp_urban_vec_comma,"};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "", 
        append = T
      )  
    } else {
      cat(
        "{};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "", 
        append = T
      )  
    }
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }

  cat(
    "\n\nset road_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(road_BMPs) > 0) {
    cat(
      "{",bmp_road_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }

  cat(
    "\n\nset ag_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    cat(
      "{",bmp_ag_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat(
    "\n\nset ag_tilebmp within ag_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Ag_BMPs) > 0) {
    if(!grepl("''", paste0(tilebmp_ag_vec_comma, collapse = ""))) {
      cat(
        "{",tilebmp_ag_vec_comma,"};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "", 
        append = T
      )  
    } else {
      cat(
        "{};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "", 
        append = T
      )  
    }
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }
  
  cat(
    "\n\nset graz_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Graz_BMPs) > 0) {
    cat(
      "{",bmp_graz_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  
  cat(
    "\n\nset ripbuf_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(RiparianBuffer_BMPs) > 0) {
    cat(
      "{",bmp_ripbuf_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    ) 
  }
  
  cat(
    "\n\nset point_bmp :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Point_BMPs) > 0) {
    cat(
      "{",bmp_point_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  
  cat(
    "\n\nset loads_all :=
{'point', 'urban', 'road', 'ag', 'graz'};
  
set area_sub :=
{'ag', 'graz'};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
  cat(
    "set urban_area_sub :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(Urban_BMPs) > 0) {
    cat(
      "{'urban'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
  
  cat(
    "set road_area_sub :=",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  if(length(road_BMPs) > 0) {
    cat(
      "{'road'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  } else {
    cat(
      "{'none'};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )
  }
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
  
  cat("param urban_comid_xwalk {urban_comid_all} symbolic;", file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "",
      append = T)
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
  
  cat("param road_comid_xwalk {road_comid_all} symbolic;", file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "",
      append = T)
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0("param baseloads_N", i, " {comid_N", i, ", loads_all} >= 0;"),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0("param baseloads_P", i, " {comid_P", i, ", loads_all} >= 0;"),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0("param riparianload_N", i, " {c in comid_N", i, "} >= 0;"),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0("param riparianload_P", i, " {c in comid_P", i, "} >= 0;"),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  cat(
    "\nparam area {comid_all, area_sub} >=0;
    
param urban_area {urban_comid_all, urban_area_sub} >=0;
param road_area {road_comid_all, road_area_sub} >=0;
param urban_bmp_implementationpotential {urban_comid_all, urban_bmp} >= 0;
param ag_bmp_implementationpotential {comid_all, ag_bmp} >= 0;
param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;

param ag_costs_capital {comid_all, ag_bmp};
param ag_costs_operations {comid_all, ag_bmp};
param graz_costs_capital {comid_all, graz_bmp};
param graz_costs_operations {comid_all, graz_bmp};
param point_costs_capital {comid_all, point_bmp};
param point_costs_operations {comid_all, point_bmp};
param urban_costs_capital {urban_comid_all, urban_bmp};
param urban_costs_operations {urban_comid_all, urban_bmp};
param road_costs_capital {road_comid_all, road_bmp};
param road_costs_operations {road_comid_all, road_bmp};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n",
    append = T
  )  

  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N {comid_all_N, ag_bmp};
param graz_effic_N {comid_all_N, graz_bmp};
param point_effic_N {comid_all_N, point_bmp};
param urban_effic_N {urban_comid_all_N, urban_bmp};
param road_effic_N {road_comid_all_N, road_bmp};\n",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }

  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P {comid_all_P, ag_bmp};
param graz_effic_P {comid_all_P, graz_bmp};
param point_effic_P {comid_all_P, point_bmp};
param urban_effic_P {urban_comid_all_P, urban_bmp};
param road_effic_P {road_comid_all_P,road_bmp};\n",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }
  
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_N)) %do% {
        cat(
          paste0(
            "param riparianremoval_N", 
            i, 
            " {c in comid_N", 
            i, 
            ", b in ripbuf_bmp};"
          ),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(comid_vec_P)) %do% {
        cat(
          paste0(
            "param riparianremoval_P", 
            i, 
            " {c in comid_P", 
            i, 
            ", b in ripbuf_bmp};"
          ),
          file = paste(OutPath, "STmodel.mod", sep = ""),
          sep = "\n",
          append = T
        )
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(param_loads_lim[i,1] == "TN") {
          cat(
            paste0("param loads_lim_N", i, " >= 0;"),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)
  
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TP") {
          cat(
            paste0("param loads_lim_P", i, " >= 0;"),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n",
            append = T
          ) 
        }
      }
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)
  
  invisible(
    foreach(i = 1:length(param_other_loads_tn)) %do% {
      cat(
        paste0("param other_loads_N", i, " >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)
  
  invisible(
    foreach(i = 1:length(param_other_loads_tp)) %do% {
      cat(
        paste0("param other_loads_P", i, " >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  )
  
  cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)
  
  # All land use included in urban components
  cat(
    "param agcost_frac >=0;
    
param agBMP_minarea;
param grazBMP_minarea;
param urbanBMP_minarea;
param roadBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;
param road_frac_min {road_bmp} >=0, <= 1;
param road_frac_max {r in road_bmp} >= road_frac_min[r], <= 1;
param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;
param graz_frac_min {graz_bmp} >= 0, <= 1;
param graz_frac_max {g in graz_bmp} >= graz_frac_min[g], <= 1;
param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {b in ripbuf_bmp} >= ripbuf_frac_min[b], <= 1;
  
param ps_coef {c in comid_all, p in point_bmp} := (point_costs_capital[c, p] + 
    point_costs_operations[c, p]);
param urban_coef {x in urban_comid_all, u in urban_bmp} := urban_area[x,'urban'] * 
  (urban_costs_capital[x,u] + urban_costs_operations[x,u]) ;
param road_coef {h in road_comid_all, r in road_bmp} := road_area[h,'road'] * 
  (road_costs_capital[h,r] + road_costs_operations[h,r]) ;
param ag_coef {c in comid_all, a in ag_bmp} := agcost_frac * area[c,'ag'] * 
  (ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;
param graz_coef {c in comid_all, g in graz_bmp} := agcost_frac * area[c,'graz'] * 
  (graz_costs_capital[c,g] + graz_costs_operations[c,g]) ;
param ripbuf_coef {c in comid_all, b in ripbuf_bmp} := 
  agcost_frac * (ripbuf_costs_capital[c, b] + ripbuf_costs_operations[c, b]);
  
var agBMP_bin {comid_all, ag_bmp} binary;
var grazBMP_bin {comid_all, graz_bmp} binary;
var urbanBMP_bin {urban_comid_all, urban_bmp} binary;
var roadBMP_bin {road_comid_all, road_bmp} binary;

var point_dec {c in comid_all, p in point_bmp} binary :=0;
var urban_frac {x in urban_comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[x, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[x, u] :=0;
var ag_frac {c in comid_all, a in ag_bmp}
   >= ag_frac_min[a] * ag_bmp_implementationpotential[c, a]  
   <= ag_frac_max[a] * ag_bmp_implementationpotential[c, a] :=0;
var road_frac {road_comid_all, r in road_bmp} >= road_frac_min[r] <= road_frac_max[r] :=0;
var graz_frac {comid_all, g in graz_bmp} >= graz_frac_min[g] <= graz_frac_max[g] :=0;
var ripbuf_length {c in comid_all, b in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, b] * ripbuf_frac_max[b] := 0;
  
minimize cost: sum {c in comid_all, p in point_bmp} (ps_coef[c, p] * point_dec[c, p]) + 
sum {x in urban_comid_all, u in urban_bmp} (urban_coef[x,u] * urban_frac[x,u]) +
sum {h in road_comid_all, r in road_bmp} (road_coef[h,r] * road_frac[h,r]) +
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, g in graz_bmp} (graz_coef[c,g] * graz_frac[c,g]) +
sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef[c, b] * ripbuf_length[c, b]);
  \n",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )

  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if(param_loads_lim[i,1] == "TN") {
          cat(
            paste0(
              "subject to total_loads_N", i, ":\n",
              "other_loads_N", i, " + ",
              "sum {c in comid_N", i, "} (baseloads_N", i, 
              "[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])))", " + \n",
              "sum {c in comid_N", i, "} (baseloads_N", i, 
              "[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g])))", " + \n",
              "sum {c in comid_N", i, "} (baseloads_N", i, 
              "[c,'urban'] * (1 - sum {x in urban_comid_N", i, ", u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_N[x,u] * urban_frac[x,u]))) + \n",
              "sum {c in comid_N", i, "} (baseloads_N", i, 
              "[c,'road'] * (1 - sum {h in road_comid_N", i, ", r in road_bmp : c == road_comid_xwalk[h]}(road_effic_N[h,r] *", " road_frac[h,r]))) + \n",
              "sum {c in comid_N", i, "} (baseloads_N", i, 
              "[c,'point'] * (1 - sum {p in point_bmp} (point_effic_N[c,p] * point_dec[c,p]))) - \n",
              "sum {c in comid_N", i, ", b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N", i, "[c, b]) <= loads_lim_N", i, "; \n"
            ),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n", 
            append = T
          ) 
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TP") {
          cat(
            paste0(
              "subject to total_loads_P", i, ":\n",
              "other_loads_P", i, " + ",
              "sum {c in comid_P", i, "} (baseloads_P", i, 
              "[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])))", " + \n",
              "sum {c in comid_P", i, "} (baseloads_P", i, 
              "[c,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_P[c,g] * graz_frac[c,g])))", " + \n",
              "sum {c in comid_P", i, "} (baseloads_P", i, 
              "[c,'urban'] * (1 - sum {x in urban_comid_P", i, ", u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_P[x,u] * urban_frac[x,u]))) + \n",
              "sum {c in comid_P", i, "} (baseloads_P", i, 
              "[c,'road'] * (1 - sum {h in road_comid_P", i, ", r in road_bmp : c == road_comid_xwalk[h]}(road_effic_P[h,r] *", " road_frac[h,r]))) + \n",
              "sum {c in comid_P", i, "} (baseloads_P", i, 
              "[c,'point'] * (1 - sum {p in point_bmp} (point_effic_P[c,p] * point_dec[c,p]))) - \n",
              "sum {c in comid_P", i, ", b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_P", i, "[c, b]) <= loads_lim_P", i, "; \n"
              
            ),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n", 
            append = T
          ) 
        }
      }
    }
  )
  
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TN") {
          cat(
            paste0(
              "subject to riparian_loads_N", i, " {c in comid_N", i, "}:\n",
              "sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N", i, 
              "[c, b]) <= riparianload_N", i, "[c]; \n"
            ),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n", 
            append = T
          ) 
        }
      }
    }
  )
  
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
        if (param_loads_lim[i,1] == "TP") {
          cat(
            paste0(
              "subject to riparian_loads_P", i, " {c in comid_P", i, "}:\n",
              "sum {b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_P", i, 
              "[c, b]) <= riparianload_P", i, "[c]; \n"
            ),
            file = paste(OutPath, "STmodel.mod", sep = ""),
            sep = "\n", 
            append = T
          ) 
        }
      }
    }
  )
  
  cat(
    "subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];

subject to graz_treat_min {c in comid_all, g in graz_bmp}:
graz_frac[c,g] * area[c,'graz'] >= grazBMP_minarea * grazBMP_bin[c,g];

subject to urban_treat_min {x in urban_comid_all, u in urban_bmp}:
urban_frac[x,u] * urban_area[x,'urban'] >= urbanBMP_minarea * urbanBMP_bin[x,u];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];

subject to graz_frac_const {c in comid_all, g in graz_bmp}:
graz_frac[c,g] <= grazBMP_bin[c,g];
  
subject to urban_frac_const {x in urban_comid_all, u in urban_bmp}:
urban_frac[x,u] <= urbanBMP_bin[x,u];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to graz_frac_limit {c in comid_all}: 
sum {g in graz_bmp} graz_frac[c,g] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to urban_frac_limit {x in urban_comid_all}: 
sum {u in urban_bmp} urban_frac[x,u] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to point_dec_limit {c in comid_all}: 
sum {p in point_bmp} point_dec[c,p] <= 1;
# limits each WWTP to one upgrade

subject to total_banks {c in comid_all}:
sum {b in ripbuf_bmp} ripbuf_length[c,b] <= max {b in ripbuf_bmp} 
   unbuffered_banklength[c,b];
  
",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
  
  if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))){
    cat(
      "
subject to roads_and_parkinglots {x in urban_comid_all}:
sum{up in urban_pervbmp} urban_frac[x, up] <= max {up in urban_pervbmp} 
   urban_bmp_implementationpotential[x,up];
",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n", 
      append = T
    )
  }
  
  if(!grepl("''", paste0(tilebmp_ag_vec_comma, collapse = ""))){
    cat(
      "
subject to tiledrains {c in comid_all}:
sum{agti in ag_tilebmp} ag_frac[c, agti] <= max {agti in ag_tilebmp}
   ag_bmp_implementationpotential[c,agti];
",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
  
  
  print(
    paste(
      "RBEROST has finished writing AMPL scripts without uncertainty at",
      Sys.time()
    )
  )
}
