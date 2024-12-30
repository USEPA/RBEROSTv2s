### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PURPOSE: Generate Uncertainty Estimates in the AMPL Model
# BY: Cathy Chamberlin, Kelly-Anne Moffa (ICF), Hunter Parker (ICF), Sam Ennett (ICF)
# DATE: 4/1/21
# UPDATED DATE: June 2024
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Based on: RBEROST-Northeast preprocessor with uncertainty.
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# Notes, Introduction, and Set up -----

# General Notes:
# This step generates the uncertainty estimates used in the AMPL model. 
# It is designed to be run after the Preprocessor, and will amend the AMPL files.
# This Preprocessor is sourced through the RunRBEROST-Pacific.Rmd file.
# Installation of necessary packages is done through this RunRBEROST-Pacific.Rmd file as well.
# If more advanced R users would like to assess different locations/data, they would need to search for and update lines of code that end in "#*#"
# Author notes are indicated by "#@# Note:"

### ### ### ### ### ### ### ### ### 
# PART 1: PREPROCESSOR STEP ------
### ### ### ### ### ### ### ### ###

### I. Run Preprocessor without uncertainty ------
source("./RBEROST-Pacific/R/01_Optimization_Preprocessing-Pacific.R", local = TRUE)

print(
  paste(
    "RBEROST is now creating AMPL files with uncertainty analysis at",
    Sys.time()
  )
)

### II. Define scenario increment ------
scenarioincrement <- 1 - scenariostepchange

### ### ### ### ### ### ### ### ### 
# PART 2: ADJUST & PREPROCESS DATA INPUTS TO INCLUDE UNCERTAINTY ------
### ### ### ### ### ### ### ### ### 

### I. SPARROW N uncertainty ------

temp_inc_tn <- sparrow_cons_out_tn %>% 
  select(c("comid", "il_tn_scg":"il_tn_wwtp", "ilse_tn_scg":"ilse_tn_wwtp")) %>%
  mutate(n = 200) %>%
  mutate(
    across(
      .cols = "il_tn_scg":"il_tn_wwtp",
      .fns = list(
        sd = ~(sparrow_cons_out_tn[[paste0("ilse", substr(cur_column(), 3, (nchar(cur_column()))))]] * sqrt(n)) #@# Note: The standard deviation is calculated from the standard error, but is never used. It was for exploratory purposes only and remains as legacy. 
      )
    )
  )

temp_inc_tn$in_ag <- with(temp_inc_tn, il_tn_fer) #*#

temp_inc_tn$sin_ag <- temp_inc_tn %>%
  rowwise() %>% 
  mutate(
    sin_ag = my_propogateerror(
      vals = list(
        c(il_tn_fer, ilse_tn_fer)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sin_ag

temp_inc_tn$in_urb <- with(temp_inc_tn, il_tn_urb)
temp_inc_tn$sin_urb <- with(temp_inc_tn, ilse_tn_urb)

temp_inc_tn$in_poin <- with(temp_inc_tn, il_tn_wwtp)
temp_inc_tn$sin_poin <- with(temp_inc_tn, ilse_tn_wwtp)

temp_inc_tn$in_graz <- 0
temp_inc_tn$sin_graz <- 0

temp_inc_tn$in_other <- with(temp_inc_tn, il_tn_scg + il_tn_spr + il_tn_ald)
temp_inc_tn$sin_other <- with(temp_inc_tn, sqrt(ilse_tn_scg ^ 2 + ilse_tn_spr ^ 2 + ilse_tn_ald ^ 2))

### II. NDEP uncertainty ------
### Adjust 2012 SPARROW loadings for N deposition based on percentage decreases to represent present conditions
#*# AL: This is now 2020 NDEP data, confirm use of 0.43 from literature and adjust variable names.

ndep_change <- ndep_change %>%
  mutate(
    TDEP_TN_2012_arch_sd = TDEP_TN_2012_arch * 0.43, #@# Note: Corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub, see Sensitivity Analysis Part 15 in Tier1OptimizationProject notebook
    TDEP_TN_2020_cur_sd = TDEP_TN_2020_cur * 0.43, #@# Note: Corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub, see Sensitivity Analysis Part 15 in Tier1OptimizationProject notebook
    AdjChange2012_2020_sd = AdjChange2012_2020 *
      sqrt(
        (
          sqrt(TDEP_TN_2012_arch_sd ^ 2 + TDEP_TN_2020_cur_sd ^ 2) /
            (TDEP_TN_2012_arch - TDEP_TN_2020_cur)
        ) ^ 2 +
          (TDEP_TN_2012_arch_sd/TDEP_TN_2012_arch) ^ 2
      ) /
      sqrt(nrow(.)) #@# Note: Using the standard error instead of the standard deviation keeps the distribution of resampled values similar to the original.
  )

##### a. Revised il_tn_atm uncertainty ------
temp_inc_tn_rev <- merge(
  temp_inc_tn,
  ndep_change[, c("comid", "AdjChange2012_2020", "AdjChange2012_2020_sd")],
  by = "comid",
  all.x = TRUE
)

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

temp_inc_tn_rev[c("il_tn_atm")] <-  temp_inc_tn_rev[c("il_tn_atm")] -
  temp_inc_tn_rev[c("il_tn_atm")] * temp_inc_tn_rev[["AdjChange2012_2020"]] #*#

temp_inc_tn_rev[c("ilse_tn_atm")] <- sqrt(
  (temp_inc_tn_rev[c("ilse_tn_atm")]) ^ 2 + 
    (
      (temp_inc_tn_rev[c("il_tn_atm")] * temp_inc_tn_rev[["AdjChange2012_2020"]]) *
        sqrt(
          (temp_inc_tn_rev[c("ilse_tn_atm")] / temp_inc_tn_rev[c("il_tn_atm")]) ^ 2 +
            (
              temp_inc_tn_rev[["AdjChange2012_2020_sd"]] / 
                temp_inc_tn_rev[["AdjChange2012_2020"]]
            ) ^ 2
        )
    ) ^ 2
) #@# Note: This leaves a lot of rows with NaN, presumably from dividing by 0 somewhere in the formula above, these NaN's are replaced with 0's below

#@# Note: Now that atm error has been calculated based on NDEP, it can be configured as it's own variable.
temp_inc_tn_rev$in_road <- with(temp_inc_tn_rev, il_tn_atm) #*#
temp_inc_tn_rev$sin_road <- with(temp_inc_tn_rev, ilse_tn_atm)

##### b. Final N data ------
inc_tn <- temp_inc_tn_rev %>%
  select(
    c(
      "comid",
      "in_ag",
      "in_urb",
      "in_poin",
      "in_graz",
      "in_road",
      "in_other",
      "sin_ag",
      "sin_urb",
      "sin_poin",
      "sin_graz",
      "sin_road",
      "sin_other"
    )
  ) %>%
  mutate(across(in_ag:sin_other, .fns = ~replace_na(., replace = 0)))


### III. SPARROW P uncertainty ------
temp_inc_tp <- sparrow_cons_out_tp %>% 
  select(c("comid","il_tp_chan":"il_tp_wwtp", "ilse_tp_chan":"ilse_tp_wwtp")) #*#

temp_inc_tp$ip_ag <- with(temp_inc_tp, il_tp_fer) #*#
temp_inc_tp$sip_ag <- with(temp_inc_tp, ilse_tp_fer ^ 2) #*#

temp_inc_tp$ip_poin <- with(temp_inc_tp, il_tp_wwtp) #*#
temp_inc_tp$sip_poin <- with(temp_inc_tp, ilse_tp_wwtp) #*#

temp_inc_tp$ip_urb <- with(temp_inc_tp, il_tp_urb) #*#
temp_inc_tp$sip_urb <- with(temp_inc_tp, ilse_tp_urb) #*#

temp_inc_tp$ip_graz <- with(temp_inc_tp, il_tp_graz) #*#
temp_inc_tp$sip_graz <- with(temp_inc_tp, ilse_tp_graz ) #*#

temp_inc_tp$ip_road <- 0
temp_inc_tp$sip_road <- 0

temp_inc_tp$ip_other <- with(temp_inc_tp, il_tp_geo + il_tp_chan + il_tp_spr) #*#
temp_inc_tp$sip_other <- with(temp_inc_tp, sqrt(ilse_tp_geo ^ 2 + ilse_tp_chan ^ 2 + ilse_tp_spr ^ 2)) #*#

inc_tp <- temp_inc_tp %>% 
  select(
    c(
      "comid",
      "ip_ag",
      "ip_poin",
      "ip_urb",
      "ip_graz",
      "ip_road",
      "ip_other",
      "sip_ag",
      "sip_poin",
      "sip_urb",
      "sip_graz",
      "sip_road",
      "sip_other"
    )
  ) %>%
  mutate(across(ip_ag:sip_other, .fns = ~replace_na(., replace = 0)))


### IV. WWTP baseline uncertainty ------

### Adjust 2012 SPARROW loadings for WWTPs based on percentage decreases to represent present conditions

wwtp_rem <- fread(paste(InPath,"WWTP_BaselineRemoval.csv", sep="")) %>%
  mutate(
    Plant_Name = ifelse(NPDES_ID == "WA0029181_WA0031682", "West Point WWTP/Alki Treatment Plant",
    ifelse(NPDES_ID == "WA0029181", "West Point WWTP",
    ifelse(NPDES_ID == "WA0030830", "Pacific Coast Coal Co",
    ifelse(NPDES_ID == "WA0029581", "King County South WWTP",
    ifelse(NPDES_ID == "WA0037087", "Tacoma Central WWTP",
    ifelse(NPDES_ID == "WA0042099", "Grand Mound WWTP",
    ifelse(NPDES_ID == "WA0037168", "Pullayup WPCP",
    ifelse(NPDES_ID == "WA0040479", "South Praire WWTP",
    ifelse(NPDES_ID == "WA0037061", "Lott Alliance Budd Inlet Treatment Plant",
    ifelse(NPDES_ID == "WA0040291", "Boston Harbor WWTP",
    ifelse(NPDES_ID == "WA0024031", "Lynnwood WWTP",
    ifelse(NPDES_ID == "WA0025569", "Paradise WWTP",
    ifelse(NPDES_ID == "WA0031682", "Alki Treatment Plant", Plant_Name))))))))))))) #@# Note: NPDESID with no names manually entered
  )

wwtp_rem_N <- wwtp_rem %>%
  select(
    State,
    Plant_Name,
    NPDES_ID,
    COMID,
    Rem_2012oroldest_2021ornewest_load_ch_TN,
    contains("load_kgyr_Nitrogen") 
  ) %>%
  pivot_longer(
    cols = contains("load_kgyr"),
    names_to = "Year",
    values_to = "load_kgyr",
    names_pattern = "(\\d+)"
  ) %>%
  filter(!(is.na(load_kgyr))) %>% #@# Note: Added this filter due to error in lm.fit function.
  mutate(Year = as.numeric(Year))

wwtp_rem_P <- wwtp_rem %>%
  select(
    State,
    Plant_Name,
    NPDES_ID,
    COMID,
    Rem_2012oroldest_2021ornewest_load_ch_TP,
    contains("load_kgyr_Phosphorus")
  ) %>%
  pivot_longer(
    cols = contains("load_kgyr"),
    names_to = "Year",
    values_to = "load_kgyr",
    names_pattern = "(\\d+)"
  ) %>%
  filter(!(is.na(load_kgyr))) %>% #@# Note: Added this filter due to error in lm.fit function.
  mutate(Year = as.numeric(Year))


##### a. Adjusted N removal efficiency uncertainty for point ------

uncertain_list <- list()
plant_ids <- unique(wwtp_rem_N$NPDES_ID)

invisible(
foreach(i = seq_along(unique(plant_ids))) %do% {
  rem_temp <- wwtp_rem_N %>%
    filter(NPDES_ID == plant_ids[[i]]) %>%
    group_by(COMID)
  
  suppressWarnings({
    rem_temp <- rem_temp %>%
      mutate(
        load_kgyr_first_se = predict(
          lm(load_kgyr ~ Year, data = rem_temp),
          newdata = data.frame(Year = first(rem_temp$Year):last(rem_temp$Year)),
          se.fit = TRUE
        )$se.fit[1],
        load_kgyr_last_se = predict(
          lm(load_kgyr ~ Year, data = rem_temp),
          newdata = data.frame(Year = first(rem_temp$Year):last(rem_temp$Year)),
          se.fit = TRUE
        )$se.fit[2],
        Rem_2012oroldest_2021ornewest_load_ch_TN_se = abs(Rem_2012oroldest_2021ornewest_load_ch_TN) *
          sqrt(
            (
              sqrt(load_kgyr_first_se ^ 2 + load_kgyr_last_se ^ 2) /
                (first(load_kgyr) - last(load_kgyr))
            ) ^ 2 +
              (load_kgyr_first_se / first(load_kgyr)) ^ 2
          )
      ) %>%
      ungroup()
  })
  
  uncertain_list[[i]] <- rem_temp
}
)

wwtp_rem_uncertain_N <- dplyr::bind_rows(uncertain_list)

inc_tn_rev <- merge(
  inc_tn,
  wwtp_rem_uncertain_N[, c("COMID","Rem_2012oroldest_2021ornewest_load_ch_TN", "Rem_2012oroldest_2021ornewest_load_ch_TN_se")], 
  by.x = "comid",
  by.y = "COMID",
  all.x = TRUE
) #*#

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[c("sin_poin")] <-  sqrt(
  inc_tn_rev[c("sin_poin")] ^ 2 + 
    inc_tn_rev[c("in_poin")] * 
    inc_tn_rev[["Rem_2012oroldest_2021ornewest_load_ch_TN"]] *
    sqrt(
      (inc_tn_rev[, c("sin_poin")] / inc_tn_rev[, c("in_poin")]) ^ 2 + 
        (
          inc_tn_rev[["Rem_2012oroldest_2021ornewest_load_ch_TN_se"]] /
            inc_tn_rev[["Rem_2012oroldest_2021ornewest_load_ch_TN"]]
        ) ^ 2
    )
)

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[,(ncol(inc_tn_rev)-1):ncol(inc_tn_rev)] <- NULL

##### b. Adjusted P removal efficiency uncertainty for point ------
uncertain_list <- list()
plant_ids <- unique(wwtp_rem_P$NPDES_ID)

invisible(
foreach(i = seq_along(unique(plant_ids))) %do% {
  rem_temp <- wwtp_rem_P %>%
    filter(NPDES_ID == plant_ids[[i]]) %>%
    group_by(COMID)
  
  suppressWarnings({
    rem_temp <- rem_temp %>%
      mutate(
        load_kgyr_first_se = predict(
          lm(load_kgyr ~ Year, data = rem_temp),
          newdata = data.frame(Year = first(rem_temp$Year):last(rem_temp$Year)),
          se.fit = TRUE
        )$se.fit[1],
        load_kgyr_last_se = predict(
          lm(load_kgyr ~ Year, data = rem_temp),
          newdata = data.frame(Year = first(rem_temp$Year):last(rem_temp$Year)),
          se.fit = TRUE
        )$se.fit[2],
        Rem_2012oroldest_2021ornewest_load_ch_TP_se = abs(Rem_2012oroldest_2021ornewest_load_ch_TP) *
          sqrt(
            (
              sqrt(load_kgyr_first_se ^ 2 + load_kgyr_last_se ^ 2) /
                (first(load_kgyr) - last(load_kgyr))
            ) ^ 2 +
              (load_kgyr_first_se / first(load_kgyr)) ^ 2
          )
      ) %>%
      ungroup()
  })
  
  uncertain_list[[i]] <- rem_temp
}
)

wwtp_rem_uncertain_P <- dplyr::bind_rows(uncertain_list)

inc_tp_rev <- merge(
  inc_tp,
  wwtp_rem_uncertain_P[, c("COMID","Rem_2012oroldest_2021ornewest_load_ch_TP", "Rem_2012oroldest_2021ornewest_load_ch_TP_se")], 
  by.x = "comid",
  by.y = "COMID",
  all.x = TRUE
) #*#

inc_tp_rev[is.na(inc_tp_rev)] <- 0

inc_tp_rev[c("sip_poin")] <-  sqrt(
  inc_tp_rev[c("sip_poin")] ^ 2 + 
    inc_tp_rev[c("ip_poin")] * 
    inc_tp_rev[["Rem_2012oroldest_2021ornewest_load_ch_TP"]] *
    sqrt(
      (inc_tp_rev[, c("sip_poin")] / inc_tp_rev[, c("ip_poin")]) ^ 2 + 
        (
          inc_tp_rev[["Rem_2012oroldest_2021ornewest_load_ch_TP_se"]] /
            inc_tp_rev[["Rem_2012oroldest_2021ornewest_load_ch_TP"]]
        ) ^ 2
    )
)

inc_tp_rev[is.na(inc_tp_rev)] <- 0

inc_tp_rev[,(ncol(inc_tp_rev)-1):ncol(inc_tp_rev)] <- NULL

### V. Delivery fractions ------

# Adjust delivery fraction to terminal COMID

##### a. Propagate error in delivery fraction ------
max_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {

      sparrow_cons_out_tp %>%
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
    select(c("comid", "DEL_FRAC", "SE_DEL_FRAC"))
  
}

invisible(
  foreach(i = seq_along(temp_delfrac_rev_tn)) %do% {
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[[i]][1]
    )
    temp_delfrac_rev_tn[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], 
      (DEL_FRAC / max_delfrac_tn[[i]][1]) *
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
            (max_delfrac_tn[[i]][2] / max_delfrac_tn[[i]][1]) ^ 2
        )
    )
    temp_delfrac_rev_tn[[i]]$se_delfrac_rev <- ifelse(is.na(temp_delfrac_rev_tn[[i]]$se_delfrac_rev), 0, temp_delfrac_rev_tn[[i]]$se_delfrac_rev)
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  temp_delfrac_rev_tn[[i]] %>%
    select(c("comid", "delfrac_rev", "se_delfrac_rev"))
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
    select(c("comid", "DEL_FRAC", "SE_DEL_FRAC"))
  
}

invisible(
  foreach(i = seq_along(temp_delfrac_rev_tp)) %do% {
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[[i]][1]
    )
    temp_delfrac_rev_tp[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], 
      (DEL_FRAC / max_delfrac_tp[[i]][1]) *
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 +
            (max_delfrac_tp[[i]][2] / max_delfrac_tp[[i]][1]) ^ 2
        )
    )
    temp_delfrac_rev_tp[[i]]$se_delfrac_rev <- ifelse(is.na(temp_delfrac_rev_tp[[i]]$se_delfrac_rev), 0, temp_delfrac_rev_tp[[i]]$se_delfrac_rev)
  }
)

delfrac_rev_tp <- foreach(i = seq_along(temp_delfrac_rev_tp)) %do% {
  temp_delfrac_rev_tp[[i]] %>%
    select(c("comid", "delfrac_rev", "se_delfrac_rev"))
}

##### b. Uncertainty in delivered baseloads ------

# Multiply all loads by revised del_frac 

temp_inc_tn_dat <- foreach(i = 1:length(delfrac_rev_tn)) %do% {
  
  
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% delfrac_rev_tn[[i]]$comid,],
    delfrac_rev_tn[[i]],
    by = "comid",
    all.x = TRUE
  )
  
  temp_inc_tn_dat_tmp[2:7] <- temp_inc_tn_dat_tmp[2:7] * 
    temp_inc_tn_dat_tmp[["delfrac_rev"]]
  temp_inc_tn_dat_tmp[8:13] <- temp_inc_tn_dat_tmp[2:7] * 
    temp_inc_tn_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tn_dat_tmp[8:13] / temp_inc_tn_dat_tmp[2:7]) ^ 2 +
        (
          temp_inc_tn_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tn_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
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
  )
  
  temp_inc_tp_dat_tmp[2:7] <- temp_inc_tp_dat_tmp[2:7] * 
    temp_inc_tp_dat_tmp[["delfrac_rev"]]
  temp_inc_tp_dat_tmp[8:13] <- temp_inc_tp_dat_tmp[2:7] * 
    temp_inc_tp_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tp_dat_tmp[8:13] / temp_inc_tp_dat_tmp[2:7]) ^ 2 +
        (
          temp_inc_tp_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tp_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
  temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
    order(temp_inc_tp_dat_tmp$comid), 
  ]
  
}


### VI. Uncertainty in runoff coefficients ------

# Calculate runoff coefficient for urban area

# Specify column for impervious dataset

temp_runoffcoeff <- imperv %>%
  select(c("comid", "PctImp2011Cat")) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>%
  #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.) The PctImp2011Cat came from NLCD, possibly the impervious dataset, not the larger categorical one. I'm assuming the accuracy is the same regardless. https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/Documentation/Metadata/ImperviousSurfaces2016.html
  mutate(PctImp2011Cat_se = PctImp2011Cat * cv_numpixels_nlcd) %>%
  as.data.frame()


temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2011Cat
) #*#

#@# Note: This equation comes from Schueler 1987, Table A6 & Figure 1-2. 
Schueler.table <- data.frame(
  PercentImpervious = c(
    41, 38, 24, 33, 33, 19, 29, 29, 76, 20, 22, 38, 29, 50, 57, 21, 18, 37, 37, 22, 17, 27, 21, 34, 58, 81, 23, 5, 6, 69, 99, 91, 69, 21, 99, 90, 4, 1, 11, 7, 55, 34, 90, 22
  ),
  Mean = c(
    0.35, 0.18, 0.16, 0.46, 0.25, 0.19, 0.47, 0.24, 0.56, 0.24, 0.17, 0.22, 0.2, 0.37, 0.43, 0.17, 0.18, 0.24, 0.37, 0.99, 0.19, 0.11, 0.26, 0.28, 0.73, 0.82, 0.28, 0.11, 0.02, 0.65, 0.98, 0.99, 0.9, 0.21, 0.92, 0.74, 0.08, 0.12, 0.05, 0.08, 0.57, 0.47, 0.75, 0.42
  )
)

temp_runoffcoeff$runoffcoeff_se <- (predict(
  with(Schueler.table, lm(Mean ~ PercentImpervious)),
  newdata = data.frame(PercentImpervious = temp_runoffcoeff$PctImp2011Cat),
  se.fit = TRUE
)$se.fit) / sqrt(nrow(Schueler.table))

runoffcoeff <- temp_runoffcoeff %>%
  select(c("comid", "runoffcoeff", "runoffcoeff_se"))
runoffcoeff <- distinct(runoffcoeff) # Removes duplicates

### VII. Uncertainty in other loads ------

# (SC for StreamCat)
temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid,
  ] %>%
    mutate(across(everything(), .fns = ~ replace_na(., replace = 0)))
}

temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid),
  ] %>%
    mutate(across(everything(), .fns = ~ replace_na(., replace = 0)))
}

temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
  ] %>%
    mutate(across(everything(), .fns = ~ replace_na(., replace = 0)))
}

temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
  ] %>%
    mutate(across(everything(), .fns = ~ replace_na(., replace = 0)))
}

param_other_loads_tn <- foreach(i = 1:length(temp_inc_tn_dat_other)) %do% {
  list(
    param = sum(temp_inc_tn_dat_SC[[i]]$in_other) +
      sum(
        if (nrow(temp_inc_tn_dat_other[[i]]) > 0) {
          temp_inc_tn_dat_other[[i]][
            , c("in_poin", "in_urb", "in_ag", "in_road", "in_graz", "in_other")
          ]
        } else {
          c(0)
        },
        na.rm = TRUE
      ),
    se = sqrt(
      sum(temp_inc_tn_dat_SC[[i]]$sin_other ^ 2) +
        sum(
          if (nrow(temp_inc_tn_dat_other[[i]]) > 0) {
            temp_inc_tn_dat_other[[i]][
              , c("sin_poin", "sin_urb", "sin_ag", "sin_road", "sin_graz", "sin_other")
            ] ^ 2
          } else {
            0
          },
          na.rm = TRUE
        )
    )
  )
}


param_other_loads_tp <- foreach(i = 1:length(temp_inc_tp_dat_other)) %do% {
  list(
    param = sum(temp_inc_tp_dat_SC[[i]]$ip_other) +
      sum(
        if (nrow(temp_inc_tp_dat_other[[i]]) > 0) {
          temp_inc_tp_dat_other[[i]][
            , c("ip_poin", "ip_urb", "ip_ag", "ip_road", "ip_graz", "ip_other")
          ]
        } else {
          c(0)
        },
        na.rm = TRUE
      ),
    se = sqrt(
      sum(temp_inc_tp_dat_SC[[i]]$sip_other ^ 2) +
        sum(
          if(nrow(temp_inc_tp_dat_other[[i]]) > 0) {
            temp_inc_tp_dat_other[[i]][
              , c("sip_poin", "sip_urb", "sip_ag", "sip_road", "sip_graz", "sip_other")
            ] ^ 2
          } else {
            0
          },
          na.rm = TRUE
        )
    )
  )
}

### VIII. Format loads and coefficients ------

##### a. Format baseline loading se -----

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tn_dat_se <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
    temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
    
    if(nrow(temp_inc_tn_dat_opt) > 0) {
      temp_inc_tn_dat_opt$comid_form <- paste0("'", temp_inc_tn_dat_opt$comid, "'")
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_opt %>%
      select(c("comid_form", "sin_poin", "sin_urb", "sin_ag", "sin_road", "sin_graz")) %>%
      mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
  }
    temp_inc_tn_dat_opt$comid_form <- temp_inc_tn_dat_opt$comid
    temp_inc_tn_dat_opt %>%
      select(c("comid_form", "sin_poin", "sin_urb", "sin_ag", "sin_road", "sin_graz"))
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tp_dat_se <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
    temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
    
    if(nrow(temp_inc_tp_dat_opt) > 0) {
      temp_inc_tp_dat_opt$comid_form <- paste0("'", temp_inc_tp_dat_opt$comid, "'")
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_opt %>%
        select(c("comid_form", "sip_poin", "sip_urb", "sip_ag", "sip_road", "sip_graz")) %>%
        mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
    }
    temp_inc_tp_dat_opt$comid_form <- temp_inc_tp_dat_opt$comid
    temp_inc_tp_dat_opt %>%
      select(c("comid_form", "sip_poin", "sip_urb", "sip_ag", "sip_road", "sip_graz"))
  }
}

##### b. Format runoff coefficient -----

temp_runoffcoeff_dat <- runoffcoeff[
  runoffcoeff$comid %in% streamcat_subset_all$comid,
]
temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat_se <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff_se"))


### IX. Riparian Buffers ------

##### a. Uncertainty in riparian loadings -----

riparian.loadings_tn <- foreach(i = 1:length(target_selection)) %do% {
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])

  riparian.loadings %>%
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, N_riparian_kgyr, N_riparian_kgyr_se) %>%
    right_join(
      .,
      sparrow_cons_out_tn %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        select(comid, il_tn, ilse_tn),
      by = "comid"
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > il_tn ~ il_tn,
        is.na(N_riparian_kgyr) ~ 0,
        TRUE ~ N_riparian_kgyr
      ),
      N_riparian_kgyr_se = case_when(
        N_riparian_kgyr > il_tn ~ ilse_tn,
        is.na(N_riparian_kgyr_se) ~ 0,
        TRUE ~ N_riparian_kgyr_se
      )
    ) %>%
    select(-il_tn, -ilse_tn)
}

riparian.loadings_tp <- foreach(i = 1:length(target_selection)) %do% {

  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])

  riparian.loadings %>%
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, P_riparian_kgyr, P_riparian_kgyr_se) %>%
    right_join(
      .,
      sparrow_cons_out_tp %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        select(comid, il_tp, ilse_tp),
      by = "comid"
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > il_tp ~ il_tp,
        is.na(P_riparian_kgyr) ~ 0,
        TRUE ~ P_riparian_kgyr
      ),
      P_riparian_kgyr_se = case_when(
        P_riparian_kgyr > il_tp ~ ilse_tp,
        is.na(P_riparian_kgyr_se) ~ 0,
        TRUE ~ P_riparian_kgyr_se
      )
    )  %>%
    select(-il_tp, -ilse_tp)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_riparian_tn_dat <- foreach(i = 1:length(inc_tn_dat)) %do% {
    
    
    temp_riparian_tn_dat_tmp <- merge(
      riparian.loadings_tn[[i]],
      delfrac_rev_tn[[i]],
      by = "comid",
      all = TRUE
    ) 
    
    if (length(temp_riparian_tn_dat_tmp > 0)) {
      temp_riparian_tn_dat_tmp <- temp_riparian_tn_dat_tmp %>%
      rowwise() %>%
      mutate(
        in_riparian = N_riparian_kgyr * delfrac_rev,
        sin_riparian = case_when(
          N_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
            vals = list(c(N_riparian_kgyr, N_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
            method = "mult"
          ),
          N_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
        )
      ) %>%
      arrange(comid) %>%
      select(comid, in_riparian, sin_riparian) %>%
      ungroup()
    } else {
      temp_riparian_tn_dat_tmp %>%
        select(comid, in_riparian = N_riparian_kgyr, sin_riparian = N_riparian_kgyr_se)
    }
    
  }
}



if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_riparian_tp_dat <- foreach(i = 1:length(inc_tp_dat)) %do% {
    
    temp_riparian_tp_dat_tmp <- merge(
      riparian.loadings_tp[[i]],
      delfrac_rev_tp[[i]],
      by = "comid",
      all = TRUE
    ) 
    if (length(temp_riparian_tp_dat_tmp > 0)) {
      temp_riparian_tp_dat_tmp <- temp_riparian_tp_dat_tmp %>%
      rowwise() %>%
      mutate(
        ip_riparian = P_riparian_kgyr * delfrac_rev,
        sip_riparian = case_when(
          P_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
            vals = list(
              c(P_riparian_kgyr, P_riparian_kgyr_se), 
              c(delfrac_rev, se_delfrac_rev)
            ), 
            method = "mult"
          ),
          P_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
        )
      ) %>%
      arrange(comid) %>%
      select(comid, ip_riparian, sip_riparian) %>%
      ungroup()
    } else {
      temp_riparian_tp_dat_tmp %>%
        select(comid, ip_riparian = P_riparian_kgyr, sip_riparian = P_riparian_kgyr_se)
    }
  } 
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    temp_riparian_tn_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, in_riparian, sin_riparian)
    
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
    
    temp_riparian_tp_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, ip_riparian, sip_riparian)
    
  }
}

##### b. Updated riparian buffer removal efficiencies with uncertainty -----

if(length(RiparianBuffer_BMPs) > 0) {
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    
    riparian_buffer_efficiencies_N <- foreach(
      i = 1: length(streamcat_subset_tn)
    ) %do% {
      
      riparian_buffer_efficiencies_N_tmp <- foreach(
        j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
      ) %do% {
        
        short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
          "Grass"
        } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
        
        riparian.efficiencies.wat <- riparian.efficiencies %>%
          filter(comid %in% streamcat_subset_tn[[i]]$comid) 
        
        if(nrow(riparian.efficiencies.wat) > 0) {
          
          riparian.efficiencies.tmp <- riparian.efficiencies.wat %>%
            select(
              -contains("P_"),
              -!contains(short.form.bmp.name),
              -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
              -!contains("uncertainty"),
              comid
            ) %>%
            rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
            mutate(
              x = RiparianBuffer_Widths[j],
              expression = strsplit(
                str_extract(
                  mapply(
                    gsub,
                    pattern = "x",
                    replacement = x,
                    x = mapply(
                      gsub,
                      pattern = "'",
                      replacement = "",
                      x = Curve.Form
                    )
                  ),
                  pattern = "(?<=c[(]).*(?=[)])"
                ),
                ","
              )
            ) %>%
            rowwise() %>%
            mutate(
              iter = 1,
              effic = list(
                lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
              ),
              effic_mean = suppressWarnings(mean(unlist(effic))),
              effic_sd = sd(unlist(effic))
            ) %>%
            select(comid, effic_mean, effic_sd) %>%
            rename_with(
              .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
              .cols = contains("effic")
            ) %>%
            ungroup()
        } else {
          riparian.efficiencies.tmp <- riparian.efficiencies.wat %>%
            select(
              -contains("P_"),
              -!contains(short.form.bmp.name),
              -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
              -!contains("uncertainty"),
              comid
            ) %>%
            mutate(effic_mean = NA,
                   effic_sd = NA) %>%
            select(comid, effic_mean, effic_sd) %>%
            rename_with(
              .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
              .cols = contains("effic")
            ) %>%
            ungroup() 
        }
      }
      
      riparian_buffer_efficiencies_N_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
          
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_buffer_efficiencies_P <- foreach(
      i = 1: length(streamcat_subset_tp)
    ) %do% {
      riparian_buffer_efficiencies_P_tmp <- foreach(
        j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
      ) %do% {
        short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
          "Grass"
        } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
        
        riparian.efficiencies.wat <- riparian.efficiencies %>%
          filter(comid %in% streamcat_subset_tp[[i]]$comid) 
        
        if (nrow(riparian.efficiencies.wat) > 0) {
          riparian.efficiencies.tmp <- riparian.efficiencies.wat %>%
            select(
              -contains("N_"),
              -!contains(short.form.bmp.name),
              -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
              -!contains("uncertainty"),
              comid
            ) %>%
            rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
            mutate(
              x = RiparianBuffer_Widths[j],
              expression = strsplit(
                str_extract(
                  mapply(
                    gsub,
                    pattern = "x", 
                    replacement = x,
                    x = mapply(
                      gsub,
                      pattern = "'", 
                      replacement = "", 
                      x = Curve.Form
                    )
                  ), 
                  pattern = "(?<=c[(]).*(?=[)])"
                ), 
                ","
              )
            ) %>%
            rowwise() %>%
            mutate(
              iter = 1,
              effic = list(
                lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
              ),
              effic_mean = suppressWarnings(mean(unlist(effic))),
              effic_sd = sd(unlist(effic))
            ) %>%
            select(comid, effic_mean, effic_sd) %>%
            rename_with(
              .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
              .cols = contains("effic")
            ) %>%
            ungroup()
        } else {
          riparian.efficiencies.tmp <- riparian.efficiencies.wat %>%
            select(
              -contains("N_"),
              -!contains(short.form.bmp.name),
              -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
              -!contains("uncertainty"),
              comid
            ) %>%
            mutate(effic_mean = NA,
                   effic_sd = NA) %>%
            select(comid, effic_mean, effic_sd) %>%
            rename_with(
              .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
              .cols = contains("effic")
            ) %>%
            ungroup()
        }
      }
      
      riparian_buffer_efficiencies_P_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .))
          
        )
    }
  }
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_N)) %do% {
      riparian_buffer_efficiencies_N[[i]] <- riparian_buffer_efficiencies_N[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_P)) %do% {
      riparian_buffer_efficiencies_P[[i]] <- riparian_buffer_efficiencies_P[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
##### c. Uncertainty in loading per bank-ft -----
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_loadingperbankft_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      tmp1 <- left_join(
        riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          N_riparian_kgyr, 
          N_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) 
      if (length(tmp1$comid > 0)) {
      tmp1 <- tmp1 %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            N_riparian_kgyr <= 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(N_riparian_kgyr, N_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      
      } else {
        tmp1$loading_per_bankft_kg_ftyr <- NA
        tmp1$loading_per_bankft_kg_ftyr_se <- NA
      }
      tmp1 
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_loadingperbankft_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      tmp1 <- left_join(
        riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          P_riparian_kgyr, 
          P_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) 
      if (length(tmp1$comid) > 0) {
      tmp1 <- tmp1 %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            P_riparian_kgyr == 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(P_riparian_kgyr, P_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      } else {
        tmp1$loading_per_bankft_kg_ftyr <- NA
        tmp1$loading_per_bankft_kg_ftyr_se <- NA
      }
      tmp1
      
    }
  }

  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    temp_riparian_tn_removal <- foreach(i = seq_along(inc_tn_dat)) %do% {
      temp_riparian_tn_removal_tmp <- merge(
        riparian_buffer_removal_N[[i]],
        delfrac_rev_tn[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        pivot_longer(
          cols = -c(comid, delfrac_rev, se_delfrac_rev), 
          names_to = c("RiparianBMP"),
          values_to = "removal"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
        rowwise() %>%
        mutate(
               removal = case_when(
                 removal == -999 ~ 0, removal >= 0 ~ (removal * delfrac_rev)
               ),
               se = NA)
      if (length(temp_riparian_tn_removal_tmp$comid) > 0) {
        temp_riparian_tn_removal_tmp <- temp_riparian_tn_removal_tmp %>%
          mutate(
            se = if (removal == 0 | delfrac_rev == 0) {
              0
            } else {
              case_when(
                removal == -999 ~ 0,
                removal > 0 ~ my_propogateerror(
                  vals = list(
                    c(removal, se),
                    c(delfrac_rev, se_delfrac_rev)
                  ),
                  method = "mult"
                )
            )
          }
        ) %>%
        pivot_wider(id_cols = c("comid"),
                    names_from = "RiparianBMP",
                    values_from = c("removal", "se")) %>%
          arrange(comid) %>%
          rename_with(
            .fn = ~gsub(., pattern = "removal_", replacement = ""), 
            .cols = contains("removal")
          ) %>%
          select(comid, contains(RiparianBuffer_BMPs)) %>%
          ungroup()
      } else {
        foreach(i = seq_along(RiparianBuffer_BMPs)) %do% {
          newvar <- sym(paste0(RiparianBuffer_BMPs[i]))
          newvar_se <- sym(paste0("se_", RiparianBuffer_BMPs[i]))
          temp_riparian_tn_removal_tmp <- temp_riparian_tn_removal_tmp %>%
            mutate(
              !!newvar := NA,
              !!newvar_se := NA
            )
        }
        temp_riparian_tn_removal_tmp <- temp_riparian_tn_removal_tmp %>%
          select(comid, contains(RiparianBuffer_BMPs))
      }
    }
  }

  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    temp_riparian_tp_removal <- foreach(i = seq_along(inc_tp_dat)) %do% {
      temp_riparian_tp_removal_tmp <- merge(
        riparian_buffer_removal_P[[i]],
        delfrac_rev_tp[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        pivot_longer(
          cols = -c(comid, delfrac_rev, se_delfrac_rev),
          names_to = c("RiparianBMP"),
          values_to = "removal"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
        rowwise() %>%
        mutate(
               removal = case_when(
                 removal == -999 ~ 0, removal >= 0 ~ (removal * delfrac_rev)
               ),
               se = NA)
      if (length(temp_riparian_tp_removal_tmp$comid) > 0) {
        temp_riparian_tp_removal_tmp <- temp_riparian_tp_removal_tmp %>%
          mutate(
            se = if (removal == 0 | delfrac_rev == 0) {
              0
            } else {
              case_when(
                removal == -999 ~ 0, 
                removal >= 0 ~ my_propogateerror(
                  vals = list(
                    c(removal, se),
                    c(delfrac_rev, se_delfrac_rev)
                  ),
                  method = "mult"
                )
              )
            }
          ) %>%
          pivot_wider(id_cols = c("comid"),
                      names_from = "RiparianBMP",
                      values_from = c("removal", "se")) %>%
          arrange(comid) %>%
          rename_with(
            .fn = ~gsub(., pattern = "removal_", replacement = ""),
            .cols = contains("removal")
          ) %>%
          select(comid, contains(RiparianBuffer_BMPs)) %>%
          ungroup()
      } else {
        foreach(i = seq_along(RiparianBuffer_BMPs)) %do% {
          newvar <- sym(paste0(RiparianBuffer_BMPs[i]))
          newvar_se <- sym(paste0("se_", RiparianBuffer_BMPs[i]))
          temp_riparian_tp_removal_tmp <- temp_riparian_tp_removal_tmp %>%
            mutate(
              !!newvar := NA,
              !!newvar_se := NA
            )
        }
        temp_riparian_tp_removal_tmp <- temp_riparian_tp_removal_tmp %>%
          select(comid, contains(RiparianBuffer_BMPs))
      }
    }
  }

  riparian_tn_removal_se <- foreach(i = seq_along(temp_riparian_tn_removal)) %do% {
    
    temp_riparian_tn_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, contains("se_")) %>%
      rename_with(.fn = ~gsub("se_", "", .), .cols = -comid_form) %>%
      mutate(across(.cols = any_of(RiparianBuffer_BMPs), .fns = ~case_when(is.na(.) ~0, . == -999 ~ 0, . >= 0 ~ .)))
    
  }
  
  riparian_tp_removal_se <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
    
    temp_riparian_tp_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, contains("se_")) %>%
      rename_with(.fn = ~gsub("se_", "", .), .cols = -comid_form) %>%
      mutate(across(any_of(RiparianBuffer_BMPs), ~case_when(is.na(.) ~0, . == -999 ~ 0, . >= 0 ~ .)))
    
  }
}

##### e.Point Source BMPs effic ------
if(length(Point_BMPs) > 0) {
  temp_point_effic <- fread(
    paste(InPath, "WWTP_RemovalEffic.csv", sep = "")
  ) #*#
  
  temp_bmp_effic_point_se <- merge(
    temp_point_effic, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(COMID, State, Plant_Name, NPDES_ID, contains("_se")) %>%
    pivot_longer(
      cols = -c(COMID, State, Plant_Name, NPDES_ID), 
      names_to = c("BMP", "Nutrient"), 
      names_pattern = "(.*)_(TN|TP)_Efficiency",
      values_to = "Efficiency_se"
    )
  
  temp_bmp_effic_point_selected <- temp_bmp_effic_point_se %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_effic_point_se$BMP, "_"), 
            .f = ~(all(.x %in% UserSelected_pointBMPs$BMP)))
        )
      )
    ) 
  
  point_effic_se <- temp_bmp_effic_point_selected %>%
    mutate(across(contains("Efficiency_se"), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
  
  point_effic_bycomid_tn_se <- point_effic_se %>% 
    filter(Nutrient == "TN") %>%
    select(comid = COMID, BMP, effic_se = Efficiency_se) #%>%
    # mutate(comid_form = paste0("'", comid, "'")) %>%
    # select(comid_form, BMP, effic_se) %>%
    # pivot_wider(., id_cols = comid_form, names_from = BMP, values_from = effic_se)
  
  point_effic_bycomid_tp_se <- point_effic_se %>% 
    filter(Nutrient == "TP") %>%
    select(comid = COMID, BMP, effic_se = Efficiency_se) #%>%
    # mutate(comid_form = paste0("'", comid, "'")) %>%
    # select(comid_form, BMP, effic_se) %>%
    # pivot_wider(., id_cols = comid_form, names_from = BMP, values_from = effic_se)
  
} else {
  point_effic_bycomid_tn_se <- data.frame(
    comid = NA, BMP = NA, effic_se = NA
  )
  point_effic_bycomid_tp_se <- data.frame(
    comid = NA, BMP = NA, effic_se = NA
  )
}

# Format point source BMP efficiency data

if(length(Point_BMPs) > 0) {
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    temp_point_effic_dat_tn_se <- point_effic_bycomid_tn_se %>%
      filter(comid %in% unlist(streamcat_subset_tn)) %>% 
      pivot_wider(id_cols = comid, names_from = BMP, values_from = effic_se) %>%
      select(comid, any_of(Point_BMPs)) # forces the order
    
    temp_point_effic_other_tn_se <- data.frame(
      comid = unlist(
        streamcat_subset_tn, use.names = FALSE
      )[
        !(
          unlist(streamcat_subset_tn, use.names = FALSE) %in% 
            temp_point_effic_dat_tn_se$comid
        )
      ]
    )
    
    temp_point_effic_other_tn_se[2:(1+length(Point_BMPs))] <- 0
    names(temp_point_effic_other_tn_se) <- c("comid", Point_BMPs)
    
    if(nrow(temp_point_effic_dat_tn_se) > 0) {
      temp_point_effic_dat_rev_tn_se <- rbind(
        temp_point_effic_dat_tn_se, temp_point_effic_other_tn_se
      ) %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    } else {
      temp_point_effic_dat_rev_tn_se <- temp_point_effic_other_tn_se %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    }
    
    point_effic_dat_tn_se <- temp_point_effic_dat_rev_tn_se %>% 
      select(comid_form, all_of(Point_BMPs)) %>%
      unique() # function used to ensure duplicates don't creep in
  }
  
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    
    temp_point_effic_dat_tp_se <- point_effic_bycomid_tp_se %>%
      filter(comid %in% unlist(streamcat_subset_tp)) %>% 
      pivot_wider(id_cols = comid, names_from = BMP, values_from = effic_se) %>%
      select(comid, any_of(Point_BMPs)) # forces the order
    
    temp_point_effic_other_tp_se <- data.frame(
      comid = unlist(
        streamcat_subset_tp, use.names = FALSE
      )[
        !(
          unlist(streamcat_subset_tp, use.names = FALSE) %in% 
            temp_point_effic_dat_tp_se$comid
        )
      ]
    )
    
    temp_point_effic_other_tp_se[2:(1+length(Point_BMPs))] <- 0
    names(temp_point_effic_other_tp_se) <- c("comid", Point_BMPs)
    
    if(nrow(temp_point_effic_dat_tp_se) > 0) {
      temp_point_effic_dat_rev_tp_se <- rbind(
        temp_point_effic_dat_tp_se, temp_point_effic_other_tp_se
      ) %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    } else {
      temp_point_effic_dat_rev_tp_se <- temp_point_effic_other_tp_se %>% 
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
    }
    
    point_effic_dat_tp_se <- temp_point_effic_dat_rev_tp_se %>% 
      select(comid_form, all_of(Point_BMPs)) %>%
      unique() # function used to ensure duplicates don't creep in
  }
}

### X. Area uncertainty -----

##### a. Urban area uncertainty -----

# Select urban area and incremental area from SPARROW input data

temp_sparrow_area <- sparrow_in %>%
  select(comid, IncAreaKm2, urban_km2 = developed11_km2) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(cv_km2_nlcd = cv_numpixels_nlcd) %>%
  mutate(urban_km2_se = urban_km2 * cv_km2_nlcd) %>%
  mutate(uncertainty_boundary_wdb_m = 12.2) %>% #@# Note: The 8-, 10- and 12-digit hydrologic units must be delineated from and georeferenced to a minimum horizontal accuracy of 1:24,000 scale in the United States, except for Alaska at 1:63,360 scale and 1:25,000 scale in the Caribbean, to meet the National Standard for Spatial Data Accuracy (Federal Geographic Data Committee, 1998b). For example, to quantify 1:24,000-scale horizontal accuracy as it applies to the delineation of WBD-compliant hydrologic units, a hydrologic unit boundary must fall within a buffer of 40 feet or 12.2 meters of a well-defined point on a 1:24,000-scale topographic map. Geospatial positioning accuracy standards are defined and stated in documents by the Federal Geographic Data Committee (1998b).<https://pubs.usgs.gov/tm/11/a3/pdf/tm11-a3_4ed.pdf>
  mutate(
    uncertainty_wdb_boundary_length_m = my_propogateerror( #  error in each direction for the start & end point
      list(
        c(1, uncertainty_boundary_wdb_m),
        c(1, uncertainty_boundary_wdb_m),
        c(1, uncertainty_boundary_wdb_m),
        c(1, uncertainty_boundary_wdb_m)
      ), 
      method = "addsub"
    ),
    approx_wdb_boundary_length_m = sqrt(IncAreaKm2 * 1000000), # Assume a square
  ) %>%
  rowwise() %>%
  mutate(
    uncertainty_wdb_area_m2 = if(
      approx_wdb_boundary_length_m == 0
    ) {0} else { my_propogateerror(
      vals = list(
        c(approx_wdb_boundary_length_m, uncertainty_wdb_boundary_length_m),
        c(approx_wdb_boundary_length_m, uncertainty_wdb_boundary_length_m)
      ),
      method = "mult"
    )
    },
    IncAreaKm2_se = uncertainty_wdb_area_m2 / 1000000
  ) %>%
  ungroup()

km2_to_ac <- 1/247.105

temp_sparrow_area$urban_ac <- temp_sparrow_area$urban_km2/km2_to_ac #*#
temp_sparrow_area$urban_ac_se <- temp_sparrow_area$urban_km2_se/km2_to_ac #*#
temp_sparrow_area$inc_ac <- temp_sparrow_area$IncAreaKm2/km2_to_ac #*#
temp_sparrow_area$inc_ac_se <- temp_sparrow_area$IncAreaKm2_se/km2_to_ac #*#

#@# Note: Generate urban areas by land use type and soil group using proportions and streamcat data
urban_areas_tmp <- temp_sparrow_area %>%
  select(comid, urban_ac, urban_ac_se) %>%
  merge(., urban.proportions)

urban_areas <- urban_areas_tmp %>%
  mutate(urban_ac = urban_ac * area_port, 
         urban_ac_se = urban_ac_se * area_port) %>%
  select(comid, Land_Use, HSG, urban_ac, urban_ac_se)

urban_areas <- urban_areas %>%
  mutate(urban_ac = drop_units(urban_ac),
         urban_ac_se = drop_units(urban_ac_se))

urban_area_dat <- urban_areas %>%
  mutate(comid_form = paste0("'",comid,"_",Land_Use,"_",HSG,"'")) %>%
  select(comid, comid_form, urban_ac_se)

##### b. Cropland area uncertainty -----

# Select percentage of incremental area that is cropland

streamcat_crop <- cropland %>% select(c("comid", "PctCrop2011Cat", "PctHay2011Cat")) %>% 
  mutate(cv_numpixels_nlcd = 0.17) %>% #@# Note: Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(PctCrop2011Cat_se = PctCrop2011Cat * cv_numpixels_nlcd,
         PctHay2011Cat_se = PctHay2011Cat * cv_numpixels_nlcd) #*# 

temp_area <- merge(
  temp_sparrow_area, streamcat_crop, by = c("comid"), all.y = TRUE
) %>%
  mutate(
    ag_ac = (PctCrop2011Cat / 100) * inc_ac,
    graz_ac = (PctHay2011Cat / 100) * inc_ac
  ) %>%
  rowwise() %>%
  mutate(
    ag_ac_se = case_when(
      PctCrop2011Cat > 0 & inc_ac > 0 ~ my_propogateerror(
        list(
          c(PctCrop2011Cat / 100, PctCrop2011Cat_se / 100),
          c(inc_ac, inc_ac_se)
        ),
        method = "mult"
      ),
      PctCrop2011Cat <= 0 | inc_ac <= 0 ~ 0
    ),
    graz_ac_se = case_when(
      PctHay2011Cat > 0 & inc_ac > 0 ~ my_propogateerror(
        list(
          c(PctHay2011Cat / 100, PctHay2011Cat_se / 100),
          c(inc_ac, inc_ac_se)
        ),
        method = "mult"
      ),
      PctHay2011Cat <= 0 | inc_ac <= 0 ~ 0
    )
  ) %>%
  ungroup()

##### c. Format area uncertainty -----
area <- temp_area %>% 
  select(c("comid", "ag_ac", "graz_ac", "ag_ac_se", "graz_ac_se")) %>%
  group_by(comid) %>%
  summarize(across(contains("ac"), .fns = ~mean(.))) # Removes duplicates

##### d. Read in the Ag baseline BMP % -----

ag_bmp_baseline_perc   <- fread(paste(InPath, "USGS_AgBMP_PugetSound.csv", sep="")) #*#

# Calculate Ag baseline BMP Area
area <- area %>%
  merge(., ag_bmp_baseline_perc, by = "comid") %>%
  mutate(ag_ac_bmpadj = ag_ac - ((TOT_IMPV12_PERC / 100) * ag_ac)) %>%
  select(-TOT_IMPV12_PERC, -ag_ac) %>%
  relocate(ag_ac_bmpadj, .after = comid)

distinct_area <- distinct(area) # Removes duplicates

temp_area_dat <- distinct_area[distinct_area$comid %in% reaches_all$comid,]
temp_area_dat <- temp_area_dat[order(temp_area_dat$comid),]
temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
area_dat <- temp_area_dat %>% 
  select(comid_form, ag_ac = ag_ac_bmpadj, graz_ac, ag_ac_se, graz_ac_se)

### XI. Updating standard errors of costs -----

##### a. Find standard error of AG & riparian costs over years -----
ag_costs_yearly <- read.csv(file = paste0(InPath, "EQIPcosts_overyears.csv"))

names(ag_costs_yearly)[1] <- "BMP_Category"

ag_costs_yearly <- ag_costs_yearly %>%
  mutate(BMP = case_when(BMP == "Grade_Stabilize" ~ "Grade_Stabilization", TRUE ~ BMP))

bmp_costs_se <- ag_costs_yearly %>%
  pivot_longer(
    cols = -c(BMP_Category, BMP, capital_units, operations_units), 
    names_to = c("costtype", "State", "Year"), 
    names_sep = "_", 
    values_to = "Value"
  ) %>%
  group_by(
    BMP_Category, BMP, State, costtype, capital_units, operations_units
  ) %>%
  summarize(mean = mean(Value), se = (sd(Value) / sqrt(4)), .groups = "keep") %>%
  pivot_wider(
    id_cols = c(BMP_Category, BMP, capital_units, operations_units), 
    names_from = c(costtype, State), 
    values_from = se
  ) %>%
  ungroup()

## adjust ag & riparian units ####

temp_bmp_costs <- merge(
  user_specs_BMPs %>%
    filter(BMP_Selection == "X") %>%
    select(
      BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_width_ft
    ),
  bmp_costs_se,
  by = c("BMP_Category", "BMP"),
  suffixes = c("_Estimate", "_se")
) %>%
  mutate(capital_WA_Estimate = as.numeric(capital_WA_Estimate), operations_WA_Estimate = as.numeric(operations_WA_Estimate))


# Convert area-specific costs to costs per acre for ag BMPs and to costs per linear foot for Riparian BMPs


temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * ft2_to_ac,
          capital_units_Estimate == "km2" ~ . * km2_to_ac,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac,
          capital_units_Estimate == "ac" ~ .
        ),
        BMP_Category == "ag" ~ case_when(
          capital_units_se == "ft2" ~ . * ft2_to_ac,
          capital_units_se == "km2" ~ . * km2_to_ac,
          capital_units_se == "yd2" ~ . * yd2_to_ac,
          capital_units_se == "ac" ~ .
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * ft2_to_ac,
          capital_units_Estimate == "km2" ~ . * km2_to_ac,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac,
          capital_units_Estimate == "ac" ~ .
        ),
        BMP_Category == "graz" ~ case_when(
          capital_units_se == "ft2" ~ . * ft2_to_ac,
          capital_units_se == "km2" ~ . * km2_to_ac,
          capital_units_se == "yd2" ~ . * yd2_to_ac,
          capital_units_se == "ac" ~ .
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units_Estimate == "ft2" ~ . * UserSpec_width_ft,
          capital_units_Estimate == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_Estimate == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_Estimate == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
        BMP_Category == "ripbuf" ~ case_when(
          capital_units_se == "ft2" ~ . * UserSpec_width_ft,
          capital_units_se == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_se == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_width_ft,
          capital_units_se == "ac" ~ . / ft2_to_ac * UserSpec_width_ft
        ),
      ), 
      .names = "{.col}_rev"
    )
  ) %>%
  rename_with(., .fn = ~gsub("_Estimate", "", .), .cols = everything())


## annualize costs ####
bmp_costs <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & !contains("_se")
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

bmp_costs_se <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & contains("_se")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_se_rev", "", .))
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

## Separate parameters for ag_capital_se and ag_operations_se and format costs data ####


if(length(Ag_BMPs) > 0) {
  temp_bmp_costs_ag_se <- merge(
    bmp_costs_se %>%
      filter(category == "ag") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_ag_se$capital)) | 
    any(is.na(temp_bmp_costs_ag_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for ag ",
        if(
          any(is.na(temp_bmp_costs_ag_se$capital)) & 
          any(is.na(temp_bmp_costs_ag_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ag_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  bmp_costs_ag_capital_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_ag_capital_rev_se <- bmp_costs_ag_capital_se[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat_se <- bmp_costs_ag_capital_rev_se[
    ,names(bmp_costs_ag_capital_rev_se) != "comid"
  ]
  ag_costs_cap_dat_se <- ag_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev_se <- bmp_costs_ag_operations_se[
    order(bmp_costs_ag_operations_se$comid),
  ]
  ag_costs_op_dat_se <- bmp_costs_ag_operations_rev_se[ 
    ,names(bmp_costs_ag_operations_rev_se) != "comid"
  ]
  ag_costs_op_dat_se <- ag_costs_op_dat_se %>% select(comid_form, everything())
}

if(length(Graz_BMPs) > 0) {
  temp_bmp_costs_graz_se <- merge(
    bmp_costs_se %>%
      filter(category == "graz") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_graz_se$capital)) | 
    any(is.na(temp_bmp_costs_graz_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for graz ",
        if(
          any(is.na(temp_bmp_costs_graz_se$capital)) & 
          any(is.na(temp_bmp_costs_graz_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_graz_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_graz_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_graz_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  bmp_costs_graz_capital_se <- reshape2::dcast(
    temp_bmp_costs_graz_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_graz_operations_se <- reshape2::dcast(
    temp_bmp_costs_graz_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_graz_capital_rev_se <- bmp_costs_graz_capital_se[
    order(bmp_costs_graz_capital$comid),
  ]
  graz_costs_cap_dat_se <- bmp_costs_graz_capital_rev_se[
    ,names(bmp_costs_graz_capital_rev_se) != "comid"
  ]
  graz_costs_cap_dat_se <- graz_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_graz_operations_rev_se <- bmp_costs_graz_operations_se[
    order(bmp_costs_graz_operations_se$comid),
  ]
  graz_costs_op_dat_se <- bmp_costs_graz_operations_rev_se[ 
    ,names(bmp_costs_graz_operations_rev_se) != "comid"
  ]
  graz_costs_op_dat_se <- graz_costs_op_dat_se %>% select(comid_form, everything())
}



## Format riparian costs ####
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf_se <- merge(
    bmp_costs_se %>%
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
    any(is.na(temp_bmp_costs_ripbuf_se$capital)) | 
    any(is.na(temp_bmp_costs_ripbuf_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for Riparian Buffer ",
        if(
          any(is.na(temp_bmp_costs_ripbuf_se$capital)) & 
          any(is.na(temp_bmp_costs_ripbuf_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ripbuf_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  
  bmp_costs_ripbuf_capital_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ripbuf_operations_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev_se <- bmp_costs_ripbuf_capital_se[
    order(bmp_costs_ripbuf_capital_se$comid),
  ]
  ripbuf_costs_cap_dat_se <- bmp_costs_ripbuf_capital_rev_se[
    ,names(bmp_costs_ripbuf_capital_rev_se) != "comid"
  ]
  ripbuf_costs_cap_dat_se <- ripbuf_costs_cap_dat_se %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev_se <- bmp_costs_ripbuf_operations_se[
    order(bmp_costs_ripbuf_operations_se$comid),
  ]
  ripbuf_costs_op_dat_se <- bmp_costs_ripbuf_operations_rev_se[ 
    ,names(bmp_costs_ripbuf_operations_rev_se) != "comid"
  ]
  ripbuf_costs_op_dat_se <- ripbuf_costs_op_dat_se %>% select(comid_form, everything())
}

### point cost uncertainty

if(
  "point" %in% 
  (user_specs_BMPs %>% filter(BMP_Selection == "X") %>% .$BMP_Category)
) {
  point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) #*#
  # Note to user: Can update this file if more WWTPs desired for analysis; must also update UserSpecs_BMPs.csv with costs
  
  bmp_costs_ptsrc <- fread(paste(InPath, "WWTP_Costs.csv", sep=""))
  
  temp_bmp_costs_point_se <- merge(
    bmp_costs_ptsrc, 
    point_comid,
    by = "COMID",
    all.x = TRUE
  ) %>%
    select(COMID, State, Plant_Name, NPDES_ID, contains("_se")) %>%
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
  
  temp_bmp_costs_point_selected <- temp_bmp_costs_point_se %>% 
    slice(
      which(
        unlist(
          map(
            .x = str_split(temp_bmp_costs_point_se$BMP, "_"), 
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
    
    bmp_costs_point_se <- temp_bmp_costs_point_selected
  } else {
    print(
      paste0(
        "Based on User Selections, no WWTP upgrade BMPs will be considered. ",
        "The BMPs the User has selected may only be implemented in conjunction with unselected BMPs."
      )
    )
    
    bmp_costs_point_se <- data.frame(
      COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
      CostType = NA, Costunits = NA, Cost = NA
    )
  }
  
} else {
  bmp_costs_point_se <- data.frame(
    COMID = NA, State = NA, Plant_Name = NA, NPDES_ID = NA, BMP = NA, 
    CostType = NA, Costunits = NA, Cost = NA
  )
} 

# Format point source BMP costs data
if (length(Point_BMPs) > 0) {
  temp_point_costs_dat_se <- bmp_costs_point_se[
    bmp_costs_point_se$COMID %in% streamcat_subset_all$comid,
  ] %>% 
    select(comid = COMID, State, BMP, CostType, Cost)
  
  temp_point_costs_capital_se <- temp_point_costs_dat_se %>%
    filter(CostType == "Capital") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(everything(), .fns = ~replace_na(., 0))) %>% # Replace NA values with 0 to avoid adding costs to model
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_operations_se <- temp_point_costs_dat_se %>%
    filter(CostType == "OM") %>%
    pivot_wider(id_cols = c(comid, State), names_from = BMP, values_from = Cost) %>%
    mutate(across(everything(), .fns = ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    select(comid, State, any_of(Point_BMPs)) # selection forces the order
  
  temp_point_costs_other_se <- as.data.frame(
    streamcat_subset_all[
      !(streamcat_subset_all$comid %in% temp_point_costs_dat_se$comid),
    ]
  )
  names(temp_point_costs_other_se) <- "comid"
  temp_point_costs_other_se_rev <- merge(
    temp_point_costs_other_se, COMID_State, by = c("comid"), all.x = TRUE
  )
  
  temp_point_costs_other_se_rev[3:(2+length(Point_BMPs))] <- 0
  names(temp_point_costs_other_se_rev) <- c(
    "comid", "State", Point_BMPs)
  
  if(nrow(temp_point_costs_capital_se) > 0) {
    temp_point_capital_costs_dat_se_rev <- rbind(
      temp_point_costs_capital_se, temp_point_costs_other_se_rev
    )
  } else {
    temp_point_capital_costs_dat_se_rev <- temp_point_costs_other_se_rev
  }
  
  temp_point_capital_costs_dat_se_rev <- temp_point_capital_costs_dat_se_rev[
    order(temp_point_capital_costs_dat_se_rev$comid),
  ]
  temp_point_capital_costs_dat_se_rev$comid_form <- paste0(
    "'", temp_point_capital_costs_dat_se_rev$comid, "'"
  )
  point_costs_capital_dat_se <- temp_point_capital_costs_dat_se_rev %>% 
    select(comid_form, all_of(Point_BMPs))
  
  if(nrow(temp_point_costs_operations_se) > 0) {
    temp_point_operations_costs_dat_se_rev <- rbind(
      temp_point_costs_operations_se, temp_point_costs_other_se_rev
    )
  } else {
    temp_point_operations_costs_dat_se_rev <- temp_point_costs_other_se_rev
  }
  
  temp_point_operations_costs_dat_se_rev <- temp_point_operations_costs_dat_se_rev[
    order(temp_point_operations_costs_dat_se_rev$comid),
  ]
  temp_point_operations_costs_dat_se_rev$comid_form <- paste0(
    "'", temp_point_operations_costs_dat_se_rev$comid, "'"
  )
  point_costs_operations_dat_se <- temp_point_operations_costs_dat_se_rev %>% 
    select(comid_form, all_of(Point_BMPs))
} else {
  point_costs_capital_dat_se <- data.frame(
    comid = NA, State = NA, bmp = NA
  )
  point_costs_operations_dat_se <- data.frame(
    comid = NA, State = NA, bmp = NA
  )
}


### Ag and Graz BMPs ----
# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_nonACRE.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency_se"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency_se"
) #*#


## ACRES data - choose ####

temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#

# Read in efficiency data for ACRE database BMPs
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

temp_acre_cast_tn_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic_se"
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

temp_acre_cast_tn_HUC8_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12_se <- temp_acre_cast_tn_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

temp_acre_reaches_HUC12_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC12")],
  temp_acre_cast_tn_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC10")],
  temp_acre_cast_tn_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC8")],
  temp_acre_cast_tn_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn_se <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn_se[ , ACRE_BMPs] <- NA

acre_reaches_tn_se <- temp_acre_reaches_tn_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

temp_acre_cast_tp_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic_se"
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

temp_acre_cast_tp_HUC8_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12_se <- temp_acre_cast_tp_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

temp_acre_reaches_HUC12_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC12")],
  temp_acre_cast_tp_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC10")],
  temp_acre_cast_tp_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC8")],
  temp_acre_cast_tp_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp_se <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp_se[ , ACRE_BMPs] <- NA

acre_reaches_tp_se <- temp_acre_reaches_tp_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))


###add deleted here###
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
    datacheck = map2(.x = N_Efficiency_se, .y = P_Efficiency_se, .f = ~sum(.x, .y))
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

ag_effic_bycomid_tn_se <- add_column(
  acre_reaches_tn_se, 
  temp_ag_effic_fert_man_cast_tn %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.

ag_effic_bycomid_tp_se <- add_column(
  acre_reaches_tp_se, 
  temp_ag_effic_fert_man_cast_tp %>%
    filter(Category == "ag") %>%
    select(any_of(Ag_BMPs))
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) # By setting unknowns to 0, the model will not implement BMPs that have missing data.


# Format agricultural BMP efficiency data

if(length(Ag_BMPs) > 0) {
  temp_ag_effic_dat_tn_se <- ag_effic_bycomid_tn_se[
    ag_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tn_se <- temp_ag_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, everything())
  
  temp_ag_effic_dat_tp_se <- ag_effic_bycomid_tp_se[
    ag_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tp_se <- temp_ag_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

## format graz efficiency data
if(length(Graz_BMPs) > 0) {
  graz_effic_bycomid_tn_se <- data.frame(
    comid = sparrow_in %>% 
      filter(comid %in% reaches_TN_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tn %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  graz_effic_bycomid_tp_se <- data.frame(
    comid = sparrow_in %>% 
      filter(comid %in% reaches_TP_target$catchment_comid) %>% 
      .$comid
  ) %>%
    add_column(
      temp_ag_effic_fert_man_cast_tp %>%
        filter(Category == "graz") %>%
        select(any_of(Graz_BMPs))
    )
  
  temp_graz_effic_dat_tn_se <- graz_effic_bycomid_tn_se[
    graz_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  graz_effic_dat_tn_se <- temp_graz_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, everything()) 
  
  temp_graz_effic_dat_tp_se <- graz_effic_bycomid_tp_se[
    graz_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  graz_effic_dat_tp_se <- temp_graz_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

## get urban efficiency ses ####
if(length(Urban_BMPs) > 0) { 
  
  urban_effic.n <- merge(
    data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)),
    urban.dat,
    by = "comid"
  )
  
  urban_effic_bycomid_tn <- urban_effic.n %>%
    select(comid, Land_Use, HSG, BMP, effic_N, std_efficN, n_efficN) %>%
    filter(BMP %in% Urban_BMPs) %>%
    mutate(category = "urban",
           se_effic_N = std_efficN / (sqrt(n_efficN))) %>%
    select(
      category, comid, Land_Use, HSG, bmp = BMP, effic = effic_N, se = se_effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, Land_Use, HSG), names_from = bmp, values_from = c(effic, se)
    ) %>%
    fill(any_of(contains(Urban_BMPs))) %>% 
    unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, Land_Use, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
    rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
    
  
  urban_effic.p <- merge(
    data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)),
    urban.dat,
    by = "comid"
  )
  
  if (length(urban_effic.p$comid > 0)) {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, Land_Use, HSG, BMP, effic_P, std_efficP, n_efficP) %>%
      filter(BMP %in% Urban_BMPs) %>%
      mutate(category = "urban",
             se_effic_P = std_efficP / (sqrt(n_efficP))) %>%
      select(
        category, comid, Land_Use, HSG, bmp = BMP, effic = effic_P, se = se_effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, Land_Use, HSG), names_from = bmp, values_from = c(effic, se)
      ) %>%
      fill(any_of(contains(Urban_BMPs))) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, Land_Use, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
      rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
    } else {
    urban_effic_bycomid_tp <- urban_effic.p %>%
      select(comid, Land_Use, HSG)
  }
  
  ## Format urban BMP efficiency data ####
  
  temp_urban_effic_dat_tn_se <- urban_effic_bycomid_tn[
    urban_effic_bycomid_tn$comid %in% 
      unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "_", Land_Use, "_", HSG, "'"))
  
  urban_effic_dat_tn_se <- temp_urban_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))

  
  temp_urban_effic_dat_tp_se <- urban_effic_bycomid_tp[
    urban_effic_bycomid_tp$comid %in% 
      unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "_", Land_Use, "_", HSG, "'"))
  
  urban_effic_dat_tp_se <- temp_urban_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list(~gsub("se_", "", .))) 
  
  # Add in missing BMP columns for TN
  bmp_cols <- urban_effic_dat_tn_se %>%
    select(!comid_form)
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(urban_effic_dat_tn_se) > 0) {
      urban_effic_dat_tn_se[[col]] <- 0
    } else {
      urban_effic_dat_tn_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  # Add in missing BMP columns for TP    
  bmp_cols <- urban_effic_dat_tp_se %>%
    select(!comid_form)
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(urban_effic_dat_tp_se) > 0) {
      urban_effic_dat_tp_se[[col]] <- 0
    } else {
      urban_effic_dat_tp_se[[col]] <- vector("numeric", length = 0)
    }
  }
 
  
   ## Format urban BMP cost data #### 
  urban_cost_coeffs_se_tmp <- urban.dat %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, BMP, Land_Use, HSG, capital_se = cap_rmse, operations_se = om_rmse)
  
  urban_cost_coeffs_se_tmp <- urban_cost_coeffs_se_tmp %>% 
    filter(BMP %in% Urban_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital_se = (as.numeric(capital_se) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations_se = as.numeric(operations_se)
    )
  
  urban_cost_coeffs_dat_se <- urban_cost_coeffs_se_tmp %>%
    merge(
      ., 
      streamcat_subset_all,
      by = c("comid")
    ) %>%
    # mutate(across(c(capital_se, operations_se), ~replace_na(., 0.00001))) %>% # # Replace NA values with -999 to avoid adding costs to model
    mutate(comid_form = paste0("'", comid, "_", Land_Use, "_", HSG, "'")) %>%
    unique()
  
  urban_cost_coeffs_dat_mean_cap_se <- urban_cost_coeffs_dat_se %>%
    group_by(BMP) %>%
    summarise(mean_cap_cost_se = mean(capital_se, na.rm = T)) %>%
    pivot_wider(names_from = BMP, values_from = c(mean_cap_cost_se)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%
    rename_with(~paste0(., "_mean"))
  
  urban_cost_coeffs_dat_mean_op_se <- urban_cost_coeffs_dat_se %>%
    group_by(BMP) %>%
    summarise(mean_op_cost_se = mean(operations_se, na.rm = T)) %>%
    pivot_wider(names_from = BMP, values_from = c(mean_op_cost_se)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%
    rename_with(~paste0(., "_mean"))
  
  urban_costs_capital_se <- urban_cost_coeffs_dat_se %>% 
    as_tibble() %>%
    pivot_wider(id_cols = c(comid_form), names_from = BMP, values_from = capital_se) %>%
    cbind(., urban_cost_coeffs_dat_mean_cap_se) %>%
    # Replace NA values with the average cost within the model to avoid AMPL errors
    mutate(across(any_of(Urban_BMPs), ~map2(., urban_cost_coeffs_dat_mean_cap_se[grepl(cur_column(), names(urban_cost_coeffs_dat_mean_cap_se))], ~replace_na(.x, .y)))) %>%
    # unnest(cols = 4:ncol(.)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(!contains("mean"))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- urban_costs_capital_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(urban_costs_capital_se) > 0) {
        urban_costs_capital_se[[col]] <- 1e30
      } else {
        urban_costs_capital_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  urban_costs_operations_se <- urban_cost_coeffs_dat_se %>%
    pivot_wider(id_cols = c(comid_form), names_from = BMP, values_from = operations_se) %>%
    cbind(., urban_cost_coeffs_dat_mean_op_se) %>%
    # Replace NA values with the average cost within the model to avoid AMPL errors
    mutate(across(any_of(Urban_BMPs), ~map2(., urban_cost_coeffs_dat_mean_op_se[grepl(cur_column(), names(urban_cost_coeffs_dat_mean_op_se))], ~replace_na(.x, .y)))) %>%
    # unnest(cols = 4:ncol(.)) %>%
    mutate(across(any_of(Urban_BMPs), ~replace_na(., 0))) %>%  # # Replace NA values with 0 to avoid adding costs to model
    unique() %>%
    select(!contains("mean"))
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- urban_costs_operations_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(Urban_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(urban_costs_operations_se) > 0) {
        urban_costs_operations_se[[col]] <- 1e30
      } else {
        urban_costs_operations_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
}

if(length(road_BMPs) > 0) { 
  
  road_effic.n <- merge(
    data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)),
    road.dat,
    by = "comid"
  )
  
  road_effic_bycomid_tn <- road_effic.n %>%
    select(comid, HSG, BMP, effic_N, std_efficN, n_efficN) %>%
    filter(BMP %in% road_BMPs) %>%
    mutate(category = "road",
           se_effic_N = std_efficN / (sqrt(n_efficN))) %>%
    select(
      category, comid, HSG, bmp = BMP, effic = effic_N, se = se_effic_N
    ) %>%
    unique() %>%
    pivot_wider(
      id_cols = c(comid, HSG), names_from = bmp, values_from = c(effic, se)
    ) %>%
    fill(any_of(contains(road_BMPs))) %>% unnest(cols = 1:ncol(.)) %>%
    unique() %>%
    mutate(across(-c(comid, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
    rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
  
  
  road_effic.p <- merge(
    data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)),
    road.dat,
    by = "comid"
  )
  
  if (length(road_effic.p$comid > 0)) {
    road_effic_bycomid_tp <- road_effic.p %>%
      select(comid, HSG, BMP, effic_P, std_efficP, n_efficP) %>%
      filter(BMP %in% road_BMPs) %>%
      mutate(category = "road",
             se_effic_P = std_efficP / (sqrt(n_efficP))) %>%
      select(
        category, comid, HSG, bmp = BMP, effic = effic_P, se = se_effic_P
      ) %>%
      unique() %>%
      pivot_wider(
        id_cols = c(comid, HSG), names_from = bmp, values_from = c(effic, se)
      ) %>%
      fill(any_of(contains(road_BMPs))) %>%
      unnest(cols = 1:ncol(.)) %>%
      unique() %>%
      mutate(across(-c(comid, HSG), ~replace_na(., 0))) %>% # By setting unknowns to 0, the model will not implement BMPs that have missing data as they are not effective.
      rename_with(~ gsub("^effic_", "", .), starts_with("effic_"))
  } else {
    road_effic_bycomid_tp <- road_effic.p %>%
      select(comid, HSG)
  }
  
  ## Format road BMP efficiency data ####
  
  temp_road_effic_dat_tn_se <- road_effic_bycomid_tn[
    road_effic_bycomid_tn$comid %in% 
      unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "_", HSG, "'"))
  
  road_effic_dat_tn_se <- temp_road_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  temp_road_effic_dat_tp_se <- road_effic_bycomid_tp[
    road_effic_bycomid_tp$comid %in% 
      unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "_", HSG, "'"))
  
  road_effic_dat_tp_se <- temp_road_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  # Add in missing BMP columns for TN
  bmp_cols <- road_effic_dat_tn_se %>%
    select(!comid_form)
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(road_effic_dat_tn_se) > 0) {
      road_effic_dat_tn_se[[col]] <- 0
    } else {
      road_effic_dat_tn_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  # Add in missing BMP columns for TP    
  bmp_cols <- road_effic_dat_tp_se %>%
    select(!comid_form)
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  for (col in missing_columns) {
    if (nrow(road_effic_dat_tp_se) > 0) {
      road_effic_dat_tp_se[[col]] <- 0
    } else {
      road_effic_dat_tp_se[[col]] <- vector("numeric", length = 0)
    }
  }
  
  ##  Define road cost correction coefficients ####
  
  road_cost_coeffs_se_tmp <- road.dat %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, BMP, HSG, capital_se = cap_rmse, operations_se = om_rmse)
  
  road_cost_coeffs_se_tmp <- road_cost_coeffs_se_tmp %>% 
    filter(BMP %in% road_BMPs) %>%
    # Annualize capital costs based on planning horizon and interest rate
    mutate(
      capital_se = (as.numeric(capital_se) * (
        interest_rate * ((1 + interest_rate) ^ horizon) / 
          (((1 + interest_rate) ^ horizon) - 1)
      )
      ),
      operations_se = as.numeric(operations_se)
    )
  
  road_cost_coeffs_dat_se <- road_cost_coeffs_se_tmp %>%
    merge(
      ., 
      streamcat_subset_all,
      by = c("comid")
    ) %>%
    mutate(across(c(capital_se, operations_se), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    mutate(comid_form = paste0("'", comid, "_", HSG, "'")) %>%
    unique()
  
  road_costs_capital_se <- road_cost_coeffs_dat_se %>% 
    pivot_wider(id_cols = c(comid_form), names_from = BMP, values_from = capital_se) %>%
    unnest(cols = 1:ncol(.)) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique()
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- road_costs_capital_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(road_costs_capital_se) > 0) {
        road_costs_capital_se[[col]] <- 1e30
      } else {
        road_costs_capital_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
  road_costs_operations_se <- road_cost_coeffs_dat_se %>%
    pivot_wider(id_cols = c(comid_form), names_from = BMP, values_from = operations_se) %>%
    unnest(cols = 1:ncol(.)) %>%
    mutate(across(any_of(road_BMPs), ~replace_na(., 0))) %>% # # Replace NA values with 0 to avoid adding costs to model
    unique()
  
  #Fill in all BMPs if some are dropped due to no data
  bmp_cols <- road_costs_operations_se %>%
    select(!comid_form)
  
  missing_columns <- setdiff(road_BMPs, names(bmp_cols))
  
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      if (nrow(road_costs_operations_se) > 0) {
        road_costs_operations_se[[col]] <- 1e30
      } else {
        road_costs_operations_se[[col]] <- vector("numeric", length = 0)
      }
    }
  }
  
}

# Write Command Script ####

cat(
  " 
#WMOST Optimization Screening Tool AMPL command file with uncertainty

option display_transpose -10000;
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "\n"
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if(param_loads_lim[i,1] == "TN") {
        cat(
          paste0("\ndisplay loads_lim_N", i, ";"),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
      if(param_loads_lim[i,1] == "TP") {
        cat(
          paste0("\ndisplay loads_lim_P", i, ";"),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

cat(
  "
for {i in Scenarios} {

if i > 1
  then {
  ", 
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

#@ SE: made changes here
invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0("\nlet loads_lim_N", i, ":= loads_lim_N", i, " * ", scenarioincrement, ";"),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
        paste0("\nlet loads_lim_P", i, ":= loads_lim_P", i, " * ", scenarioincrement, ";"),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
      }
    }
  }
)

cat(
  "
  };
solve;

for {s in Bootstraps} {
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0(
            "\nrepeat { \nlet other_loads_N", 
            i, 
            "_rev[s] := Normal(other_loads_N", 
            i, 
            ", ", 
            param_other_loads_tn[[i]]$se,
            "); \n } until other_loads_N",
            i,
            "_rev[s] >= 0;"
          ),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
          paste0(
            "\nrepeat { \nlet other_loads_P", 
            i, 
            "_rev[s] := Normal(other_loads_P", 
            i, 
            ", ", 
            param_other_loads_tp[[i]]$se,
            "); \n } until other_loads_P",
            i,
            "_rev[s] >= 0;"
          ),    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

cat(
  "
	let agcost_frac_rev[s] := Uniform(1,5/3);
	 ",    
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)


invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0(
            "\nfor {c in comid_N", 
            i, 
            "} {\nfor {l in loads_all} {\nrepeat {\nlet  baseloads_N", 
            i, 
            "_rev[c,l,s] := Normal(baseloads_N", 
            i,
            "[c,l], baseloads_N",
            i,
            "_se[c,l]);\n} until baseloads_N",
            i,
            "_rev[c,l,s] >= 0;\n};\nfor {b in ripbuf_bmp} {\nlet riparianremoval_N",
            i,
            "_rev[c,b,s] := Normal(riparianremoval_N",
            i,
            "[c,b], riparianremoval_N",
            i,
            "_se[c,b]);\n  };\n};"
          ),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
for {c in comid_all_N} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_N_rev[c,a,s] := Normal(ag_effic_N[c,a], ag_effic_N_se[c,a]);
    } until ag_effic_N_rev[c,a,s] <= 1;
  };
  for {g in graz_bmp} {
    repeat {
      let graz_effic_N_rev[c,g,s] := Normal(graz_effic_N[c,g], graz_effic_N_se[c,g]);
    } until graz_effic_N_rev[c,g,s] <= 1;
  };
  for {p in point_bmp} {
    let point_effic_N_rev[c,p,s] := Normal(point_effic_N[c,p], abs(point_effic_N_se[c,p]) * 0.2);
  };
};
for {x in urban_comid_all_N, u in urban_bmp} {
    repeat {
      let urban_effic_N_rev[x,u,s] := Normal(urban_effic_N[x,u], urban_effic_N_se[x,u]);
    } until urban_effic_N_rev[x,u,s] <= 1;
  };
for {h in road_comid_all_N, r in road_bmp} {
    repeat {
      let road_effic_N_rev[h,r,s] := Normal(road_effic_N[h,r], road_effic_N_se[h,r]);
    } until road_effic_N_rev[h,r,s] <= 1;
  };  
",    
    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
    sep = "", 
    append = T
  )
}

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TP") {
        cat(
          paste0(
            "\nfor {c in comid_P", 
            i, 
            "} {\nfor {l in loads_all} {\nrepeat {\nlet  baseloads_P", 
            i, 
            "_rev[c,l,s] := Normal(baseloads_P", 
            i,
            "[c,l], baseloads_P",
            i,
            "_se[c,l]);\n} until baseloads_P",
            i,
            "_rev[c,l,s] >= 0;\n};\nfor {b in ripbuf_bmp} {\nlet riparianremoval_P",
            i,
            "_rev[c,b,s] := Normal(riparianremoval_P",
            i,
            "[c,b], riparianremoval_P",
            i,
            "_se[c,b]);\n  };\n};"
          ),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
for {c in comid_all_P} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_P_rev[c,a,s] := Normal(ag_effic_P[c,a], ag_effic_P_se[c,a]);
    } until ag_effic_P_rev[c,a,s] <=1;
  };
  for {g in graz_bmp} {
    repeat {
      let graz_effic_P_rev[c,g,s] := Normal(graz_effic_P[c,g], graz_effic_P_se[c,g]);
    } until graz_effic_P_rev[c,g,s] <=1;
  };
  for {p in point_bmp} {
    let point_effic_P_rev[c,p,s] := Normal(point_effic_P[c,p], abs(point_effic_P_se[c,p]) * 0.2);
  };
};
for {x in urban_comid_all_P, u in urban_bmp} {
    repeat {
      let urban_effic_P_rev[x,u,s] := Normal(urban_effic_P[x,u], urban_effic_P_se[x,u]);
    } until urban_effic_P_rev[x,u,s] <= 1;
  };
for {h in road_comid_all_P, r in road_bmp} {
    repeat {
      let road_effic_P_rev[h,r,s] := Normal(road_effic_P[h,r], road_effic_P_se[h,r]);
    } until road_effic_P_rev[h,r,s] <= 1;
  };
    ",
    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}


cat(
  "
for {c in comid_all} {
    
  for {p in point_bmp} {
    repeat {
      let point_costs_capital_rev[c,p,s] := Normal(point_costs_capital[c,p] * 1.15, point_costs_capital[c,p] * 0.15);
    } until point_costs_capital_rev[c,p,s] >= 0;
    repeat {
      let point_costs_operations_rev[c,p,s] := Normal(point_costs_operations[c,p] * 1.15, point_costs_operations[c,p] * 0.15);
    } until point_costs_operations_rev[c,p,s] >= 0;
  };
  
  for {b in ripbuf_bmp} {
    repeat {
      let ripbuf_costs_capital_rev[c,b,s] := Normal(ripbuf_costs_capital[c,b], ripbuf_costs_capital_se[c,b]);
    } until ripbuf_costs_capital_rev[c,b,s] >= 0;
    repeat { 
      let ripbuf_costs_operations_rev[c,b,s] := Normal(ripbuf_costs_operations[c,b], ripbuf_costs_operations_se[c,b]);
    } until ripbuf_costs_operations_rev[c,b,s] >= 0;
  };
  
  for {ar in area_sub} {
    repeat {
      let area_rev[c,ar,s] := Normal(area[c,ar], area_se[c,ar]);
    } until area_rev[c,ar,s] >= 0;
  };
  
  for {a in ag_bmp} {
    repeat {
      let ag_costs_capital_rev[c,a,s] := Normal(ag_costs_capital[c,a], ag_costs_capital_se[c,a]);
    } until ag_costs_capital_rev[c,a,s] >= 0;
    repeat {
      let ag_costs_operations_rev[c,a,s] :=  Normal(ag_costs_operations[c,a], ag_costs_operations_se[c,a]);
    } until ag_costs_operations_rev[c,a,s] >= 0;
  };
  
  for {g in graz_bmp} {
    repeat {
      let graz_costs_capital_rev[c,g,s] := Normal(graz_costs_capital[c,g], graz_costs_capital_se[c,g]);
    } until graz_costs_capital_rev[c,g,s] >= 0;
    repeat {
      let graz_costs_operations_rev[c,g,s] :=  Normal(graz_costs_operations[c,g], graz_costs_operations_se[c,g]);
    } until graz_costs_operations_rev[c,g,s] >= 0;
  };
  
};

for {x in urban_comid_all} {
 for {ur in urban_area_sub} {
    repeat {
      let urban_area_rev[x,ur,s] := Normal(urban_area[x, ur], urban_area_se[x,ur]);
    } until urban_area_rev[x,ur,s] >= 0;
  };
  
 for {u in urban_bmp} {
    repeat {
	      let urban_costs_capital_rev[x,u,s] := Normal(urban_costs_capital[x,u], urban_costs_capital_se[x,u]);
	  } until urban_costs_capital_rev[x,u,s] >= 0;
	  repeat {
	      let urban_costs_operations_rev[x,u,s] := Normal(urban_costs_operations[x,u], urban_costs_operations_se[x,u]);
	  } until urban_costs_operations_rev[x,u,s] >= 0;
	};
 };

for {h in road_comid_all} {
  
 for {r in road_bmp} {
    repeat {
	      let road_costs_capital_rev[h,r,s] := Normal(road_costs_capital[h,r], road_costs_capital_se[h,r]);
	  } until road_costs_capital_rev[h,r,s] >= 0;
	  repeat {
	      let road_costs_operations_rev[h,r,s] := Normal(road_costs_operations[h,r], road_costs_operations_se[h,r]);
	  } until road_costs_operations_rev[h,r,s] >= 0;
	};
 };
};	

option display_precision 10;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option omit_zero_rows 1;
option omit_zero_cols 1;

display {s in Bootstraps} (sum {c in comid_all, p in point_bmp} (ps_coef_rev[c,p,s] * point_dec[c,p]) + sum {x in urban_comid_all, u in urban_bmp} (urban_coef_rev[x,u,s] * urban_frac[x,u]) + sum {h in road_comid_all, r in road_bmp} (road_coef_rev[h,r,s] * road_frac[h,r]) + sum {c in comid_all, a in ag_bmp} (ag_coef_rev[c,a,s] * ag_frac[c,a]) + sum {c in comid_all, g in graz_bmp} (graz_coef_rev[c,g,s] * graz_frac[c,g]) + sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef_rev[c,b,s] * ripbuf_length[c,b]));
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

# Create a text variable to add into the following code for when no point BMps are available
point_bmp_text <- ifelse(length(Point_BMPs) > 0, 'point', 'none')

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0(
            "\ndisplay {s in Bootstraps}  (other_loads_N", 
            i, 
            "_rev[s] + sum {c in comid_N", 
            i, 
            "} (baseloads_N", 
            i,
            "_rev[c,'ag',s] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,s] * ag_frac[c,a]))) + sum {c in comid_N",
            i,
            "} (baseloads_N", 
            i,
            "_rev[c,'graz',s] * (1 - sum {g in graz_bmp}(graz_effic_N_rev[c,g,s] * graz_frac[c,g]))) + sum {c in comid_N",
            i,
            "} (baseloads_N",
            i,
            "_rev[c,'urban',s] * (1 - sum {x in urban_comid_N", i, ", u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_N_rev[x,u,s] * urban_frac[x,u]))) + sum {c in comid_N",
            i,
            "} (baseloads_N",
            i,
            "_rev[c,'road',s] * (1 - sum {h in road_comid_N", i, ", r in road_bmp : c == road_comid_xwalk[h]}(road_effic_N_rev[h,r,s] * road_frac[h,r]))) + sum {c in comid_N",
            i,
            "} (baseloads_N",
            i,
            "_rev[c,'point',s] * (1 - sum {p in point_bmp}(point_effic_N_rev[c,p,s] * point_dec[c,p]))) - sum {c in comid_N",
            i,
            ", b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_N",
            i,
            "_rev[c,b,s]));"
          ),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
          paste0(
            "\ndisplay {s in Bootstraps}  (other_loads_P", 
            i, 
            "_rev[s] + sum {c in comid_P", 
            i, 
            "} (baseloads_P", 
            i,
            "_rev[c,'ag',s] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,s] * ag_frac[c,a]))) + sum {c in comid_P",
            i,
            "} (baseloads_P", 
            i,
            "_rev[c,'graz',s] * (1 - sum {g in graz_bmp}(graz_effic_P_rev[c,g,s] * graz_frac[c,g]))) + sum {c in comid_P",
            i,
            "} (baseloads_P",
            i,
            "_rev[c,'urban',s] * (1 - sum {x in urban_comid_P", i, ", u in urban_bmp : c == urban_comid_xwalk[x]}(urban_effic_P_rev[x,u,s] * urban_frac[x,u]))) + sum {c in comid_P",
            i,
            "} (baseloads_P",
            i,
            "_rev[c,'road',s] * (1 - sum {h in road_comid_P", i, ", r in road_bmp : c == road_comid_xwalk[h]}(road_effic_P_rev[h,r,s] * road_frac[h,r]))) + sum {c in comid_P",
            i,
            "} (baseloads_P",
            i,
            "_rev[c,'point',s] * (1 - sum{p in point_bmp}(point_effic_P_rev[c,p,s] * point_dec[c,p]))) - sum {c in comid_P",
            i,
            ", b in ripbuf_bmp} (ripbuf_length[c, b] * riparianremoval_P",
            i,
            "_rev[c,b,s]));"
          ),
          file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

cat(
  "
display point_dec;

option display_1col 0;
display ripbuf_length;

option display_width 100000000000;
display urban_frac;
display road_frac;
display ag_frac;
display graz_frac;

}

",
file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
sep = "\n",
append = TRUE
)

# Write Data Script ####

orig.dat <- read.table(
  file = paste(OutPath, "STdata.dat", sep=""), 
  header = FALSE,
  sep = "\n",
  quote = ""
)

write.table(
  orig.dat, # selecting forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_uncertainty.dat", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        write(
          paste0("\nparam baseloads_N", i, "_se : 'point' 'urban' 'road' 'ag' 'graz' :="), 
          file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tn_dat_se[[i]] %>% 
            select(comid_form, point = sin_poin, urban = sin_urb, road = sin_road, ag = sin_ag, graz = sin_graz) %>%
            unique(), # renaming also forces the order in case they are disordered in processing code above
          file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file =   paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T) 
      }
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if(param_loads_lim[i,1] == "TP") {
        write(
          paste0("\nparam baseloads_P", i, "_se : 'point' 'urban' 'road' 'ag' 'graz' :="), 
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          inc_tp_dat_se[[i]] %>% 
            select(comid_form, point = sip_poin, urban = sip_urb, road = sip_road, ag = sip_ag, graz = sip_graz) %>%
            unique(), # renaming also forces the order in case they are disordered in processing code above
          file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
        write( ";", file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T) 
      }
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if(param_loads_lim[i,1] == "TN") {
        if(length(RiparianBuffer_BMPs) > 0) {
          write(
            paste0(
              "\nparam riparianremoval_N", 
              i, 
              "_se : ", 
              paste(bmp_ripbuf_vec, collapse  = "  "), 
              " :="
            ), 
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
            append = T
          )
          write.table(
            riparian_tn_removal_se[[i]] %>% 
              select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          write(
            paste0(
              "\nparam riparianremoval_N", 
              i, 
              "_se : 'none' :="
            ), 
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
            append = T
          )
          write.table(
            ripbuf_bmp_dummy_tn[[i]] %>% 
              select(comid, none), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T) 
      }
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if(param_loads_lim[i,1] == "TP") {
        if(length(RiparianBuffer_BMPs) > 0) {
          write(
            paste0(
              "\nparam riparianremoval_P", 
              i, 
              "_se : ", 
              paste(bmp_ripbuf_vec, collapse  = "  "), 
              " :="
            ), 
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
            append = T
          )
          write.table(
            riparian_tp_removal_se[[i]] %>% 
              select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        } else {
          write(
            paste0(
              "\nparam riparianremoval_P", 
              i, 
              "_se : 'none' :="
            ), 
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
            append = T
          )
          write.table(
            ripbuf_bmp_dummy_tp[[i]] %>% 
              select(comid, none), # renaming also forces the order in case they are disordered in processing code above
            file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
      }
    }
  }
)
# BACK HERE
if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tn_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tp_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
}

if(length(Graz_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam graz_effic_N_se : ",bmp_graz_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      graz_effic_dat_tn_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam graz_effic_P_se : ",bmp_graz_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      graz_effic_dat_tp_se %>% select(comid_form, any_of(Graz_BMPs)),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam graz_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", graz_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam graz_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", graz_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
}


if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_N_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tn_se %>% select(comid_form, all_of(bmp_urban_direct))), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_P_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tp_se %>% select(comid_form, all_of(bmp_urban_direct))), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_effic_N_se : ",bmp_road_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(road_effic_dat_tn_se %>% select(comid_form, all_of(road_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam road_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", road_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(road_BMPs) > 0) {
    cat(
      "\nparam road_effic_P_se : ",bmp_road_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(road_effic_dat_tp_se %>% select(comid_form, all_of(road_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam road_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", road_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if (length(Point_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_N_se : ",bmp_point_vec," :=	", 
        file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
      write.table(
        unique(point_effic_dat_tn_se %>% select(comid_form, all_of(Point_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }

  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        "\nparam point_effic_P_se : ",bmp_point_vec," :=	", 
        file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        sep="", 
        append = T
      )
      cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
      write.table(
        unique(point_effic_dat_tp_se %>% select(comid_form, all_of(Point_BMPs))), # selecting forces the order in case they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
      }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam point_effic_N_se : 'none' :=	", 
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    sep="", 
    append = T
  )
  cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
  write.table(
    unique(do.call("rbind", point_bmp_dummy_tn) %>% 
             select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
 if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam point_effic_P_se : 'none' :=	", 
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    sep="", 
    append = T
  )
  cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
  write.table(
    unique(do.call("rbind", point_bmp_dummy_tp) %>% 
             select(comid, none)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
 }
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)


write( 
  "\n\nparam area_se : 'ag' 'graz' :=", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  area_dat %>% select(comid_form, ag = ag_ac_se, graz = graz_ac_se), # renaming also forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)

write( 
  "\n\nparam urban_area_se : 'urban' :=", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T
)

write.table(
  urban_area_dat %>% filter(comid %in% final_target_list$catchment_comid) %>% select(comid_form, urban = urban_ac_se), # renaming also forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)

##Costs
if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital_se : ", bmp_ripbuf_vec, " :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : ", bmp_ripbuf_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}

if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}

if(length(Graz_BMPs) > 0) {
  cat(
    "\nparam graz_costs_capital_se : ",bmp_graz_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_costs_cap_dat_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam graz_costs_operations_se : ",bmp_graz_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table( 
    graz_costs_op_dat_se %>% select(comid_form, any_of(Graz_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam graz_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam graz_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    graz_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}

if(length(Point_BMPs) > 0) {
  cat(
    "\nparam point_costs_capital_se : ",bmp_point_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_costs_capital_dat_se %>% select(comid_form, any_of(Point_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam point_costs_operations_se : ",bmp_point_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table( 
    point_costs_operations_dat_se %>% select(comid_form, any_of(Point_BMPs)), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam point_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam point_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    point_bmp_dummy %>% select(comid, none), # selecting forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}

cat( 
  "\nparam urban_costs_capital_se :", bmp_urban_vec," :=	", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_capital_se %>% 
      select(comid_form, all_of(bmp_urban_direct)) %>%
      mutate(across(where(is.list), unlist)),
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam urban_costs_operations_se :", bmp_urban_vec," :=	", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_operations_se %>% 
      select(comid_form, all_of(bmp_urban_direct)) %>%
      mutate(across(where(is.list), unlist)),
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam road_costs_capital_se :", bmp_road_vec," :=	", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(road_BMPs) > 0) {
  write.table(
    road_costs_capital_se %>% select(comid_form, all_of(road_BMPs)),
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    road_bmp_dummy,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)

cat( 
  "\nparam road_costs_operations_se :", bmp_road_vec," :=	", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
  append = T
)
cat(
  "\n",
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
  sep = "", 
  append = T
)
if(length(road_BMPs) > 0) {
  write.table(
    road_costs_operations_se %>% select(comid_form, all_of(road_BMPs)),
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    road_bmp_dummy,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)

# Write Model Script ####

orig.mod <- readLines(paste(OutPath, "STmodel.mod", sep=""))

write.table(
  orig.mod, # selecting forces the order in case they are disordered in preprocessing code above
  file = paste(OutPath, "STmodel_uncertainty.mod", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0(
            "\nparam baseloads_N", 
            i, 
            "_se {comid_N", 
            i, 
            ", loads_all} >= 0;"
          ),
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
          paste0(
            "\nparam baseloads_P", 
            i, 
            "_se {comid_P", 
            i, 
            ", loads_all} >= 0;"
          ),
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
          sep = "", 
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
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se {c in comid_N", 
            i, 
            ", b in ripbuf_bmp};"
          ), 
          file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
          append = T
        ) 
      }
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if(param_loads_lim[i,1] == "TP") {
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            "_se {c in comid_P", 
            i, 
            ", b in ripbuf_bmp};"
          ), 
          file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
          append = T
        ) 
      }
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
param ag_effic_N_se {comid_all_N ,ag_bmp} >= 0;
param urban_effic_N_se {urban_comid_all_N, urban_bmp} >= 0;
param road_effic_N_se {road_comid_all_N, road_bmp} >= 0;
param point_effic_N_se {comid_all_N, point_bmp} >= 0;
param graz_effic_N_se {comid_all_N, graz_bmp} >= 0;",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
  
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
param ag_effic_P_se {comid_all_P ,ag_bmp} >= 0;
param urban_effic_P_se {urban_comid_all_P, urban_bmp} >= 0;
param road_effic_P_se {road_comid_all_P, road_bmp} >= 0;
param point_effic_P_se {comid_all_P, point_bmp} >= 0;
param graz_effic_P_se {comid_all_P, graz_bmp} >= 0;",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param ripbuf_costs_capital_se {comid_all, ripbuf_bmp};
param ripbuf_costs_operations_se {comid_all, ripbuf_bmp};

param area_se {comid_all,area_sub} >=0;
param urban_area_se {urban_comid_all,urban_area_sub} >=0;

param ag_costs_capital_se {comid_all,ag_bmp};
param ag_costs_operations_se {comid_all,ag_bmp};

param urban_costs_capital_se {urban_comid_all,urban_bmp};
param urban_costs_operations_se {urban_comid_all,urban_bmp};

param road_costs_capital_se {road_comid_all,road_bmp};
param road_costs_operations_se {road_comid_all,road_bmp};

param graz_costs_capital_se {comid_all,graz_bmp};
param graz_costs_operations_se {comid_all,graz_bmp};

param point_costs_capital_se {comid_all,point_bmp};
param point_costs_operations_se {comid_all,point_bmp};

",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  paste0("set Scenarios := 1 .. ", n.scenarios, " by 1;"),
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  "
set Bootstraps := 1 .. 200 by 1;
",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        cat(
          paste0(
            "\nparam baseloads_N", 
            i, 
            "_rev {comid_N", 
            i, 
            ", loads_all, Bootstraps};"
          ),
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
          paste0(
            "\nparam baseloads_P", 
            i, 
            "_rev {comid_P", 
            i, 
            ", loads_all, Bootstraps};"
          ),
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
    param ag_effic_N_rev {comid_all_N ,ag_bmp, Bootstraps};
param point_effic_N_rev {comid_all_N, point_bmp, Bootstraps};
param urban_effic_N_rev {urban_comid_all_N, urban_bmp, Bootstraps};
param road_effic_N_rev {road_comid_all_N, road_bmp, Bootstraps};
param graz_effic_N_rev {comid_all_N, graz_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
    param ag_effic_P_rev {comid_all_P ,ag_bmp, Bootstraps};
param point_effic_P_rev {comid_all_P, point_bmp, Bootstraps};
param urban_effic_P_rev {urban_comid_all_P, urban_bmp, Bootstraps};
param road_effic_P_rev {road_comid_all_P, road_bmp, Bootstraps};
param graz_effic_P_rev {comid_all_P, graz_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param area_rev {comid_all,area_sub, Bootstraps};
param urban_area_rev {urban_comid_all,urban_area_sub, Bootstraps};
param ag_costs_capital_rev {comid_all,ag_bmp, Bootstraps};
param ag_costs_operations_rev {comid_all,ag_bmp, Bootstraps};
param point_costs_capital_rev {comid_all,point_bmp, Bootstraps};
param point_costs_operations_rev {comid_all,point_bmp , Bootstraps};
param urban_costs_capital_rev {urban_comid_all, urban_bmp, Bootstraps};
param urban_costs_operations_rev {urban_comid_all, urban_bmp, Bootstraps};
param road_costs_capital_rev {road_comid_all, road_bmp, Bootstraps};
param road_costs_operations_rev {road_comid_all, road_bmp, Bootstraps};
param ripbuf_costs_capital_rev {comid_all, ripbuf_bmp, Bootstraps};
param ripbuf_costs_operations_rev {comid_all, ripbuf_bmp, Bootstraps};
param graz_costs_capital_rev {comid_all, graz_bmp, Bootstraps};
param graz_costs_operations_rev {comid_all, graz_bmp, Bootstraps};
",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim$TN_or_TP)) %do% {
      if (param_loads_lim[i,1] == "TN") {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_rev {c in comid_N", 
            i, 
            ", b in ripbuf_bmp, Bootstraps};"
          ), 
          file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            "_rev {c in comid_P", 
            i, 
            ", b in ripbuf_bmp, Bootstraps};"
          ), 
          file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
            "\nparam other_loads_N", 
            i, 
            "_rev {Bootstraps};"
          ),
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
      if(param_loads_lim[i,1] == "TP") {
        cat(
          paste0(
            "\nparam other_loads_P", 
            i, 
            "_rev {Bootstraps};"
          ),   
          file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
          sep = "", 
          append = T
        ) 
      }
    }
  }
)

cat(
  "
param agcost_frac_rev {Bootstraps} >=0;

param ps_coef_rev {c in comid_all, p in point_bmp, s in Bootstraps} := point_costs_capital_rev[c,p,s] + point_costs_operations_rev[c,p,s];
param urban_coef_rev {x in urban_comid_all, u in urban_bmp, s in Bootstraps} := urban_area_rev[x,'urban',s] * (urban_costs_capital_rev[x,u,s] + urban_costs_operations_rev[x,u,s]);
param road_coef_rev {h in road_comid_all, r in road_bmp, s in Bootstraps} := road_area[h,'road'] * (road_costs_capital_rev[h,r,s] + road_costs_operations_rev[h,r,s]);
param ag_coef_rev {c in comid_all, a in ag_bmp, s in Bootstraps} := agcost_frac_rev[s] * area_rev[c,'ag',s] *  (ag_costs_capital_rev[c,a,s] + ag_costs_operations_rev[c,a,s]) ; 
param ripbuf_coef_rev {c in comid_all, b in ripbuf_bmp, s in Bootstraps} := agcost_frac_rev[s] * (ripbuf_costs_capital_rev[c,b,s] +  ripbuf_costs_operations_rev[c,b,s]);
param graz_coef_rev {c in comid_all, g in graz_bmp, s in Bootstraps} := agcost_frac_rev[s] * area_rev[c,'graz',s] * (graz_costs_capital_rev[c,g,s] + graz_costs_operations_rev[c,g,s]) ;


",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)


print(
  paste(
    "RBEROST has finished writing AMPL scripts with uncertainty at",
    Sys.time()
  )
)
