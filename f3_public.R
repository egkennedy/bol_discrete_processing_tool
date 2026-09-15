
# This imports the CRM batch information, calculates CRM performance metrics (SD, accuracy)
# and corrects the data for the three CLOSEST TOGETHER CRMs on either side of the 
# run. It depends on the analyst maintaining a .csv file of CRM batch information that includes
# the silicate and phosphate concentrations reported by Andrew Dickson's lab.
# It also assumes that all CRMs have "CRM" in the sample name. 

# Note as well that there are venue-specific volume considerations at line 116.

library("tidyverse")
library("data.table")


f3 <- function(f2_output){
  crm_batch <- f2_output$crm_batch
  crm_batch_info_path <- f2_output$crm_batch_info_path
  gstd_fits <- f2_output$gstd_fits
  lstd_fits <- f2_output$lstd_fits
  df <- f2_output$discrete_samples
  run_id <- first(df$run_number)
  roomair_co2 <- first(df$roomair_co2)
  Lab_T <- first(df$lab_temp)
  sample_inventory_path <- f2_output$sample_inventory_path
  
  
  # Get the CRM info
  crm_batch_data <- read_csv(crm_batch_info_path)
  
  crm_tco2 = crm_batch_data$tco2[which(crm_batch_data$batch == crm_batch)]
  crm_phos = crm_batch_data$phos[which(crm_batch_data$batch == crm_batch)]
  crm_sil = crm_batch_data$sil[which(crm_batch_data$batch == crm_batch)]
  crm_sal = crm_batch_data$salinity[which(crm_batch_data$batch == crm_batch)]
  crm_ta = crm_batch_data$ta[which(crm_batch_data$batch == crm_batch)]
  
  
  # Get the crm group names
  crm_groupnames <- df |> 
    arrange(julian_dt) |> 
    filter(type == "CRM") |> 
    distinct(set) |> unlist()
  
  # Make a dataframe to put crm info into
  crm_info <- data.frame(crm_set = NA, crm_mean = NA, crm_sd_all = NA, crm_sd_closest = NA, crm_corfac = NA, crm_time = NA, num_crms = NA)
  crm_info <- crm_info[-1, ]
  
  ## Find the closest three CRMs if there are more than 3 in a group.
  findClosest <- function(x, n) {
    x <- sort(x)
    x[seq.int(which.min(diff(x, lag = n - 1L)), length.out = n)]
  }
  
  crm_corr_info <- function(data, crm_set){
    crms <- data |> 
      filter(set == crm_set)
    
    if (nrow(crms) > 3){
      closest <- findClosest(crms$tco2_umolkg, 3L)
    } else {
      closest <- crms$tco2_umolkg
    }
    
    crm_mean <- mean(closest, na.rm = TRUE)
    crm_sd_all <- sd(crms$tco2_umolkg, na.rm = TRUE)
    crm_sd_closest <- sd(closest, na.rm = TRUE)
    crm_corfac <- crm_tco2/crm_mean
    crm_time <- max(crms$DOY_TCO2, na.rm = TRUE) # Using the last time stamp, not the average, per the Excel sheets
    num_crms <- nrow(crms)
    
    ## Append to the crm dataframe
    data.frame(crm_set = crm_set, crm_mean = crm_mean, crm_sd_all = crm_sd_all, 
               crm_sd_closest = crm_sd_closest, crm_corfac = crm_corfac, crm_time = crm_time, num_crms = num_crms)
  }
  
  # Get the crm info for the two sets
  compilation <- crm_corr_info(df, crm_groupnames[1])
  crm_info <- rbind(crm_info, compilation)
  
  compilation <- crm_corr_info(df, crm_groupnames[2])
  crm_info <- rbind(crm_info, compilation)
  
  
  ## Drift corrections for the crm correction factor
  mod <- lm(crm_corfac ~ crm_time, data = crm_info)
  
  corfac_slope <- summary(mod)$coefficients[[2]]
  corfac_intercept <- summary(mod)$coefficients[[1]]
  
  df <- df %>% 
    mutate(crm_corr_factor = DOY_TCO2*corfac_slope + corfac_intercept) %>% 
    mutate(crm_corr_tco2 = tco2_umolkg*crm_corr_factor)
  
  
  
  ## Headspace corrections for the TCO2
  # First, get the atm values from the gas standard fits (not from the room air CO2)
  # Correct them for the standards.
  # Let this change through time (so a drift-corrected atm value for the headspace correction)
  # Also convert the atm values to uatm to be consistent with other records.
  atm_val1 <- (gstd_fits$fit1$atm_val*gstd_fits$fit1$slope + gstd_fits$fit1$intercept)*gstd_fits$fit1$atm_pressure/101.325
  atm_ts1 <- gstd_fits$fit1$average_time
  atm_val2 <- (gstd_fits$fit2$atm_val*gstd_fits$fit2$slope + gstd_fits$fit2$intercept)*gstd_fits$fit2$atm_pressure/101.325
  atm_ts2 <- gstd_fits$fit2$average_time
  atm_val3 <- (gstd_fits$fit3$atm_val*gstd_fits$fit3$slope + gstd_fits$fit3$intercept)*gstd_fits$fit3$atm_pressure/101.325
  atm_ts3 <- gstd_fits$fit3$average_time
  
  atm_drift_factor1 <- (atm_val2 - atm_val1)/(atm_ts2 - atm_ts1)
  atm_drift_factor2 <- (atm_val3 - atm_val2)/(atm_ts3 - atm_ts2)
  
  df <- df %>% 
    mutate(atm_pco2 = ifelse(julian_dt < atm_ts2, atm_val1 + atm_drift_factor1*(julian_dt - atm_ts1),
                             atm_val2 + atm_drift_factor2*(julian_dt - atm_ts2))) 
  
  
  # Volume info for headspace corrections
  # These are venue-specific!
  approx_sample_vol = if(run_id < 2200){
    300
  } else {325}
  
  pco2_headspace_vol = if(run_id < 2200){
    100
  } else {90}
  
  df <- df |> 
    mutate(hdspc_adjusted_tco2 = crm_corr_tco2 - ((pco2_uatm - atm_pco2)*pco2_headspace_vol/22414)*1000/approx_sample_vol)
  
  
  ## Export the stuff! Namely, the crm info, the standard info, and the data. Also,
  # append the crm batch to the crm compilation
  crm_info <- crm_info |> 
    mutate(run_id = first(df$run_number)) |> 
    mutate(batch = crm_batch) |> 
    mutate(certified_tco2 = crm_tco2) |> 
    mutate(certified_ta = crm_ta) |> 
    mutate(certified_phos = crm_phos) |> 
    mutate(certified_sil = crm_sil) |> 
    mutate(certified_sal = crm_sal)
  
  
  list(gstd_fits = gstd_fits, lstd_fits = lstd_fits, crm_info = crm_info, 
       discrete_samples = df, sample_inventory_path = sample_inventory_path)
  
}

