
# This function creates the final files for export. Its purpose is to make it easy for analysts to copy and paste
# ouputs into their existing records. As such, it is specifically tailored to the current end users. Any future users
# would need to heavily alter the code in this function to reflect their own data records. 

# Records of interest include:
# Gas and liquid fits and intercepts
# CRM performance metrics
# The processed discrete data with some basic QA/QC based on the standard fits and CRM accuracy and precision.

library(tidyverse)
library(data.table)
library(seacarb)

f5 <- function(f4_output,
               sd_thresh_severe = 15, sd_thresh_moderate = 10,
               corfac_upper = 1.03, corfac_lower = 0.97,
               r2_dev_severe = 0.05, r2_dev_moderate = 0.01,
               k1k2 = "m10", kf = "pf", ks = "d", b = "u74",
               pHscale = "T", eos = "eos80"){
  gstd_fits <- f4_output$gstd_fits
  lstd_fits <- f4_output$lstd_fits
  crm_info <- f4_output$crm_info
  df <- f4_output$discrete_samples
  
  run_id <- first(df$run_number)
  
  df <- df |> 
    arrange(julian_dt)
  
  ## Add some rudimentary QC based on the CRM SD and standard fits
  df <- df |> 
    mutate(run_performance = case_when(max(crm_info$crm_sd_closest) > sd_thresh_severe ~ 3,
                                       max(crm_info$crm_sd_closest) > sd_thresh_moderate ~ 2,
                                       max(crm_info$crm_corfac) > corfac_upper | min(crm_info$crm_corfac) < corfac_lower ~ 3,
                                       max(abs(c(gstd_fits$fit1$r_squared, gstd_fits$fit2$r_squared, gstd_fits$fit3$r_squared) - 1)) > r2_dev_severe ~ 3,
                                       max(abs(c(gstd_fits$fit1$r_squared, gstd_fits$fit2$r_squared, gstd_fits$fit3$r_squared) - 1)) > r2_dev_moderate ~ 2,
                                       max(abs(c(lstd_fits$fit1$r_squared, lstd_fits$fit2$r_squared, lstd_fits$fit3$r_squared) - 1)) > r2_dev_severe ~ 3,
                                       max(abs(c(lstd_fits$fit1$r_squared, lstd_fits$fit2$r_squared, lstd_fits$fit3$r_squared) - 1)) > r2_dev_moderate ~ 2,
                                       TRUE ~ 1))
  

  crms <- df |> 
    filter(type == "CRM")
  
  
  # Cut down to the columns of interest for the final records sheet, order appropriately
  # Assume the analyst will want to copy over this information somewhere it can be matched with any notes from the sample
  # collector.
  
  samples <- df |> 
    filter(type == "sample") |> 
    select(run_number, analysis_date_AK, Sample_ID,
           T_insitu, Smpl_S, pCO2_analysis_T, pco2_uatm, tco2_umolkg, crm_corr_tco2, hdspc_adjusted_tco2, correct_TA, pco2_insi,
           pco2_20C, pH_insi, omega_ar, analyst, run_performance) |> 
    rename(Discrete_Run = run_number, Date_Analyzed = analysis_date_AK, Sample_T = T_insitu, Salinity = Smpl_S,
           Analysis_T = pCO2_analysis_T, Analysis_pCO2 = pco2_uatm, Analysis_TCO2 = tco2_umolkg,
           CRM_corrected_TCO2 = crm_corr_tco2, Headspace_Corrected_TCO2 = hdspc_adjusted_tco2,
           pCO2_at_situ = pco2_insi, pCO2_at_20C = pco2_20C, pH_situ = pH_insi, Omega_Arag = omega_ar,
           Analyst_Name = analyst, Run_Performance = run_performance)
  
  
  
  ## Fill out the run metadata table:
  # Have to quickly calculate the crm drift slope and offset
  crm_drift <- lm(crm_corfac ~ crm_time, data = crm_info)
  
  run_metadata <- data.frame(run = run_id,
                             gstd1_r2 = gstd_fits$fit1$r_squared,
                             gstd1_m = gstd_fits$fit1$slope,
                             gstd1_b = gstd_fits$fit1$intercept,
                             gstd2_r2 = gstd_fits$fit2$r_squared,
                             gstd2_m = gstd_fits$fit2$slope,
                             gstd2_b = gstd_fits$fit2$intercept,
                             gstd3_r2 = gstd_fits$fit3$r_squared,
                             gstd3_m = gstd_fits$fit3$slope,
                             gstd3_b = gstd_fits$fit3$intercept,
                             lstd1_r2 = lstd_fits$fit1$r_squared,
                             lstd1_m = lstd_fits$fit1$slope,
                             lstd1_b = lstd_fits$fit1$intercept,
                             lstd2_r2 = lstd_fits$fit2$r_squared,
                             lstd2_m = lstd_fits$fit2$slope,
                             lstd2_b = lstd_fits$fit2$intercept,
                             lstd3_r2 = lstd_fits$fit3$r_squared,
                             lstd3_m = lstd_fits$fit3$slope,
                             lstd3_b = lstd_fits$fit3$intercept,
                             crm1_sddev = crm_info$crm_sd_closest[1],
                             crm2_sddev = crm_info$crm_sd_closest[2],
                             crm1_corfac = crm_info$crm_corfac[1],
                             crm2_corfac = crm_info$crm_corfac[2],
                             corfac_slope = crm_drift$coefficients[2],
                             corfac_intercept = crm_drift$coefficients[1])
  
  
  
  
  ## Finally, prep the CRM datasheet. This will be a bit involved since it's recreating every intermediate column
  # in an excel processing spreadsheet developed by Wiley Evans.
 
  
  # Step 1: calculate the "official" expected pCO2 using seacarb
  crm_phos <- first(crm_info$certified_phos)*10^-6
  crm_sil <- first(crm_info$certified_sil)*10^-6
  
  crms <- crms |> 
    mutate(crm_batch = first(crm_info$batch),
           crm_cert_tco2 = first(crm_info$certified_tco2),
           crm_cert_TA = first(crm_info$certified_ta),
           crm_S = first(crm_info$certified_sal))
  
  ## Carbcalc step 2: Get the in-situ conditions using the TA and TCO2
  
  data <- crms |> 
    filter(!is.na(T_insitu))
  
  carb_out <- carb(
    flag = 15,
    var1 = data$crm_cert_TA*10^-6, 
    var2 = data$crm_cert_tco2*10^-6,
    S = data$crm_S,
    T = data$T_insitu,
    k1k2 = k1k2,
    kf = kf,
    ks = ks,
    b = b,
    pHscale = pHscale,
    eos = eos,
    Pt = crm_phos,
    Sit = crm_sil
  )
  
  data$pco2_expected <- carb_out$pCO2
  
  crms <- left_join(crms, data)
  
  
  
  
  
  data <- crms |> 
    mutate(RHOW = 999.842594+(0.06793952 + (-0.00909529 + (0.0001001685 + (-0.000001120083 + 0.000000006536332*pCO2_analysis_T)*pCO2_analysis_T)*pCO2_analysis_T)*pCO2_analysis_T)*pCO2_analysis_T) |> 
    mutate(B = (0.824493 + (-0.0040899 + (0.000076438 + (-0.00000082467 + 0.0000000053875*pCO2_analysis_T)*pCO2_analysis_T)*pCO2_analysis_T)*pCO2_analysis_T)*crm_S) |> 
    mutate(C = (-0.00572466 + (0.00010227 + -0.0000016546*pCO2_analysis_T)*pCO2_analysis_T)*crm_S*sqrt(crm_S)) |> 
    mutate(D = 0.00048314*crm_S*crm_S) |> 
    mutate(crm_density_kgL = (RHOW + B + C + D)/1000) |> 
    mutate(crm_cert_tco2_uM = crm_cert_tco2*crm_density_kgL) |> 
    mutate(crm_anal_tco2_uM = tco2_umolkg*crm_density_kgL) |> 
    mutate(crm_corrfac_from_umolkg = crm_cert_tco2/tco2_umolkg) |> 
    mutate(crm_corrfac_from_uM = crm_cert_tco2_uM/crm_anal_tco2_uM) |> 
    mutate(crmanal_minus_crmcert_umolkg = tco2_umolkg - crm_cert_tco2) |> 
    group_by(set) |> 
    mutate(mean_of_reps = mean(tco2_umolkg, na.rm = TRUE)) |>
    mutate(mean_crmanal_minus_crmcert = mean(crmanal_minus_crmcert_umolkg, na.rm = TRUE)) |> 
    mutate(sd_of_reps = sd(tco2_umolar, na.rm = TRUE)) |> 
    mutate(corfac = mean(crm_corrfac_from_umolkg, na.rm = TRUE)) |> 
    mutate(corfac_var = sd(crm_corrfac_from_umolkg, na.rm = TRUE)) |> 
    mutate(rep_no = row_number()) |> 
    mutate(num_reps = n()) |> 
    ungroup() |> 
    mutate(mean_of_reps = ifelse(rep_no != num_reps, NA, mean_of_reps)) |> 
    mutate(sd_of_reps = ifelse(rep_no != num_reps, NA, sd_of_reps)) |> 
    mutate(corfac = ifelse(rep_no != num_reps, NA, corfac)) |> 
    mutate(corfac_var = ifelse(rep_no != num_reps, NA, corfac)) |> 
    mutate(mean_crmanal_minus_crmcert = ifelse(rep_no != num_reps, NA, mean_crmanal_minus_crmcert)) |> 
    mutate(percent_diff_TA = (1-crm_cert_TA/correct_TA)*100) |> 
    mutate(percent_diff_pCO2 = (1-pco2_expected/pco2_uatm)) |> 
    mutate(percent_diff_tco2 = (1-crm_cert_tco2/mean_of_reps)) |> 
    mutate(bottle_id = NA) |> 
    rename(date = analysis_date_AK, analysis_T = T_insitu, crm_anal_tco2 = tco2_umolkg, pCO2_analyzed = pco2_uatm,
           pCO2_calculated = pco2_expected, calculated_TA = correct_TA) |> 
    select(run_number, date, crm_batch, bottle_id, crm_cert_tco2, analysis_T, crm_S,
           RHOW, B, C, D, crm_density_kgL, crm_cert_tco2_uM, crm_anal_tco2, crm_anal_tco2_uM,
           crm_corrfac_from_umolkg, crm_corrfac_from_uM, crmanal_minus_crmcert_umolkg,
           mean_of_reps, mean_crmanal_minus_crmcert, sd_of_reps, corfac, corfac_var, pCO2_analyzed, pCO2_calculated,
           calculated_TA, crm_cert_TA, percent_diff_TA, percent_diff_pCO2, percent_diff_tco2)
  
  crm_performance <- data
  
  ## Return everything as a named list of data frames, ready for Shiny download handlers
  list(run_id = run_id,
       all_columns = df,
       formatted_samples = samples,
       run_metadata = run_metadata,
       crm_performance = crm_performance)
}