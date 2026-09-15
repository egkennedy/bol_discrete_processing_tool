
## This script goes through the standard, atmospheric, and drift corrections from 
# Wiley Evan's-developed Excel processing sheets. 
# It will also need to retain and pass through the CRM information 

## NOTE: this is written assuming mid-run standards. However, the script does not care how many samples are before or
# after the "mid-run" standards. For groups that do not do mid-run standards, having two sets of post-run standards
# would work, and would not influence the data processing. The mid-run standards act as a hinge for two sequential
# linear drift functions, not as a middle point in a smooth drift curve.

library(tidyverse)
library(data.table)
library(seacarb)



f2 <- function(f1_output){
  # extract the stuff
  gs_ref <- f1_output$gs_ref
  ls_ref <- f1_output$ls_ref
  atm_P <- f1_output$atm_P
  crm_batch <- f1_output$crm_batch
  df <- f1_output$fullrun
  crm_batch_info_path <- f1_output$crm_batch_info_path
  sample_inventory_path <- f1_output$sample_inventory_path
  
  run_id <- first(df$run_number)
  
  
  ## Some reference values for sample volume, expected TCO2 blank, etc from the "START HERE" forms
  # These come directly from the Excel sheets.
  TCO2_blank = 0 
  T_slope = 1
  T_intercept = 0
  
  
  # Get indicies of the relevant sets and average times
  gstd_sets <- df |> filter(type == "gas standard") |> 
    group_by(set) |> 
    summarize(average_dt = mean(julian_dt)) |> 
    arrange(average_dt)
  lstd_sets <- df |> filter(type == "liquid standard") |> 
    group_by(set) |> 
    summarize(average_dt = mean(julian_dt)) |> 
    arrange(average_dt)
  
  
  
  ### Correction for the solution density
  std_sol_density <- function(lab_T){
    D9 <- 6.536332e-09
    D10 <- -1.120083e-06
    D11 <- 1.001685e-04
    D12 <- -9.09529e-03
    D13 <- 6.793952e-02
    D14 <- 999.842594
    
    (((((lab_T*D9 + D10)*lab_T + D11)*lab_T + D12)*lab_T + D13)*lab_T + D14)/1000
  }
  
  # Add the solution density to the data
  sol_density <- std_sol_density(first(df$lab_temp))
  
  
  
  ## Gas standard fits and drift correction slope and intercepts.
  # using straight linear interpolation between standard sets - nothing fancy. So it kinks at the second standard set
  # Samples are corrected using the two sets of standards surrounding them.
  gs_vals <- function(data, standard_set){
    gas <- data %>% 
      filter(set == standard_set & str_detect(Sample_ID, "Atm") == FALSE) %>% 
      select(DOY_pCO2, datetime, Sample_ID, pCO2_xCO2, type)
    
    atm_val <- data |> 
      filter(set == standard_set & str_detect(Sample_ID, "Atm") == TRUE & !is.na(pCO2_xCO2)) |> 
      select(pCO2_xCO2) |> unlist()
    
    atm_pressure <- data |> 
      filter(set == standard_set & str_detect(Sample_ID, "Atm") == TRUE & !is.na(pCO2_Hdspc_P)) |> 
      select(pCO2_Hdspc_P) |> unlist()
    
    mod <- lm(gs_ref$std_value ~ pCO2_xCO2, data = gas)
    
    slope <- mod$coefficients[[2]]
    intercept <- mod$coefficients[[1]]
    r_sq <- summary(mod)$r.squared
    
    average_time <- mean(gas$DOY_pCO2, na.rm = TRUE)
    
    list(slope = slope, intercept = intercept, r_squared = r_sq, average_time = average_time,
         atm_val = atm_val, atm_pressure = atm_pressure)
    
  }
  
  gs1 <- gs_vals(df, gstd_sets$set[1])
  gs2 <- gs_vals(df, gstd_sets$set[2])
  gs3 <- gs_vals(df, gstd_sets$set[3])
  
  gas_slope_factor1 <- (gs2$slope - gs1$slope)/(gs2$average_time - gs1$average_time)
  gas_slope_factor2 <- (gs3$slope - gs2$slope)/(gs3$average_time - gs2$average_time)
  
  gas_intercept_factor1 <- (gs2$intercept - gs1$intercept)/(gs2$average_time - gs1$average_time)
  gas_intercept_factor2 <- (gs3$intercept - gs2$intercept)/(gs3$average_time - gs2$average_time)
  
  
  ls_vals <- function(data, standard_set){
    liquid <- data %>% 
      filter(set == standard_set) %>% 
      select(DOY_TCO2, datetime, Sample_ID, TCO2_xCO2, type)
    
    average_time <- mean(liquid$DOY_TCO2)
    
    if(average_time < gs2$average_time){
      gs_slope_correction <- gs1$slope + (average_time - gs1$average_time)*gas_slope_factor1
      intercept_slope_correction <- gs1$intercept + (average_time - gs1$average_time)*gas_intercept_factor1
    } else {
      gs_slope_correction <- gs2$slope + (average_time - gs2$average_time)*gas_slope_factor2
      intercept_slope_correction <- gs2$intercept + (average_time - gs2$average_time)*gas_intercept_factor2
    }
    
    liquid <- liquid %>% 
      mutate(TCO2_gs_corr = gs_slope_correction*TCO2_xCO2 + intercept_slope_correction)
    
    # Density corrections to the true liquid standard values
    ls_ref <- ls_ref |> 
      mutate(corr_std_value = (std_value + TCO2_blank)/sol_density)
    
    mod <- lm(ls_ref$corr_std_value ~ TCO2_gs_corr, data = liquid)
    
    slope <- mod$coefficients[[2]]
    intercept <- mod$coefficients[[1]]
    r_sq <- summary(mod)$r.squared
    
    average_time <- mean(liquid$DOY_TCO2, na.rm = TRUE)
    
    list(slope = slope, intercept = intercept, r_squared = r_sq, average_time = average_time)
    
  }
  
  
  ls1 <- ls_vals(df, lstd_sets$set[1])
  ls2 <- ls_vals(df, lstd_sets$set[2])
  ls3 <- ls_vals(df, lstd_sets$set[3])
  
  liquid_slope_factor1 <- (ls2$slope - ls1$slope)/(ls2$average_time - ls1$average_time)
  liquid_slope_factor2 <- (ls3$slope - ls2$slope)/(ls3$average_time - ls2$average_time)
  
  liquid_intercept_factor1 <- (ls2$intercept - ls1$intercept)/(ls2$average_time - ls1$average_time)
  liquid_intercept_factor2 <- (ls3$intercept - ls2$intercept)/(ls3$average_time - ls2$average_time)
  
  
  ## Get atmospheric drift factors
  atm_pressure <- df %>% 
    filter(type == "gas standard" & !is.na(pCO2_Hdspc_P)) %>% 
    select(pCO2_Hdspc_P) %>% unlist()
  
  atm_pressure_times <- df %>% 
    filter(type == "gas standard" & !is.na(pCO2_Hdspc_P)) %>% 
    select(DOY_pCO2) %>% unlist()
  
  atm_drift_factor1 <- (atm_pressure[2] - atm_pressure[1])/(atm_pressure_times[2] - atm_pressure_times[1])
  atm_drift_factor2 <- (atm_pressure[3] - atm_pressure[2])/(atm_pressure_times[3] - atm_pressure_times[2])
  
  
  
  ### Correct the data for the gas and liquid standards + drift
  discrete <- df %>% 
    filter(type == "CRM" | type == "sample")
  
  discrete <- discrete %>% 
    mutate(pco2_headspace_P = ifelse(DOY_pCO2 < gs2$average_time, (atm_pressure[1] + atm_drift_factor1*(DOY_pCO2 - atm_pressure_times[1]))/101.325,
                                     (atm_pressure[2] + atm_drift_factor2*(DOY_pCO2 - atm_pressure_times[2]))/101.325)) |> 
    mutate(corrected_analysis_T = pCO2_analysis_T*T_slope + T_intercept) %>% 
    mutate(tco2_gas_cal_slope = ifelse(DOY_TCO2 < gs2$average_time, gs1$slope + (DOY_TCO2 - gs1$average_time)*gas_slope_factor1,
                                       gs2$slope + (DOY_TCO2 - gs2$average_time)*gas_slope_factor2)) %>% 
    mutate(tco2_gas_cal_intercept = ifelse(DOY_TCO2 < gs2$average_time, gs1$intercept +(DOY_TCO2 - gs1$average_time)*gas_intercept_factor1,
                                           gs2$intercept +(DOY_TCO2 - gs2$average_time)*gas_intercept_factor2)) %>% 
    mutate(liquid_cal_slope = ifelse(DOY_TCO2 < ls2$average_time, ls1$slope + (DOY_TCO2 - ls1$average_time)*liquid_slope_factor1,
                                     ls2$slope + (DOY_TCO2 - ls2$average_time)*liquid_slope_factor2)) %>% 
    mutate(liquid_cal_intercept = ifelse(DOY_TCO2 < ls2$average_time, ls1$intercept + (DOY_TCO2 - ls1$average_time)*liquid_intercept_factor1,
                                         ls2$intercept + (DOY_TCO2 - ls2$average_time)*liquid_intercept_factor2)) %>% 
    mutate(pco2_gas_cal_slope = ifelse(DOY_pCO2 < gs2$average_time, gs1$slope + (DOY_pCO2 - gs1$average_time)*gas_slope_factor1,
                                       gs2$slope + (DOY_pCO2 - gs2$average_time)*gas_slope_factor2)) %>% 
    mutate(pco2_gas_cal_intercept = ifelse(DOY_pCO2 < gs2$average_time, gs1$intercept + (DOY_pCO2 - gs1$average_time)*gas_intercept_factor1,
                                           gs2$intercept + (DOY_pCO2 - gs2$average_time)*gas_intercept_factor2)) %>% 
    mutate(pco2_uatm = (pco2_gas_cal_slope*pCO2_xCO2 + pco2_gas_cal_intercept)*pco2_headspace_P) %>% 
    mutate(tco2_umolar = (TCO2_xCO2*tco2_gas_cal_slope + tco2_gas_cal_intercept)*liquid_cal_slope + liquid_cal_intercept) %>% 
    filter(!is.na(DOY_TCO2) | !is.na(DOY_pCO2))
  
  
  ## Convert the tco2 to umol/kg
  umolar_to_umolkg <- function(tco2_umolar, pCO2_analysis_T, Smpl_S){
    top <- 1000*(tco2_umolar*1 + 0)
    
    bottom <- (((((0.000000006536332*pCO2_analysis_T-0.000001120083)*pCO2_analysis_T+0.0001001685)*pCO2_analysis_T-0.00909529)*
                  pCO2_analysis_T+0.06793952)*pCO2_analysis_T+999.842594+((((0.0000000053875*pCO2_analysis_T-0.00000082467)*pCO2_analysis_T+0.000076438)*pCO2_analysis_T-0.004089)
                                                                          *pCO2_analysis_T+0.824493)*Smpl_S+((-0.0000016546*pCO2_analysis_T+0.00010227)*pCO2_analysis_T-0.00572466)*Smpl_S^1.5+0.00048314*Smpl_S*Smpl_S)
    
    top/bottom
  }
  
  discrete <- discrete |> 
    mutate(tco2_umolkg = umolar_to_umolkg(tco2_umolar, pCO2_analysis_T, Smpl_S))
  
  # Export
  gstd_fits <- list(fit1 = gs1, fit2 = gs2, fit3 = gs3, ref_vals = gs_ref)
  lstd_fits <- list(fit1 = ls1, fit2 = ls2, fit3 = ls3, ref_vals = ls_ref)
  
  list(gstd_fits = gstd_fits, lstd_fits = lstd_fits, crm_batch = crm_batch, crm_batch_info_path = crm_batch_info_path,
       discrete_samples = discrete, sample_inventory_path = sample_inventory_path)
  
}
