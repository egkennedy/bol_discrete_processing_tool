
# This matches the data with the in-situ temperatures
# It relies on the sample names exactly matching those in an inventory (stored elsewhere) that includes
# the collector-measured temperature and any other collection information, saved as a csv file. Currently,
# this references in-situ column names from a specific sample inventory. This would need to be updated in line 35 for other users.

# The carbonate calculations here use those specified by Wiley Evans in his Excel processing sheets. However, these
# can be changed.

library(tidyverse)
library(data.table)
library(seacarb)

f4 <- function(f3_output, k1k2 = "m10", kf = "pf", ks = "d", b = "u74",
               pHscale = "T", eos = "eos80"){
  
  # Extract the stuff
  gstd_fits <- f3_output$gstd_fits
  lstd_fits <- f3_output$lstd_fits
  crm_info <- f3_output$crm_info
  sample_inventory_path <- f3_output$sample_inventory_path
  df <- f3_output$discrete_samples
  
  # Extract specific crm nutrient values
  crm_phos <- first(crm_info$certified_phos)*10^-6
  crm_sil <- first(crm_info$certified_sil)*10^-6
  
  
  ## Get the sample inventory 
  inventory <- read_csv(sample_inventory_path)
  
  # The only columns that truly matter in the inventory are the sample name and the temperature. 
  # Identify them and rename them.
  inventory <- inventory |> 
    rename(Sample_ID = `Sample ID`, T_insitu = `Sample Water Temp\n°C`) |> 
    select(Sample_ID, T_insitu)
  
  # Match by sample ID. NAs are fine, those samples will just have no in-situ calculations
  df <- left_join(df, inventory)
  
  # Populate the sample_temp column with the analysis temp for the CRMs
  df <- df |> 
    mutate(T_insitu = as.numeric(T_insitu)) |> 
    mutate(T_insitu = case_when(type == "sample" & str_detect(Sample_ID, "crm") == FALSE ~ T_insitu,
                                type == "sample" & str_detect(Sample_ID, "crm") == TRUE ~ pCO2_analysis_T,
                                type == "CRM" ~ pCO2_analysis_T,
                                TRUE ~ NA_real_))
  
  
  # Make extra sure the crm salinities are correct
  df <- df |> 
    mutate(Smpl_S = ifelse(type == "CRM", first(crm_info$certified_sal), Smpl_S))
  
  
  ## Break the data into a CRM df and a sample df
  crms <- df |> 
    filter(type == "CRM")
  
  
  samples <- df |> 
    filter(type != "CRM")
  
  
  
  ### Carbcalc step 1: get the correct TA
  for(i in c("CRM", "sample")){
    if (i == "CRM"){
      data <- crms |> 
        filter(!is.na(pco2_uatm) & !is.na(hdspc_adjusted_tco2) & !is.na(Smpl_S) & !is.na(pCO2_analysis_T))
    } else {
      data <- samples |> 
        filter(!is.na(pco2_uatm) & !is.na(hdspc_adjusted_tco2)& !is.na(Smpl_S) & !is.na(pCO2_analysis_T))
    }
    
    carb_out <- carb(
      flag = 25,
      var1 = data$pco2_uatm, 
      var2 = data$hdspc_adjusted_tco2*10^-6,
      S = data$Smpl_S,
      T = data$pCO2_analysis_T,
      k1k2 = k1k2,
      kf = kf,
      ks = ks,
      b = b,
      pHscale = pHscale,
      eos = eos,
      Pt = if(i == "CRM"){
        crm_phos
      } else {0},
      Sit = if(i == "CRM"){
        crm_sil
      } else {0}
    )
    
    data$correct_TA <- carb_out$ALK*10^6
    
    if (i == "CRM"){
      crms <- left_join(crms, data)
    } else {
      samples <- left_join(samples, data)
    }
    
  }
  
  
  ## Carbcalc step 2: Get the in-situ conditions using the TA and TCO2
  for(i in c("CRM", "sample")){
    if (i == "CRM"){
      data <- crms |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(T_insitu) & !is.na(Smpl_S))
    } else {
      data <- samples |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(T_insitu) & !is.na(Smpl_S))
    }
    
    carb_out <- carb(
      flag = 15,
      var1 = data$correct_TA*10^-6, 
      var2 = data$crm_corr_tco2*10^-6,
      S = data$Smpl_S,
      T = data$T_insitu,
      k1k2 = k1k2,
      kf = kf,
      ks = ks,
      b = b,
      pHscale = pHscale,
      eos = eos,
      Pt = if(i == "CRM"){
        crm_phos
      } else {0},
      Sit = if(i == "CRM"){
        crm_sil
      } else {0}
    )
    
    data$pH_insi <- carb_out$pH
    data$pco2_insi <- carb_out$pCO2
    data$omega_ar <- carb_out$OmegaAragonite
    data$omega_ca <- carb_out$OmegaCalcite
    
    if (i == "CRM"){
      crms <- left_join(crms, data)
    } else {
      samples <- left_join(samples, data)
    }
    
  }
  
  
  ## Carbcalc step 3: Get the pCO2 at 20C
  for(i in c("CRM", "sample")){
    if (i == "CRM"){
      data <- crms |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(Smpl_S))
    } else {
      data <- samples |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(Smpl_S))
    }
    
    carb_out <- carb(
      flag = 15,
      var1 = data$correct_TA*10^-6, 
      var2 = data$crm_corr_tco2*10^-6,
      S = data$Smpl_S,
      T = 20,
      k1k2 = k1k2,
      kf = kf,
      ks = ks,
      b = b,
      pHscale = pHscale,
      eos = eos,
      Pt = if(i == "CRM"){
        crm_phos
      } else {0},
      Sit = if(i == "CRM"){
        crm_sil
      } else {0}
    )
    
    data$pco2_20C <- carb_out$pCO2
    
    if (i == "CRM"){
      crms <- left_join(crms, data)
    } else {
      samples <- left_join(samples, data)
    }
    
  }
  
  
  ## Step 4: Get the revelle factor. Slow, but not too bad for a discrete run.
  for(i in c("CRM", "sample")){
    if (i == "CRM"){
      data <- crms |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(T_insitu) & !is.na(Smpl_S))
    } else {
      data <- samples |> 
        filter(!is.na(correct_TA) & !is.na(crm_corr_tco2) & !is.na(T_insitu) & !is.na(Smpl_S))
    }
    
    carb_out <- buffergen(
      flag = 15,
      var1 = data$correct_TA*10^-6, 
      var2 = data$crm_corr_tco2*10^-6,
      S = data$Smpl_S,
      T = data$T_insitu,
      k1k2 = k1k2,
      kf = kf,
      ks = ks,
      b = b,
      pHscale = pHscale,
      Pt = if(i == "CRM"){
        crm_phos
      } else {0},
      Sit = if(i == "CRM"){
        crm_sil
      } else {0}
    )
    
    data$revelle_out <- as.numeric(carb_out$RF)
    
    if (i == "CRM"){
      crms <- left_join(crms, data)
    } else {
      samples <- left_join(samples, data)
    }
    
  }
  
  # Rejoin the data and order it. In next (final) function, I'll make the final export files.
  
  data <- rbind(crms, samples)
  
  
  ## Export stuff
  list(gstd_fits = gstd_fits, lstd_fits = lstd_fits, crm_info = crm_info, discrete_samples = data)
}
