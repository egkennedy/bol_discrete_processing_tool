# Burke-o-Lator Discrete Sample Processing Tool
## Introduction
This code and R Shiny app was developed to help Alaskan Burke-o-Lator (BoL) operators process discrete sample data. The overall processing protocol was developed by Dr. Wiley Evans at the Hakai Institute and has been described in Evans et al., 2015 and the references therein.

Dr. Evans had previously adapted his processing scripts to Excel and Google spreadsheets with embedded formulas, allowing operators to correct the raw data and calculate the full suite of carbonate parameters. These scripts and app were built from the Google sheets to automate and speed up the process while reducing opportunities for error. These scripts were built with a specific end user in mind and would need to be lightly edited by any future users (particularly f5_public.R), but are posted here for any BoL operators to download and adapt.

Interested users are encouraged to contact Esther Kennedy at esther.kennedy3@gmail.com with questions.

## Contents
R scripts: the functions f1_public.R, f2_public.R, f3_public.R, f4_public.R, and f5_public.R support the Shiny script app_public.R. All scripts are heavily commented and future users are recommended to read through all before using.

CSV files:
- crm_batch_info: this file contains the chemical information from select batches of Andrew Dickson's Certified Reference Materials for seawater, documented at https://www.ncei.noaa.gov/access/ocean-carbon-acidification-data-system/oceans/Dickson_CRM/batches.html.

- test_raw_desc_sample_public: this is a test example of the data this tool is designed to process. This file is a version of the BoL's DescSmpl file outputs, but some light cleaning and QC has been applied which is described below.

- bottle_inventory_public: this is a slim test example an organization's local inventory, where the collection information about discrete samples (e.g., in-situ temperature) is stored. 

BoL Fit files (BoL_std_fit_files_example.zip)
- These files are GstdFit and LstdFit files automatically exported by the BoL. They are provided here as an example, but future users should point their local app toward the directory their BoL exports to.

## Usage:
Downloading this tool:
 1. The app_public.zip contains all the scripts and referenced csv files within the same Rproject. These should stay together within the same directory.
 2. Create an "exported_data" folder within the same directory as the Rproject.
 3. Unzip the BoL_std_fit_files_example.zip a different folder. Users will need to point the app towards this folder or their true BoL export folder explicitly in the app_public.R script in line 16.
    
Before processing data tool:
 This tool assumes BoL operators have pulled the raw DescSmpl export from their discrete sample run on the BoL into a spreadsheet and made the following corrections if necessary:
 1. Eliminated extra gas and liquid standard runs such that there is one set of pre-sample standards, one set of mid-run standards, and one set of post-run standards. If no mid-run standards occurred, operators must include two sets of post-run standards.
 2. Sample names exactly match those in the local inventory.
 3. Any mis-logged or duplicated rows are eliminated. Mislogged salinities are corrected.
 4. All pre- and post- sample CRMs have "CRM" in the sample name.

What this tool does:
 1. Applies drift-adjusted gas- and liquid standard corrections to the samples and CRMs.
 2. Corrects for the headspace gas in the bottles.
 3. Calculates the full in-situ suite of carbonate parameters as well as the pCO2 at 20C using user-specified coefficients. The defaults are Millero (2010) for K1 and K2, Perez and Fraga (1987) for Kf, Dickson (1990) for Ks, and Uppstrom (1974) for the concentration of total boron.
 4. Formats the processed data, CRMs, and run metadata for export.

## References
Dickson A. G., 1990 Standard potential of the reaction: AgCI(s) + 1/2H2(g) = Ag(s) + HCI(aq), and the standard acidity constant of the ion HSO4 in synthetic sea water from 273.15 to 318.15 K. Journal of Chemical Thermodynamics 22, 113-127.

Evans E., Mathis J. T., Ramsay J., and Hetrick J., 2015. On the frontline: Tracking ocean acidification in an ALaskan shellfish hatchery. PLoS ONE 10(7), e0130384.

Millero F. J., 2010. Carbonate constant for estuarine waters. Marine and Freshwater Research 61: 139-142.

Perez F. F. and Fraga F., 1987 Association constant of fluoride and hydrogen ions in seawater. Marine Chemistry 21, 161-168.

Uppstrom L.R., 1974 The boron/chlorinity ratio of the deep-sea water from the Pacific Ocean. Deep-Sea Research I 21, 161-162.

