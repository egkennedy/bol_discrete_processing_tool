invisible(lapply(c("shiny", "shinyFiles", "tidyverse", "data.table", "seacarb", "DT", "here"),
                 function(pkg) if (!require(pkg, character.only = TRUE)) { install.packages(pkg); library(pkg, character.only = TRUE) }))

source("f1_public.R")
source("f2_public.R")
source("f3_public.R")
source("f4_public.R")
source("f5_public.R")

# --- Helper -----------------------------------------------------------------
# f1() scans a folder (data_filepath) and matches Gstd/Lstd calibration files
# by filename pattern + timestamp embedded in the filename. Since this app
# runs locally on the BoL's export computer, we can point it directly at the
# BoL export folder.

volumes <- c("BoL Fit Files" = "PATH_TO_EXPORT_FILES",
             "Project" = here(),
             "Processed Data" = here("exported_data"),
             Home = fs::path_home(),
             getVolumes()())

# Same shortcuts, reordered so the "Save outputs" folder picker defaults to
# the Export folder instead of BoL Fit Files.
output_volumes <- c(volumes["Processed Data"], volumes[names(volumes) != "Processed Data"])


# --- UI -----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Discrete Carbonate Chemistry Processing Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      
      h4("1. Run information"),
      helpText("Details about this analytical run, update EVERY time."),
      numericInput("run_number", "Run number", value = NA, min = 0),
      dateInput("run_date", "Run date (AK Time)", value = Sys.Date()),
      textInput("analyst", "Analyst name", value = ""),
      numericInput("lab_temp", "Current lab temperature (\u00b0C)", value = NA),
      numericInput("roomair_co2", "Room air CO2 (ppm)", value = 420),
      numericInput("crm_batch", "CRM batch number", value = NA),
      
      hr(),
      h4("2. Upload files"),
      helpText("Upload your clean version of the run's `DescSmpl` file and saved as a csv. Do not change any column names. This should have correct salinity info and no extra standards."),
      fileInput("bol_data", "Discrete sample file (.csv)", accept = ".csv"),
      helpText("Point to the folder where the BoL exports its raw run files (Gstd/Lstd fit files, etc). The tool automatically finds the fit files closest in time to your samples \u2014 you don't need to pick them out yourself or upload them."),
      shinyDirButton("fitfiles_dir", "Browse for the folder the BoL exports to\u2026", "Select the folder containing the Gstd/Lstd fit files"),
      verbatimTextOutput("fitfiles_dir_display"),
      helpText("The CRM batch reference sheet (certified values for tco2, ta, salinity, etc.). Make sure you keep this updated."),
      fileInput("crm_batch_file", "CRM batch info (.csv)", accept = ".csv"),
      helpText("The most recent sample inventory, downloaded or saved as a .csv. Used to get the in-situ temperatures."),
      fileInput("inventory_file", "Sample inventory (.csv)", accept = ".csv"),
      
      hr(),
      h4("3. Carbonate chemistry constants"),
      helpText("Dissociation-constant choices used by seacarb for the carbonate system calculations. Defaults match current standard practice \u2014 change only if you have a specific reason to."),
      selectInput("k1k2", "K1 / K2 formulation", choices = c(
        "Millero (2010) [default]"     = "m10",
        "Lueker et al. (2000)"         = "l",
        "Cai & Wang (1998)"            = "cw",
        "Millero et al. (2002)"        = "m02",
        "Millero et al. (2006)"        = "m06",
        "Mojica Prieto et al. (2002)"  = "mp2",
        "Papadimitriou et al. (2018)"  = "p18",
        "Roy et al. (1993)"            = "r",
        "Shockman & Byrne (2021)"      = "sb21",
        "Sulpis et al. (2020)"         = "s20",
        "Waters et al. (2014)"         = "w14"
      ), selected = "m10"),
      selectInput("kf", "KF formulation", choices = c(
        "Perez & Fraga (1987) [default]" = "pf",
        "Dickson & Riley (1979)"          = "dg"
      ), selected = "pf"),
      selectInput("ks", "KS formulation", choices = c(
        "Dickson (1990) [default]" = "d",
        "Khoo et al. (1977)"       = "k"
      ), selected = "d"),
      selectInput("b_const", "Total boron formulation", choices = c(
        "Uppstrom (1974) [default]" = "u74",
        "Lee et al. (2010)"          = "l10"
      ), selected = "u74"),
      selectInput("pHscale", "pH scale", choices = c(
        "Total scale [default]" = "T",
        "Free scale"             = "F",
        "Seawater scale"         = "SWS"
      ), selected = "T"),
      selectInput("eos", "Equation of state", choices = c(
        "EOS-80 [default]" = "eos80",
        "TEOS-10"           = "teos10"
      ), selected = "eos80"),
      
      hr(),
      h4("4. QC thresholds"),
      helpText("Cutoffs used to flag overall run performance (1 = good, 2 = caution, 3 = flagged). Defaults match current practice."),
      numericInput("sd_thresh_moderate", "CRM SD \u2014 caution threshold", value = 10),
      numericInput("sd_thresh_severe",   "CRM SD \u2014 flag threshold",    value = 15),
      numericInput("corfac_lower", "CRM correction factor \u2014 lower bound", value = 0.97),
      numericInput("corfac_upper", "CRM correction factor \u2014 upper bound", value = 1.03),
      numericInput("r2_dev_moderate", "Standard fit R\u00b2 deviation \u2014 caution threshold", value = 0.01),
      numericInput("r2_dev_severe",   "Standard fit R\u00b2 deviation \u2014 flag threshold",    value = 0.05),
      
      hr(),
      actionButton("run_pipeline", "Process run", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 7,
      tabsetPanel(
        id = "tabs",
        
        tabPanel("About this tool",
                 br(),
                 h4("What this app does"),
                 p("This tool replicates the Burke-o-Lator discrete-sample processing pipeline developed by Dr. Wiley Evans: it imports raw pCO2/TCO2 data, corrects for gas and liquid standard drift, corrects for CRM performance, calculates the full carbonate system (TA, pH, \u03a9-aragonite, etc.), and applies basic QC flagging based on the run performance."),
                 p("Fill in the run info and upload your files on the left, adjust any decision points you want to change, then click ", strong("Process run"), "."),
                 h4("Before Using this Tool..."),
                 p("Before starting, you must copy and paste the raw discrete sample data (from the _DescSmpl_ files the BoL exports) into a .csv and make sure the following conditions are met:"),
                 tags$ol(
                   tags$li(strong("Choose standards:"), " if you ran extra gas and liquid standards before, during, or after the run, get rid of the extras. Only include the standards you will use to correct the run. There should be one set of standards before, during, and after the run."),
                   tags$li(strong("Check sample names:"), " sample names MUST exactly match those in the sample inventory."),
                   tags$li(strong("Salinity check:"), " all salinities in the Smpl_S column of the raw data are correct."),
                   tags$li(strong("No CRM confusion:"), " CRMs at the beginning and end of the run for calibration must have `CRM` in their sample name. This tool can handle any number of pre- or post-sample CRMs, though it will only use the closest three within each pre- and post-run group for processing. However, mid-run CRMs that are being run as samples, rather than for run calibration, should have `crm` in the title instead of 'CRM' or should be processed via older methods."),
                 ),
                 h4("Steps This Tool Does for You"),
                 tags$ol(
                   tags$li(strong("Import & classify:"), " reads the discrete sample file, tags each row as a sample, gas standard, liquid standard, or CRM, and finds the matching standard calibration fit files."),
                   tags$li(strong("Standard & drift correction:"), " fits gas and liquid standard calibration curves and interpolates drift across the run."),
                   tags$li(strong("CRM correction:"), " compares CRM measurements to certified values and applies a drift-corrected correction factor, plus a headspace correction."),
                   tags$li(strong("Carbonate chemistry:"), " calculates TA, in-situ pH/pCO2/\u03a9, pCO2 at 20\u00b0C, and the Revelle factor using seacarb."),
                   tags$li(strong("Finalize & QC:"), " flags overall run performance and formats output tables for export.")
                 )
        ),
        
        tabPanel("CRM performance",
                 br(),
                 p("CRM measurements vs. certified values for this run, including the SD of all replicates, the `true` expected pCO2, and the correction factor. Formatted so that the exported file is ready for copying and pasting by the current user group. Note that in the event of >3 pre- or post-run CRMs, the sample processing only uses the three closest in each group."),
                 DTOutput("crm_table")
        ),
        
        tabPanel("Processed samples",
                 br(),
                 p("Final processed sample results, formatted for the current user group. Run_Performance/Acceptance Rating: 1 = good, 2 = caution, 3 = flagged for review."),
                 DTOutput("samples_table")
        ),
        
        tabPanel("Save outputs",
                 br(),
                 p("Choose a folder and save the processed output files directly to it. "),
                 shinyDirButton("output_dir", "Browse for output folder\u2026", "Select the folder to save exports into."),
                 verbatimTextOutput("output_dir_display"),
                 br(),
                 actionButton("save_outputs", "Save all outputs to this folder.", class = "btn-primary"),
                 br(), br(),
                 verbatimTextOutput("save_status")
        )
      )
    )
  )
)

# --- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  shinyDirChoose(input, "fitfiles_dir", roots = volumes, session = session)
  
  fitfiles_dir_path <- reactive({
    if (is.integer(input$fitfiles_dir)) return(NULL)  # nothing selected yet
    parseDirPath(volumes, input$fitfiles_dir)
  })
  
  output$fitfiles_dir_display <- renderText({
    path <- fitfiles_dir_path()
    if (is.null(path)) "No folder selected yet" else paste("Selected:", path)
  })
  
  pipeline_result <- eventReactive(input$run_pipeline, {
    
    validate(
      need(input$bol_data, "Please upload the discrete sample file."),
      need(fitfiles_dir_path(), "Please select the folder containing the Gstd/Lstd fit files."),
      need(input$crm_batch_file, "Please upload the CRM batch info file."),
      need(input$inventory_file, "Please upload the sample inventory file."),
      need(nchar(input$analyst) > 0, "Please enter the analyst's name.")
    )
    
    withProgress(message = "Processing run", value = 0, {
      
      result <- tryCatch({
        
        incProgress(0.1, detail = "Step 1/5: reading & classifying data")
        raw_dir <- paste0(fitfiles_dir_path(), "/")
        f1_out <- f1(run_number = input$run_number,
                     run_date = as.character(input$run_date),
                     analyst = input$analyst,
                     lab_temp = input$lab_temp,
                     roomair_co2 = input$roomair_co2,
                     bol_data = input$bol_data$datapath,
                     data_filepath = raw_dir,
                     crm_batch = input$crm_batch,
                     crm_batch_info = input$crm_batch_file$datapath,
                     sample_inventory = input$inventory_file$datapath)
        
        incProgress(0.2, detail = "Step 2/5: standard & drift correction")
        f2_out <- f2(f1_out)
        
        incProgress(0.2, detail = "Step 3/5: CRM correction")
        f3_out <- f3(f2_out)
        
        incProgress(0.2, detail = "Step 4/5: carbonate chemistry")
        f4_out <- f4(f3_out,
                     k1k2 = input$k1k2, kf = input$kf, ks = input$ks,
                     b = input$b_const, pHscale = input$pHscale, eos = input$eos)
        
        incProgress(0.2, detail = "Step 5/5: Run QC flagging & formatting")
        f5_out <- f5(f4_out,
                     sd_thresh_severe = input$sd_thresh_severe,
                     sd_thresh_moderate = input$sd_thresh_moderate,
                     corfac_upper = input$corfac_upper,
                     corfac_lower = input$corfac_lower,
                     r2_dev_severe = input$r2_dev_severe,
                     r2_dev_moderate = input$r2_dev_moderate,
                     k1k2 = input$k1k2, kf = input$kf, ks = input$ks,
                     b = input$b_const, pHscale = input$pHscale, eos = input$eos)
        
        f5_out
        
      }, error = function(e) {
        showNotification(paste("Processing failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      })
      
      result
    })
  })
  
  observeEvent(pipeline_result(), {
    req(pipeline_result())
    updateTabsetPanel(session, "tabs", selected = "Processed samples")
  })
  
  output$samples_table <- renderDT({
    req(pipeline_result())
    datatable(pipeline_result()$formatted_samples, options = list(scrollX = TRUE))
  })
  
  output$crm_table <- renderDT({
    req(pipeline_result())
    datatable(pipeline_result()$crm_performance, options = list(scrollX = TRUE))
  })
  
  shinyDirChoose(input, "output_dir", roots = output_volumes, session = session)
  
  output_dir_path <- reactive({
    if (is.integer(input$output_dir)) return(NULL)  # nothing selected yet
    parseDirPath(output_volumes, input$output_dir)
  })
  
  output$output_dir_display <- renderText({
    path <- output_dir_path()
    if (is.null(path)) "No folder selected yet" else paste("Will save to:", path)
  })
  
  # Holds the file paths queued for writing between the initial "Save" click
  # and (if needed) confirmation of an overwrite, since those are two
  # separate button-click events.
  pending_save <- reactiveVal(NULL)
  
  do_save <- function(file_paths) {
    write_csv(file_paths$res$formatted_samples, file_paths$samples)
    write_csv(file_paths$res$crm_performance,   file_paths$crm)
    write_csv(file_paths$res$run_metadata,      file_paths$metadata)
    write_csv(file_paths$res$all_columns,       file_paths$all_cols)
    output$save_status <- renderText(paste0("Saved 4 files to ", file_paths$out_dir,
                                            " at ", format(Sys.time(), "%H:%M:%S")))
    showNotification("Outputs saved.", type = "message")
  }
  
  observeEvent(input$save_outputs, {
    res <- pipeline_result()
    if (is.null(res)) {
      output$save_status <- renderText("Nothing to save yet \u2014 process a run first.")
      return()
    }
    out_dir <- output_dir_path()
    if (is.null(out_dir)) {
      output$save_status <- renderText("Please select an output folder first.")
      return()
    }
    
    file_paths <- list(
      res = res, out_dir = out_dir,
      samples  = file.path(out_dir, paste0(res$run_id, "_formatted_proc_discrete_samples.csv")),
      crm      = file.path(out_dir, paste0(res$run_id, "_crm_performance_dataframe.csv")),
      metadata = file.path(out_dir, paste0(res$run_id, "_run_metadata.csv")),
      all_cols = file.path(out_dir, paste0(res$run_id, "_ref_discrete_ALLcolumns.csv"))
    )
    
    existing <- Filter(file.exists, c(file_paths$samples, file_paths$crm,
                                      file_paths$metadata, file_paths$all_cols))
    
    if (length(existing) > 0) {
      pending_save(file_paths)
      showModal(modalDialog(
        title = "Files already exist",
        tagList(
          p("The following file(s) already exist in this folder and will be overwritten:"),
          tags$ul(lapply(basename(existing), tags$li))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_overwrite", "Overwrite", class = "btn-danger")
        )
      ))
    } else {
      do_save(file_paths)
    }
  })
  
  observeEvent(input$confirm_overwrite, {
    removeModal()
    fp <- pending_save()
    req(fp)
    do_save(fp)
    pending_save(NULL)
  })
}


shinyApp(ui, server)