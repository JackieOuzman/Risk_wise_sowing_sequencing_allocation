# ===============================================================================
# SOWING SCHEDULE PLANNER — SHINY APP (skeleton)
# ===============================================================================
library(shiny)

source("solver_logic.R")
source("report_logic.R")

# ===============================================================================
# UI
# ===============================================================================
ui <- navbarPage(
  title = "Sowing Schedule Planner",
  
  # --- TAB 1: SETUP -----------------------------------------------------------
  tabPanel(
    "Setup",
    h2("Setup"),
    fluidRow(
      column(3, textInput("site_name", "Site name", value = "Lock, Eyre Peninsula")),
      column(3, textInput("simulation_name", "Simulation name", value = "baseline_v1")),
      column(3, numericInput("cropping_area_ha", "Cropping area (ha)", value = 1000, min = 0)),
      column(3, selectInput("optimise_for", "Optimise for",
                            choices = c("Yield" = "yield", "Gross Margin" = "gm"),
                            selected = "yield"))
    ),
    
    fluidRow(
      column(8, numericInput("daily_capacity_ha", "Daily sowing capacity (ha/day)", value = 15, min = 0)),
      column(4, style = "margin-top: 25px;",
             tags$a("ℹ More info", href = "capacity_guidance.pdf", target = "_blank",
                    class = "btn btn-default btn-sm"))
    ),
    
    fluidRow(
      column(8, h3("Frost zone split (%)")),
      column(4, style = "margin-top: 15px;",
             tags$a("ℹ More info", href = "zone_guidance.pdf", target = "_blank",
                    class = "btn btn-default btn-sm"))
    ),
    
    fluidRow(
      column(4, numericInput("zone_green_pct", "Green %", value = 60, min = 0, max = 100)),
      column(4, numericInput("zone_amber_pct", "Amber %", value = 20, min = 0, max = 100)),
      column(4, numericInput("zone_red_pct", "Red %", value = 20, min = 0, max = 100))
    ),
    textOutput("zone_pct_check"),
    
    h3("Crop targets (ha)"),
    fluidRow(
      column(4, numericInput("wheat_ha", "Wheat", value = 400, min = 0)),
      column(4, numericInput("barley_ha", "Barley", value = 400, min = 0)),
      column(4, numericInput("canola_ha", "Canola", value = 0, min = 0))
    ),
    
    h4("Legumes (choose up to 2)"),
    fluidRow(
      column(6, selectInput("legume1_crop", "Legume 1",
                            choices = c("Lentils", "Beans", "Lupins", "Peas"),
                            selected = "Beans")),
      column(6, numericInput("legume1_ha", "Legume 1 area (ha)", value = 100, min = 0))
    ),
    fluidRow(
      column(6, uiOutput("legume2_crop_ui")),
      column(6, numericInput("legume2_ha", "Legume 2 area (ha)", value = 100, min = 0))
    ),
    
    textOutput("crop_total_check"),
    
    uiOutput("red_zone_crop_ui"),
    
    h3("Program start date"),
    dateInput("program_start_date", "Start date",
              value = "2026-04-08",
              min = "2026-04-08", max = "2026-07-17"),
    textOutput("feasibility_check"),
    
   
    
    h3("Economics"),
    fluidRow(
      column(12, selectInput("price_scenario", "Grain price scenario",
                             choices = c("Low", "Average", "High"),
                             selected = "Average"))
    ),
    
    h3("Files"),
    selectInput("yield_file_choice", "Yield input file",
                choices = c("Baseline" = "EP_yld_long_format.xlsx",
                            "Mock/Test scenario" = "EP_yld_long_format_MOCK.xlsx",
                            "Upload my own file..." = "custom")),
    conditionalPanel(
      condition = "input.yield_file_choice == 'custom'",
      fileInput("yield_file_upload", "Upload yield file (.xlsx)", accept = ".xlsx")
    ),
    textOutput("yield_file_display"),
    
    verbatimTextOutput("setup_check"),
    
    hr(),
    actionButton("run_btn", "Run simulation", class = "btn-primary btn-lg"),
    verbatimTextOutput("run_check")
  ),
  
  # --- TAB 2: REPORT -----------------------------------------------------------
  tabPanel(
    "Report",
    h2("Report"),
    downloadButton("download_report", "Download report + data (.zip)"),
    br(), br(),
    verbatimTextOutput("run_description_display"),
    plotOutput("report_plot", height = "900px")
  ),
  
  # --- TAB 3: COMPARE ----------------------------------------------------------
  tabPanel(
    "Compare",
    h2("Compare"),
    fileInput("compare_uploads", "Upload simulation bundle(s) (.zip)",
              multiple = TRUE, accept = ".zip"),
    radioButtons("compare_metric", "Compare by",
                choices = c("Expected yield" = "expected_yield_t",
                           "Expected GM" = "expected_gm_dollars"),
                selected = "expected_yield_t", inline = TRUE),
    plotOutput("compare_plot"),
    tableOutput("compare_table"),
    h3("Run descriptions"),
    verbatimTextOutput("compare_descriptions")
  )
)
# ===============================================================================
# SERVER
# ===============================================================================
server <- function(input, output, session) {
  
  model_result <- reactiveVal(NULL)
  
  output$zone_pct_check <- renderText({
    total_pct <- input$zone_green_pct + input$zone_amber_pct + input$zone_red_pct
    if (total_pct == 100) {
      paste0("✓ Zones sum to 100%")
    } else {
      paste0("✗ Zones sum to ", total_pct, "% — must equal 100%")
    }
  })
  
  output$legume2_crop_ui <- renderUI({
    remaining_choices <- setdiff(c("Lentils", "Beans", "Lupins", "Peas"), input$legume1_crop)
    selectInput("legume2_crop", "Legume 2", choices = remaining_choices)
  })
  output$crop_total_check <- renderText({
    total_crop_ha <- input$wheat_ha + input$barley_ha + input$canola_ha +
      input$legume1_ha + input$legume2_ha
    if (total_crop_ha == input$cropping_area_ha) {
      paste0("✓ Crop targets sum to ", total_crop_ha, " ha, matching cropping area")
    } else if (total_crop_ha < input$cropping_area_ha) {
      paste0("Crop targets sum to ", total_crop_ha, " ha — ",
             input$cropping_area_ha - total_crop_ha, " ha of the farm left unallocated")
    } else {
      paste0("✗ Crop targets sum to ", total_crop_ha, " ha — exceeds cropping area (",
             input$cropping_area_ha, " ha) by ", total_crop_ha - input$cropping_area_ha, " ha")
    }
  })
  
  
  output$red_zone_crop_ui <- renderUI({
    all_crops <- c(Wheat = input$wheat_ha, Barley = input$barley_ha, Canola = input$canola_ha)
    all_crops[input$legume1_crop] <- input$legume1_ha
    all_crops[input$legume2_crop] <- input$legume2_ha
    
    active_crops <- names(all_crops[all_crops > 0])
    
    selectInput("red_zone_excluded_crop", "Crop excluded from Red zone",
                choices = c("None", active_crops))
  })
  
  output$yield_file_display <- renderText({
    if (input$yield_file_choice == "custom") {
      req(input$yield_file_upload)
      paste0("Using uploaded file: ", input$yield_file_upload$name)
    } else {
      paste0("Using: ", input$yield_file_choice)
    }
  })
  
  output$output_folder_display <- renderText({
    sim_folder <- file.path(input$output_base_folder, input$simulation_name)
    paste0("Outputs will be saved to: ", sim_folder)
  })
  
  output$feasibility_check <- renderText({
    window_date <- as.Date(c(
      "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
      "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
      "2026-07-17"
    ))
    window_days <- rep(10, 11)
    
    active_days <- window_days[window_date >= input$program_start_date]
    total_capacity <- sum(input$daily_capacity_ha * active_days)
    
    total_target <- input$wheat_ha + input$barley_ha + input$canola_ha +
      input$legume1_ha + input$legume2_ha
    
    if (total_capacity >= total_target) {
      paste0("✓ Feasible — ", total_capacity, " ha of sowing capacity available for ",
             total_target, " ha of crop targets")
    } else {
      paste0("✗ Not feasible — only ", total_capacity, " ha of sowing capacity available, but ",
             total_target, " ha of crop targets requested (shortfall of ",
             total_target - total_capacity, " ha). Try an earlier start date or lower targets.")
    }
  })
  
  
  output$setup_check <- renderText({
    req(input$red_zone_excluded_crop)
    red_zone_display <- if (input$red_zone_excluded_crop == "None") {
      "None (all crops allowed in Red zone)"
    } else {
      input$red_zone_excluded_crop
    }
    
    paste0("Site: ", input$site_name,
           " | Cropping area: ", input$cropping_area_ha, " ha",
           " | Daily capacity: ", input$daily_capacity_ha, " ha/day",
           " | Zones: ", input$zone_green_pct, "/", input$zone_amber_pct, "/", input$zone_red_pct, "%",
           " | Wheat: ", input$wheat_ha, " | Barley: ", input$barley_ha, " | Canola: ", input$canola_ha,
           " | ", input$legume1_crop, ": ", input$legume1_ha,
           " | ", input$legume2_crop, ": ", input$legume2_ha,
           " | Red-zone excluded crop: ", red_zone_display)
  })
  
  
  output$report_plot <- renderPlot({
    req(model_result())
    res <- model_result()
    req(res$status == "success")
    print(build_sowing_report(res))
  }, width = 1400, height = 900)
 
# ===============================================================================
# RUN THE APP
# ===============================================================================
  observeEvent(input$run_btn, {
    
    updateActionButton(session, "run_btn", label = "Running...")
  
    yield_file_path <- if (input$yield_file_choice == "custom") {
      req(input$yield_file_upload)
      input$yield_file_upload$datapath
    } else {
      input$yield_file_choice
    }
  
  red_zone_final <- if (input$red_zone_excluded_crop == "None") {
    NA
  } else {
    input$red_zone_excluded_crop
  }
  
  params <- list(
    site_name = input$site_name,
    simulation_name = input$simulation_name,
    cropping_area_ha = input$cropping_area_ha,
    daily_capacity_ha = input$daily_capacity_ha,
    zone_pct = c(Green = input$zone_green_pct / 100,
                 Amber = input$zone_amber_pct / 100,
                 Red = input$zone_red_pct / 100),
    crop_targets = c(Wheat = input$wheat_ha, Barley = input$barley_ha, Canola = input$canola_ha),
    legume_targets = setNames(c(input$legume1_ha, input$legume2_ha),
                              c(input$legume1_crop, input$legume2_crop)),
    red_zone_excluded_crop = red_zone_final,
    program_start_date = input$program_start_date,
    price_scenario = input$price_scenario,
    optimise_for = input$optimise_for,
    yield_file_path = yield_file_path,
    output_folder = file.path(tempdir(), input$simulation_name)
  )
  
  ##############################################################################
  ### Setup tab progressing
  ##############################################################################
  
  withProgress(message = "Running simulation...", value = 0, {
    
    deciles_total <- 3
    
    result <- run_sowing_model(params, progress_callback = function(decile_done) {
      incProgress(1 / deciles_total, detail = paste(decile_done, "complete"))
    })
    
    model_result(result)
    
    if (result$status == "success") {
      report_plot <- build_sowing_report(result)
      ggsave(file.path(result$output_folder, paste0(params$simulation_name, "_report.png")),
             report_plot, width = 16, height = 9, dpi = 150)
    }
    
    output$run_check <- renderText({
      if (result$status == "success") {
        paste0("✓ Simulation complete: ", params$simulation_name, "\n",
               paste(capture.output(print(result$run_summary[, c("decile", "expected_yield_t", "expected_gm_dollars")])), collapse = "\n"),
               "\n\nFiles saved to: ", result$output_folder)
      } else {
        paste0("✗ ", result$message)
      }
    })
    
  })
  
  updateActionButton(session, "run_btn", label = "Run simulation")
  
  })
  
  ##############################################################################
  ### Report tab 
  ##############################################################################
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(model_result()$run_summary$simulation_name[1], "_report_bundle.zip")
    },
    content = function(file) {
      res <- model_result()
      req(res$status == "success")
      
      files_to_zip <- list.files(res$output_folder, full.names = TRUE)
      
      zip::zip(zipfile = file, files = basename(files_to_zip), root = res$output_folder)
    }
  )
  
  output$run_description_display <- renderText({
    req(model_result())
    res <- model_result()
    req(res$status == "success")
    res$run_description
  })
  
  ##############################################################################
  ### Compare tab 
  ##############################################################################
  output$compare_table <- renderTable({
    req(input$compare_uploads)
    
    all_summaries <- lapply(1:nrow(input$compare_uploads), function(i) {
      zip_path <- input$compare_uploads$datapath[i]
      extract_folder <- tempfile(pattern = "compare_")
      dir.create(extract_folder)
      
      unzip(zip_path, exdir = extract_folder)
      
      summary_file <- list.files(extract_folder, pattern = "_run_summary\\.csv$", full.names = TRUE)
      req(length(summary_file) == 1)
      
      read.csv(summary_file, stringsAsFactors = FALSE)
    })
    
    combined <- bind_rows(all_summaries)[, c("simulation_name", "decile", "optimise_for",
                                             "price_scenario", "expected_yield_t", "expected_gm_dollars")]
    combined$expected_yield_t <- format(round(combined$expected_yield_t), big.mark = ",")
    combined$expected_gm_dollars <- paste0("$", format(round(combined$expected_gm_dollars), big.mark = ","))
    combined
  })
  
  output$compare_plot <- renderPlot({
    req(input$compare_uploads)
    
    all_summaries <- lapply(1:nrow(input$compare_uploads), function(i) {
      zip_path <- input$compare_uploads$datapath[i]
      extract_folder <- tempfile(pattern = "compare_")
      dir.create(extract_folder)
      unzip(zip_path, exdir = extract_folder)
      summary_file <- list.files(extract_folder, pattern = "_run_summary\\.csv$", full.names = TRUE)
      req(length(summary_file) == 1)
      read.csv(summary_file, stringsAsFactors = FALSE)
    })
    
    combined <- bind_rows(all_summaries)
    combined$decile <- factor(combined$decile, levels = c("D1-3", "D4-6", "D7-9"))
    
    y_label <- if (input$compare_metric == "expected_gm_dollars") "Expected GM ($)" else "Expected yield (t)"
    
    ggplot(combined, aes(x = decile, y = .data[[input$compare_metric]], fill = simulation_name)) +
      geom_col(position = "dodge") +
      labs(title = paste(y_label, "by decile and simulation"), x = "Decile", y = y_label, fill = "Simulation") +
      theme_minimal(base_size = 12)
  })
  
  output$compare_descriptions <- renderText({
    req(input$compare_uploads)
    
    all_descriptions <- lapply(1:nrow(input$compare_uploads), function(i) {
      zip_path <- input$compare_uploads$datapath[i]
      extract_folder <- tempfile(pattern = "compare_")
      dir.create(extract_folder)
      unzip(zip_path, exdir = extract_folder)
      desc_file <- list.files(extract_folder, pattern = "_run_description\\.txt$", full.names = TRUE)
      req(length(desc_file) == 1)
      paste(readLines(desc_file), collapse = "\n")
    })
    
    paste(all_descriptions, collapse = "\n\n=====================================\n\n")
  })
  
  
  
}

shinyApp(ui = ui, server = server)