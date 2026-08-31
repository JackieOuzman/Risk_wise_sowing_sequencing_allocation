# ===============================================================================
# SOWING SCHEDULE PLANNER — SHINY APP (skeleton)
# ===============================================================================
library(shiny)

# ===============================================================================
# UI
# ===============================================================================
ui <- navbarPage(
  title = "Sowing Schedule Planner",
  
  # --- TAB 1: SETUP -----------------------------------------------------------
  tabPanel(
    "Setup",
    h2("Setup"),
    textInput("site_name", "Site name", value = "Lock, Eyre Peninsula"),
    textInput("simulation_name", "Simulation name", value = "baseline_v1"),
    selectInput("optimise_for", "Optimise for",
                                         choices = c("Yield" = "yield", "Gross Margin" = "gm"),
                                         selected = "yield"),
    numericInput("cropping_area_ha", "Cropping area (ha)", value = 1000, min = 0),
    fluidRow(
      column(8, numericInput("daily_capacity_ha", "Daily sowing capacity (ha/day)", value = 15, min = 0)),
      column(4, style = "margin-top: 25px;",
             actionButton("capacity_help_btn", "ℹ More info", class = "btn-sm"))
    ),
    fluidRow(column(12,
                    conditionalPanel(
                      condition = "input.capacity_help_btn % 2 == 1",
                      wellPanel(
                        p("PLACEHOLDER: guidance on how to calculate daily sowing capacity will go here.")
                      )
                    )
    )),
    fluidRow(
      column(8, h3("Frost zone split (%)")),
      column(4, style = "margin-top: 15px;",
             actionButton("zone_help_btn", "ℹ More info", class = "btn-sm"))
    ),
    fluidRow(column(12,
                    conditionalPanel(
                      condition = "input.zone_help_btn % 2 == 1",
                      wellPanel(
                        p("PLACEHOLDER: guidance on how to determine your frost zone split will go here.")
                      )
                    )
    )),
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
    radioButtons("yield_file_choice", "Yield input file",
                 choices = c("Use default" = "default", "Choose a different file" = "custom"),
                 selected = "default"),
    conditionalPanel(
      condition = "input.yield_file_choice == 'custom'",
      textInput("yield_file_custom", "Path to yield file",
                value = "", placeholder = "e.g. C:/path/to/your_yield_file.xlsx")
    ),
    textOutput("yield_file_display"),
    
    textInput("output_base_folder", "Save outputs to folder",
              value = "C:/Users/ouz001/working_from_home_post_Sep2022/Risk_wise_sowing_sequencing_allocation/output"),
    
  
    textOutput("output_folder_display"),
    
    verbatimTextOutput("setup_check"),
    
    hr(),
    actionButton("run_btn", "Run simulation", class = "btn-primary btn-lg"),
    verbatimTextOutput("run_check")
  ),
  
  # --- TAB 2: REPORT -----------------------------------------------------------
  tabPanel(
    "Report",
    h2("Report"),
    p("The sowing program report and yield histograms will go here.")
  ),
  
  # --- TAB 3: COMPARE ----------------------------------------------------------
  tabPanel(
    "Compare",
    h2("Compare"),
    p("Loading and comparing past simulations will go here.")
  )
)

# ===============================================================================
# SERVER
# ===============================================================================
server <- function(input, output, session) {
  
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
    file_path <- if (input$yield_file_choice == "default") {
      "../files/EP_yld_long_format.xlsx" # I need to upadte this later when the network is going
    } else {
      input$yield_file_custom
    }
    paste0("Using: ", file_path)
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

}
# ===============================================================================
# RUN THE APP
# ===============================================================================
observeEvent(input$run_btn, {
  
  yield_file_path <- if (input$yield_file_choice == "default") {
    "../files/EP_yld_long_format.xlsx"
  } else {
    input$yield_file_custom
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
    output_folder = file.path(input$output_base_folder, input$simulation_name)
  )
  
  output$run_check <- renderText({
    paste(capture.output(str(params)), collapse = "\n")
  })
  
})


shinyApp(ui = ui, server = server)