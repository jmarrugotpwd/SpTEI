
# MULTI-SPECIES SPTEI HOTSPOT SHINY APP

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(shinyBS)
library(rsconnect)

app_dir <- "E:/Species Data Assessment/SpTEI_ShinyApp/data"
source(file.path(app_dir, "directions.R"), local = TRUE)

precomputed <- readRDS(file.path(app_dir, "precomputed_app_data.rds"))

species_data       <- precomputed$species_data
COA_EMS            <- precomputed$COA_EMS
hotspot_points     <- precomputed$hotspot_points
metadata_species   <- precomputed$species_metadata
species_lookup     <- precomputed$species_lookup
species_choices    <- precomputed$species_choices
available_species  <- precomputed$available_species
obs                <- precomputed$obs
huc_county_joined  <- precomputed$huc_county_joined
huc12_poly         <- precomputed$huc12_poly

# USER INTERFACE
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    # --- COLOR PALETTE ---
    primary = "#2E6F40",      # Conservation Green
    secondary = "#4F7942",    # Forest Green
    success = "#6CA96B",      # Light Green
    info = "#8FA98F",         # Sage
    warning = "#C5B358",      # Prairie Gold
    danger = "#8B3A3A",       # Soil Red
    # --- NEUTRALS ---
    light = "#F5F5F2",        # Field Notebook Background
    dark = "#2F3E46",         # Slate Rock
    # --- FONTS ---
    base_font = bslib::font_google("Noto Sans"),
    heading_font = bslib::font_google("Merriweather"),
    # --- Optional Bootswatch base (minimal) ---
    bootswatch = "flatly"
  ),
  tags$style(HTML("
  /* --- PAGE BACKGROUND --- */
  body {
    background-color: #F5F5F2;
    color: #2F3E46;
  }
  /* --- HEADERS --- */
  h3, h4 {
    font-family: 'Merriweather', serif;
    color: #2E6F40;
    border-left: 5px solid #4F7942;
    padding-left: 10px;
    margin-top: 25px;
    margin-bottom: 15px;
  }
  /* --- TITLE PANEL --- */
  .title-panel {
    font-family: 'Merriweather', serif;
    font-weight: 700;
    padding-top: 20px;      
    padding-bottom: 15px;   
    color: #1B5E20;         
  }
  /* --- SIDEBAR PANEL --- */
  .well {
    background-color: #F0F0EB !important;
    border: 1px solid #D6D5CD !important;
    border-radius: 8px;
    padding: 15px !important;
  }
  .sidebar-section {
    margin-bottom: 25px;
  }
  /* --- BUTTONS --- */
  .btn-primary {
    background-color: #2E6F40 !important;
    border-color: #2E6F40 !important;
    font-weight: 600;
  }
  .btn-secondary {
    background-color: #E6E8E3 !important;
    border-color: #D0D6CD !important;
    color: #2F3E46 !important;
    font-weight: 500;
  }
  .btn:hover {opacity: 0.9;}
  .btn {
    width: 100% !important;        /* makes buttons full-width */
    padding: 10px 16px !important; /* increases height and comfort */
    font-size: 15px !important;    /* slightly larger text */
    border-radius: 6px !important; /* smooth corners */
  }
  /* --- SELECTIZE INPUT --- */
  .selectize-control {margin-bottom: 15px;}
  /* --- TABLE HEADERS --- */
  table th {
    background-color: #4F7942 !important;
    color: #FFF !important;
  }
  /* --- MAP SECTION --- */
  .map-section {
    background-color: #FFFFFF;
    padding: 20px;
    border: 1px solid #D6D5CD;
    border-radius: 10px;
    margin-top: 15px;
  }
  /* --- MAP RADIO BUTTONS --- */
  .map-options .radio-inline {margin-right: 20px;}
  /* --- SPECIES LIST --- */
  #selected_species_display ul {
    background-color: #f0f4f2;
    padding: 10px 15px;
    border: 1px solid #D6D5CD;
    border-radius: 8px;
  }
  /* --- TABS --- */
  .nav-tabs > li > a {font-weight: 600;}
")),
  titlePanel(div(class = "title-panel", "Multi-SGCN Assessment Tool")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-section",
          h4("Species Filters"),
          selectInput("taxa_group", "Taxa Group:",
                      choices = c("All", sort(unique(na.omit(metadata_species$Taxa_Group))))),
          selectInput("game_status", "Game Status:",
                      choices = c("All", sort(unique(na.omit(metadata_species$Game_Status))))),
          selectInput("habitat_clade", "Habitat Clade:",
                      choices = c("All", sort(unique(na.omit(metadata_species$Habitat_Clade)))))
      ),
      hr(),
      div(class="sidebar-section",
          h4("Species Selection"),
          fluidRow(
            column(6, actionButton("select_all_species", "Select All", icon = icon("check"))),
            column(6, actionButton("clear_all_species", "Clear All", icon = icon("times")))
          ),
          br(),
          selectizeInput(
            "species_picker", "Select Species:",
            choices = names(species_choices),
            multiple = TRUE,
            options = list(
              placeholder = "Search species...",
              plugins = list("restore_on_backspace", "remove_button")
            )
          ),
          hr(),
          fluidRow(
            column(6, actionButton("run_assessment", "Run", icon = icon("play"),
                                   class="btn btn-primary btn-block")),
            column(6, actionButton("reset_assessment", "Reset", icon = icon("refresh"),
                                   class="btn btn-secondary btn-block"))
          ),
          helpText("Select one or more species to assess.")
      ),
      hr(),
      div(class="sidebar-section", strong("Selected species:"), uiOutput("selected_species_display")),
      hr(),
      div(class="sidebar-section",
          h4("Export Results"),
          downloadButton("download_results", "Download CSV")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Hotspot Map",
                 div(class="map-section",
                     leafletOutput("hotspot_map", height = "600px"),
                     hr(),
                     h3("Map Display Options"),
                     radioButtons(
                       "map_score_checkbox", "Map Display Metric:",
                       choices = c(
                         "Habitat Score" = "Normalized_Habitat_Score",
                         "Observation Score" = "Normalized_Observation_Score",
                         "Species Count" = "Species_count"
                       ),
                       inline = TRUE
                     ),
                     hr(),
                     h3("Map Filters"),
                     
                     div(style = "width: 100%;",
                         tags$style(HTML("
                    #threshold .irs-grid-pol,
                    #threshold .irs-grid-text,
                    #acreage_threshold .irs-grid-pol,
                    #acreage_threshold .irs-grid-text {display: block !important;}
                  ")),
                         div(style = "display: flex; gap: 20px;",
                             # Threshold
                             div(style="flex:1;",
                                 sliderInput("threshold",
                                             label = tags$span(style="font-size:20px; font-weight:bold;",
                                                               "Current Map Display Score Threshold"),
                                             min = 0, max = 1, value = 0, step = 0.10,
                                             width = "100%", ticks = TRUE)
                             ),
                             # Acreage slider + numeric input
                             div(style="flex:1;",
                                 sliderInput("acreage_threshold",
                                             label = tags$span(style="font-size:20px; font-weight:bold;",
                                                               "Acreage Threshold"),
                                             min = 0, max = max(COA_EMS$Acres, na.rm = TRUE),
                                             value = 0, step = 1, width = "100%", ticks = TRUE),
                                 div(style="margin-top:-5px;",
                                     numericInput("acreage_threshold_box",
                                                  label = tags$span(style="font-size:16px;", "Enter Acreage Value"),
                                                  value = 0, min = 0, step = 1, width="100%"))
                             ),
                             # County filter
                             div(style="flex:1;",
                                 selectizeInput("county_filter",
                                                label = tags$span(style="font-size:20px; font-weight:bold;",
                                                                  "Filter by County"),
                                                choices = NULL,
                                                multiple = TRUE,
                                                options = list(
                                                  placeholder = "Select counties",
                                                  plugins = list("remove_button")
                                                )
                                 )
                             )
                         )
                     ),
                     hr(),
                     h3("Species Data at Selected Location"),
                     uiOutput("selected_location_summary"),
                     tags$details(
                       tags$summary("Click to view score definitions"),
                       tags$div(
                         style="margin-left:15px; font-size:14px; color:#444;",
                         tags$b("Habitat Score (0–5):"),
                         tags$ul(
                           tags$li("0 – Never utilized by species or detrimental"),
                           tags$li("1 – Very infrequently used, traverse"),
                           tags$li("2 – Occasionally used"),
                           tags$li("3 – Unexceptional potential habitat"),
                           tags$li("4 – Decent potential habitat that can support at least one life stage"),
                           tags$li("5 – Best potential habitat that supports most or all life stages")
                         ),
                         tags$b("Observation Score (1–3):"),
                         tags$ul(
                           tags$li("1 – None or poor‑quality observations"),
                           tags$li("2 – Mid‑grade observations"),
                           tags$li("3 – High‑quality observations")
                         )
                       )
                     ),
                     DTOutput("species_rank_table"),
                     br(),
                     h3("Observed / Expected Species"),
                     div(
                       p("Species lists include all modeled species with a habitat rank of 3 or more. Observed species are those that have quality observation records within
                         the HUC12 and Range Expected are those species with none or poor quality observations, but the HUC12 falls within the modeled range."),
                       tableOutput("obs_table")
                     )
                 )
        ),
        tabPanel("Data Summary",
                 br(),
                 h3("Assessment Summary"),
                 tableOutput("summary_table"),
                 br(),
                 h3("Top Habitat Types for Selected Species"),
                 tags$details(
                   tags$summary("This table summarizes only the top ranking habitat types (ranks 4 or 5) for the selected species. Click to view score definitions."),
                   tags$div(
                     style="margin-left:15px; font-size:14px; color:#444;",
                     tags$b("Habitat Score (0–5):"),
                     tags$ul(
                       tags$li("4 – Decent potential habitat..."),
                       tags$li("5 – Best potential habitat...")
                     )
                   )
                 ),
                 tableOutput("top_habitat_summary")
        ),
        tabPanel("Help",directions_ui("directions")),
        tabPanel("Included Species", h3("Species Data Available in App"), DTOutput("all_species_table")
        )
      )  
    )    
  )      
) 

# SERVER
server <- function(input, output, session) {
  directions_server("directions")
  #full species list
  output$all_species_table <- DT::renderDT({display_tbl <- metadata_species %>% select(Taxa_Group, Scientific_Name, Common_Name, Habitat_Clade, Game_Status)
    DT::datatable(display_tbl, filter = "top", options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE))
  })
  # Core reactive values
  selected_species <- reactiveVal(character(0)) #species selected by user
  assessment_species <- reactiveVal(character(0)) #species used for assessment once run
  selected_huc12 <- reactiveVal(NULL)
  selected_vegID <- reactiveVal(NULL)
  assessment_cache <- reactiveVal(NULL)
  #Filter species based on category selections
  active_filter <- reactiveVal("none")
  #map display reactive
    map_score_selected <- reactive({
    req(input$map_score_checkbox)
    input$map_score_checkbox
  })
  #threshold sliders
  threshold_debounced <- debounce(reactive(input$threshold), 200)
  acreage_debounced <- debounce(reactive(input$acreage_threshold), 200)
  #sync numeric box <-> slider
  observeEvent(input$acreage_threshold_box, {updateSliderInput(session,"acreage_threshold",value = input$acreage_threshold_box)})
  observeEvent(input$acreage_threshold, {updateNumericInput(session,"acreage_threshold_box",value = input$acreage_threshold)})
  # Unique county list (split multi-county strings)
  unique_counties <- reactive({
    county_strings <- huc_county_joined$County_List
    county_split <- strsplit(county_strings, ",\\s*")
    sort(unique(unlist(county_split)))})
  observeEvent(unique_counties(), {
    updateSelectizeInput(
      session,
      "county_filter",
      choices = unique_counties(),
      server = TRUE
    )
  }, ignoreInit = FALSE, once = TRUE)
  #filter species list with group selection
  observeEvent(list(input$taxa_group, input$game_status, input$habitat_clade), {
    changed <- NULL
    if (input$taxa_group != "All" && active_filter() != "taxa") {
      changed <- "taxa"
    } else if (input$game_status != "All" && active_filter() != "game") {
      changed <- "game"
    } else if (input$habitat_clade != "All" && active_filter() != "habitat") {
      changed <- "habitat"
    }
    # If nothing changed, stop
    if (is.null(changed)) return()
    # Set the active filter
    active_filter(changed)
    # Reset the other two filters WITHOUT triggering observers again
    if (changed == "taxa") {
      updateSelectInput(session, "game_status", selected = "All")
      updateSelectInput(session, "habitat_clade", selected = "All")
    }
    if (changed == "game") {
      updateSelectInput(session, "taxa_group", selected = "All")
      updateSelectInput(session, "habitat_clade", selected = "All")
    }
    if (changed == "habitat") {
      updateSelectInput(session, "taxa_group", selected = "All")
      updateSelectInput(session, "game_status", selected = "All")
    }
  })
  # Filter species based on active filter
  filtered_species <- reactive({
    lookup <- species_lookup %>% filter(!is.na(Common_Name))
    filter_type <- active_filter()
    if (filter_type == "taxa" && input$taxa_group != "All") {lookup <- lookup %>% filter(Taxa_Group == input$taxa_group)}
    if (filter_type == "game" && input$game_status != "All") {lookup <- lookup %>% filter(Game_Status == input$game_status)}
    if (filter_type == "habitat" && input$habitat_clade != "All") {lookup <- lookup %>% filter(Habitat_Clade == input$habitat_clade)}
    sort(unique(lookup$Species))})
  # Sync dropdown → selected_species
  observeEvent(input$species_picker, {
    # ADD new selections but do not remove old ones
    sel <- unique(c(selected_species(), input$species_picker))
    selected_species(sel)
  })
  #Select All
  observeEvent(input$select_all_species, {
    lookup <- filtered_species()
    updateSelectizeInput(session, "species_picker", selected = lookup)
    selected_species(lookup)})
  #Clear All
  observeEvent(input$clear_all_species, {
    updateSelectizeInput(session, "species_picker", selected = character(0))
    selected_species(character(0))})
  # Update dropdown choices when filters change
  observeEvent(filtered_species(), {
    lookup <- species_lookup %>%
      filter(Species %in% filtered_species()) %>%
      arrange(Common_Name)
    # Preserve previously selected species
    current_selection <- selected_species()
    still_valid <- current_selection[current_selection %in% lookup$Species]
    updateSelectizeInput(session, "species_picker", choices = setNames(lookup$Species, lookup$Common_Name),selected = still_valid, server = TRUE)})
  # Display selected species in sidebar
  output$selected_species_display <- renderUI({
    sel <- selected_species()
    if (length(sel) == 0) {return(tags$span(style="color:#777","No species selected"))}
    lookup <- species_lookup %>% filter(Species %in% sel) %>% arrange(Common_Name)
    tags$ul(lapply(lookup$Common_Name, tags$li))})
  # Run Assessment After Selection
  observeEvent(
    input$run_assessment,
    {selected <- selected_species()
      if (length(selected) == 0) {showNotification("Please select at least one species.", type = "warning", duration = 3)
        return()}
      assessment_species(selected)
      # Build cache only once
      dat <- species_data %>% filter(Species %in% selected)
      assessment_cache(list(
        species_wide = dat %>% 
          select(Veg_ID, Common_Name, HUC12, Species, SpTEI) %>% 
          pivot_wider(names_from = Species, values_from = SpTEI),
        ems_wide = dat %>% 
          select(Veg_ID, HUC12, Species, EMSRank) %>% 
          pivot_wider(names_from = Species, values_from = EMSRank, names_glue = "{Species}_EMS"),
        huc_wide = dat %>% 
          select(Veg_ID, HUC12, Species, HUCRank) %>% 
          pivot_wider(names_from = Species, values_from = HUCRank, names_glue = "{Species}_HUC")
      ))
      showNotification(paste0("Assessment running for ", length(selected), " species..."),
        type = "message", duration = 3)})
  #Reset button
  observeEvent(input$reset_assessment, {
    # CLEAR SPECIES SELECTION
    selected_species(character(0))
    assessment_species(character(0))
    updateSelectizeInput(session, "species_picker", selected = character(0))
    # CLEAR LOCATION SELECTION
    selected_huc12(NULL)
    selected_vegID(NULL)
    # CLEAR LOCATION-DEPENDENT UI OUTPUTS
    output$selected_location_summary <- renderUI({})
    output$species_rank_table <- DT::renderDataTable({})
    output$obs_table <- renderTable({})
    # RESET CATEGORY FILTERS
    updateSelectInput(session, "taxa_group", selected = "All")
    updateSelectInput(session, "game_status", selected = "All")
    updateSelectInput(session, "habitat_clade", selected = "All")
    # RESET DISPLAY OPTION (map score)
    updateRadioButtons(session, "map_score_checkbox", selected="Normalized_SpTEI")
    # RESET THRESHOLDS
    updateSliderInput(session, "threshold", min = 0, max = 1, value = 0, step = 0.05)
    updateSliderInput(session, "acreage_threshold", min = 0, max = 1, value = 0)
    # CLEAR MAP MARKERS & LEGEND
    leafletProxy("hotspot_map") %>%
      clearMarkers() %>%
      clearControls() %>%
      setView(lng = -99.5, lat = 31.5, zoom = 6)
    showNotification("Assessment reset.", type = "message", duration = 2)})
  #assessment calculations
  selected_data <- reactive({
    req(length(assessment_species()) > 0)
    species_data %>% filter(Species %in% assessment_species())})
  # CALCULATE HOTSPOT SCORES
  hotspot_scores <- reactive({
    req(assessment_species())
    cache <- assessment_cache()
    req(cache)
    species_wide <- cache$species_wide
    ems_wide <- cache$ems_wide
    huc_wide <- cache$huc_wide
    species_cols <- intersect(assessment_species(), names(species_wide))
    validate(need(length(species_cols)>0, "Selected species have no SpTEI values."))
    scores <- species_wide %>%
      mutate(SpTEI_Total = rowSums(across(all_of(species_cols)), na.rm = TRUE)) %>%
      left_join(ems_wide, by = c("Veg_ID", "HUC12")) %>%
      left_join(huc_wide, by = c("Veg_ID", "HUC12"))
    EMS_cols <- grep("_EMS$", names(scores), value = TRUE)
    HUC_cols <- grep("_HUC$", names(scores), value = TRUE)
    scores <- scores %>%
      mutate(across(all_of(EMS_cols),
                    ~case_when(.==5 ~ 1, .==4 ~ 0.66, .==3 ~ 0.33, TRUE ~0),
                    .names="{.col}_weighted")) %>%
      mutate(across(all_of(HUC_cols),
                    ~case_when(.==3 ~1, .==2 ~0.5, TRUE~0),
                    .names="{.col}_weighted"))
    EMS_w <- grep("_EMS_weighted$", names(scores), value=TRUE)
    HUC_w <- grep("_HUC_weighted$", names(scores), value=TRUE)
    scores <- scores %>%
      mutate(
        Overall_Habitat_Score = rowSums(across(all_of(EMS_w)), na.rm = TRUE),
        Normalized_Habitat_Score = Overall_Habitat_Score / length(assessment_species()),
        Overall_Observation_Score = rowSums(across(all_of(HUC_w)), na.rm = TRUE),
        Normalized_Observation_Score = Overall_Observation_Score / length(assessment_species()),
        Species_count = rowSums(!is.na(across(all_of(species_cols)))),
        Species_total = length(assessment_species()),
        Species_coverage = Species_count / Species_total
      )
    max_possible <- length(assessment_species()) * 15
    scores <- scores %>%
      mutate(Normalized_SpTEI      = SpTEI_Total / max_possible)
    scores %>%
      select(
        Veg_ID, HUC12,
        SpTEI_Total, Normalized_SpTEI,
        Species_count, Species_total,
        Overall_Habitat_Score, Normalized_Habitat_Score,
        Overall_Observation_Score, Normalized_Observation_Score,
        Species_coverage,
        any_of(EMS_cols),       
        all_of(species_cols))
  })
  # JOIN SCORES TO LIGHTWEIGHT MAP POINTS
  hotspot_map_data <- reactive({
    req(assessment_species())
    scores <- hotspot_scores()
    # Pre-trim COA_EMS BEFORE joining (Huge performance win)
    ems_small <- COA_EMS %>%
      select(Veg_ID, HUC12, Habitat_Name, Acres)
    # Join lightweight tables
    hotspot_points %>%
      left_join(ems_small, by = c("Veg_ID", "HUC12")) %>%
      left_join(scores,    by = c("Veg_ID", "HUC12")) %>%
      left_join(huc_county_joined, by = "HUC12")
  })
  # JOIN HOTSPOT SCORES TO COA EMS
  COA_EMS_joined <- reactive({
    req(assessment_species())
    scores <- hotspot_scores()
    scores %>%
      left_join(COA_EMS, by = c("Veg_ID", "HUC12")) %>%      
      left_join(huc_county_joined, by = "HUC12") %>%  
      select(
        ObjID,
        Veg_ID,
        HUC12,
        Habitat_Name,
        Acres,
        # HOTSPOT INFORMATION
        Normalized_SpTEI,
        Species_count,
        Species_total,
        Overall_Habitat_Score,
        Normalized_Habitat_Score,
        Overall_Observation_Score,
        Normalized_Observation_Score,
        Species_coverage,
        # INDIVIDUAL SPECIES
        any_of(assessment_species()))})
  # SPECIES COUNT DISPLAY
  output$species_count <- renderText({
    selected <- selected_species()
    if (length(selected) == 0) {return("No species selected")}
    paste(length(selected), "species selected")})
  #capture map clicks
  observeEvent(input$hotspot_map_marker_click, {
    click <- input$hotspot_map_marker_click
    if (is.null(click)) return()
    id_parts <- strsplit(click$id, "_")[[1]]
    selected_veg <- id_parts[1]
    selected_huc <- id_parts[2]
    selected_huc12(selected_huc)
    selected_vegID(selected_veg)
  })
  #create selected species detail table
  habitat_name <- reactive({
    req(selected_vegID(), selected_huc12())
    COA_EMS %>%
      filter(Veg_ID == selected_vegID(),
             HUC12 == selected_huc12()) %>%
      pull(Habitat_Name) %>%
      unique() %>%
      first()
  })
  output$selected_location_summary <- renderUI({
    req(selected_vegID(), selected_huc12())
    div(
      style = "padding:10px;
             background:#f2f2f2;
             border:1px solid #ccc;
             border-radius:6px;
             margin-bottom:12px;
             font-size:14px;",
      tags$b("Selected Habitat Details"), tags$br(),
      paste("Veg ID:", selected_vegID()), tags$br(),
      paste("HUC12:", selected_huc12()), tags$br(),
      paste("Habitat Name:", habitat_name())
    )
  })
  output$species_rank_table <- DT::renderDataTable({
    huc <- selected_huc12()
    veg <- selected_vegID()
    req(huc, veg)
    req(length(assessment_species()) > 0)
    selected_sp <- assessment_species()
    dat <- species_data %>%
      filter(
        HUC12 == huc,
        Veg_ID == veg,
        Species %in% selected_sp
      ) %>%
      select(Veg_ID, HUC12, Species, EMSRank, HUCRank) %>% 
      left_join(species_lookup %>% select(Species, Common_Name), by = "Species") %>%
      left_join(COA_EMS %>% select(Veg_ID, HUC12, Habitat_Name), by = c("Veg_ID", "HUC12")) %>%
      select(Common_Name,EMSRank,HUCRank) %>%
      rename(`Habitat Score` = EMSRank, `Observation Score` = HUCRank) %>% 
      arrange(desc(`Habitat Score`))
    DT::datatable(dat, options = list(pageLength = 20, deferRender = TRUE)) %>%
      DT::formatStyle("Habitat Score",backgroundColor = DT::styleInterval(4, c("white", "lightyellow")))
  })
  #create species observation table - EMSRank 3+
  output$obs_table <- renderTable({
    huc <- selected_huc12()
    veg <- selected_vegID()
    req(huc, veg)
    dat <- obs %>%
      filter(HUC12 == huc) %>%
      left_join(species_lookup, by = "Species") %>%
      left_join(
        species_data %>% select(Species, Veg_ID, HUC12, EMSRank),
        by = c("Species", "HUC12")
      ) %>%
      filter(Veg_ID == veg) %>%
      filter(EMSRank %in% c(3, 4, 5))
    # Observed species = HUCRank 2 or 3
    observed <- dat %>%
      filter(HUCRank %in% c(2, 3)) %>%
      arrange(Common_Name) %>%
      pull(Common_Name) %>%
      discard(is.na)
    # Range expected species = HUCRank 1
    expected <- dat %>%
      filter(HUCRank == 1) %>%
      arrange(Common_Name) %>%
      pull(Common_Name) %>%
      discard(is.na)
    tibble(Observed = ifelse(length(observed) > 0, paste(observed, collapse = ", "), "None"),
      Range_Expected = ifelse(length(expected) > 0, paste(expected, collapse = ", "), "None"))
  })
  # DYNAMIC THRESHOLD SLIDER RANGE
  observeEvent(
    list(map_score_selected(), assessment_species()),
    {
      selected <- assessment_species()
      req(selected)
      if (map_score_selected() %in%
          c("Normalized_SpTEI",
            "Normalized_Habitat_Score",
            "Normalized_Observation_Score",
            "Species_coverage")) {
        updateSliderInput(session, "threshold", min = 0, max = 1, value = 0, step = 0.05)
      } else if (map_score_selected() == "Species_count") {
        updateSliderInput(session, "threshold", min = 0, max = length(selected), value = 0, step = 1)
      } else { updateSliderInput(session, "threshold", min = 0, max = length(selected) * 15, value = 0, step = 1)}
    },
    ignoreInit = FALSE)
  # HOTSPOT MAP
  output$hotspot_map <- renderLeaflet({
    huc12_poly_simplified <- sf::st_simplify(huc12_poly, dTolerance = 150, preserveTopology = TRUE)
      huc12_poly_simplified <- huc12_poly_simplified[sf::st_geometry_type(huc12_poly_simplified) %in% c("POLYGON", "MULTIPOLYGON"), ]
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = huc12_poly_simplified, color = "#333333", weight = 1.5, opacity = 0.8, fill = FALSE, group = "HUC12") %>%
      addLayersControl(overlayGroups = c("HUC12"), options = layersControlOptions(collapsed = FALSE))%>%
      setView(lng = -99.5, lat = 31.5, zoom = 6)})
  update_hotspot_map <- function() {
    req(assessment_species())
    raw_map_data <- hotspot_map_data()
    selected_metric <- map_score_selected()
    full_scores <- raw_map_data[[selected_metric]]
    full_scores <- full_scores[!is.na(full_scores) & full_scores > 0]
    map_data <- raw_map_data %>%
      filter(!is.na(.data[[selected_metric]])) %>%
      filter(.data[[selected_metric]] > 0) %>%
      filter(.data[[selected_metric]] >= threshold_debounced())
    if (!is.null(input$county_filter) && length(input$county_filter) > 0) {
      map_data <- map_data %>%
        filter(purrr::map_lgl(
          County_List,
          function(x) {
            counties <- unlist(strsplit(x, ",\\s*"))
            any(counties %in% input$county_filter)
          }
        ))
    }
    score_values <- map_data[[selected_metric]]
    # Filter: remove non-positive values
    map_data <- map_data %>% filter(.data[[selected_metric]] > 0)
    # Recompute score_values after filter
    score_values <- map_data[[selected_metric]]
    # Filter: apply threshold
    map_data <- map_data %>% filter(.data[[selected_metric]] >= threshold_debounced())
    # Recompute score_values again
    score_values <- map_data[[selected_metric]]
    proxy <- leafletProxy("hotspot_map", data = map_data)
    proxy %>% clearMarkers() %>% clearControls()
    # legend color logic
    if (selected_metric %in% c(
      "Normalized_SpTEI",
      "Normalized_Habitat_Score",
      "Normalized_Observation_Score",
      "Species_coverage"
    )) {
      color_domain <- c(0, 1)
    } else if (selected_metric == "Species_count") {
      color_domain <- c(0, length(assessment_species()))
    } else if (selected_metric == "Overall_Habitat_Score") {
      color_domain <- range(full_scores, na.rm = TRUE)
    } else if (selected_metric == "Overall_Observation_Score") {
      color_domain <- range(full_scores, na.rm = TRUE)
    } else {
      # Fallback: use actual data range
      color_domain <- range(full_scores, na.rm = TRUE)
    }
    pal <- colorNumeric("YlGnBu", domain = color_domain, reverse = FALSE)
    # marker radius logic
    {marker_radius <- rep(6, length(score_values))}
    # popup_text stays the same, use your existing block
    popup_text <- sprintf(
      "<b>Veg_ID:</b> %s<br>
      <b>Habitat_Name:</b> %s<br>
      <b>HUC12:</b> %s<br>
      <b>Total Acres:</b> %s<br>
      <b>County(ies):</b> %s<hr>
      <b>Overall Score:</b> %.3f<br>
      <b>Habitat Score:</b> %.3f<br>
      <b>Observation Score:</b> %.3f<br>
      <b>Selected Species in Range:</b> %s / %s",
      map_data$Veg_ID,
      map_data$Habitat_Name,
      map_data$HUC12,
      formatC(map_data$Acres, digits = 2, format = "f", big.mark = ","),
      map_data$County_List,
      map_data$Normalized_SpTEI,
      map_data$Normalized_Habitat_Score,
      map_data$Normalized_Observation_Score,
      map_data$Species_count,
      map_data$Species_total
    )
    proxy %>% addCircleMarkers(
      radius = marker_radius,
      stroke = TRUE,
      weight = 1,
      color = "white",
      fillColor = pal(score_values),
      fillOpacity = 0.8,
      popup = popup_text,
      layerId = ~paste0(Veg_ID, "_", HUC12))
    proxy %>% addLegend(
      position = "bottomright",
      pal = pal,
      values = full_scores,
      title = switch(
        map_score_selected(),
        "Normalized_Habitat_Score"     = "Habitat Score",
        "Normalized_Observation_Score" = "Observation Score",
        "Species_count" = "Species Count",
        "Species_coverage" = "Species Coverage"))}
  #only show HUC12 polys when zoomed in
  observe({zoom <- input$hotspot_map_zoom
    proxy <- leafletProxy("hotspot_map")
    if (!is.null(zoom)) {if (zoom >= 12) {proxy %>% showGroup("HUC12")} else {proxy %>% hideGroup("HUC12")}}})
  #update map when assessment species changes
  observeEvent(assessment_species(), {
    req(assessment_species())
    excluded_ids <- c("9630", "9620", "9601")
    map_data <- hotspot_map_data() %>% filter(!is.na(Normalized_SpTEI)) %>% filter(!Veg_ID %in% excluded_ids)
    max_acre <- max(map_data$Acres, na.rm = TRUE)
    updateSliderInput(session, "acreage_threshold", min = 0, max = max_acre, value = 0, step = 1)
    })
  observeEvent(assessment_species(), {
    update_hotspot_map()
    # AUTO ZOOM to selected assessment extent
    map_data <- hotspot_map_data() %>% filter(!is.na(Normalized_SpTEI))
    if (nrow(map_data) > 0) {
      coords <- sf::st_coordinates(map_data)
      lng_range <- range(coords[,1], na.rm = TRUE)
      lat_range <- range(coords[,2], na.rm = TRUE)
      leafletProxy("hotspot_map") %>%
        fitBounds(lng1 = lng_range[1], lat1 = lat_range[1], lng2 = lng_range[2], lat2 = lat_range[2])}})
  #update map when map display changes
  observeEvent(map_score_selected(), {update_hotspot_map()})
  #update map when threshold slider changes
  observeEvent(threshold_debounced(), {update_hotspot_map()})
  #acreage threshold trigger
  observeEvent(acreage_debounced(), {update_hotspot_map()})
  #county filter trigger
  observeEvent(input$county_filter, {update_hotspot_map()})
  # SUMMARY TABLES
  #top habitat summary
  top_habitat_summary <- reactive({
    req(length(assessment_species()) > 0)
    # Filter to selected species with EMSRank 4 or 5
    dat <- species_data %>%
      filter(Species %in% assessment_species(), EMSRank %in% c(4, 5)) %>%
      distinct(Veg_ID, Species, EMSRank)   # important fix
    # Veg_ID + Species duplicates removed
    if (nrow(dat) == 0) {
      return(tibble(
        Veg_ID = character(0),
        Common_Name = character(0),
        Species_count = numeric(0),
        Habitat_Score = numeric(0),
        Species_List = character(0)))
    }
    habitat_summary <- dat %>%
      group_by(Veg_ID) %>%
      summarise(Species_count = n_distinct(Species),
        Habitat_Score = max(EMSRank, na.rm = TRUE),
        Species_List = paste(sort(unique(Species)), collapse = ", "),
        .groups = "drop"
      ) %>%
      left_join(COA_EMS %>%
          distinct(Veg_ID, .keep_all = TRUE) %>%
          select(Veg_ID, Habitat_Name), by = "Veg_ID") %>%
      mutate(Habitat_Score = as.integer(Habitat_Score)) %>%
      arrange(desc(Species_count), desc(Habitat_Score))  
    habitat_summary
  })
  output$top_habitat_summary <- renderTable({
    top_habitat_summary()
  })
  #metrics summary table
  output$summary_table <-
    renderTable({
      req(length(assessment_species()) > 0)
      scores <- hotspot_scores()
      # Identify all EMS columns
      EMS_cols <- grep("_EMS$", names(scores), value = TRUE)
      # Count locations with ANY EMSRank >= 3
      locations_with_ems3plus <- sum(apply(scores[EMS_cols], 1, function(x) any(x >= 3, na.rm = TRUE)))
      tibble(Metric = c("Number of Potential Habitat Points","Mean Hotspot Score", "Maximum Hotspot Score",
          "Mean Species Coverage", "Maximum Species Count"),
        Value = c(formatC(locations_with_ems3plus, format = "f", digits = 0, big.mark = ","),
                  formatC(mean(scores$Normalized_SpTEI, na.rm = TRUE), format = "f", digits = 2),
                  formatC(max(scores$Normalized_SpTEI, na.rm = TRUE), format = "f", digits = 2),
                  formatC(mean(scores$Species_coverage, na.rm = TRUE), format = "f", digits = 2),
                  formatC(max(scores$Species_count, na.rm = TRUE), format = "f", digits = 2)))})
  # SELECTED SPECIES TABLE
  output$selected_species_table <-
    renderTable({
      selected <- assessment_species()
      metadata_species %>%
        left_join(species_lookup %>% select(Species, Species_Key), by = "Species_Key") %>%
        filter(Species %in% selected) %>%
        select(
          Species,
          Common_Name,
          Taxa_Group,
          Game_Status,
          Habitat_Clade
        )%>%
        arrange(Common_Name)
        })
  # DOWNLOAD RESULTS
  output$download_results <-
    downloadHandler(
      filename = function() {
        paste0(
          "COA_v2026_SpTEI_Assessment_",Sys.Date(),".csv")},
      content = function(file) {
        req(length(assessment_species()))
        write_csv(COA_EMS_joined(), file)}, contentType = "text/csv")
}

shinyApp(ui = ui, server = server)