
# MULTI-SPECIES SPTEI HOTSPOT SHINY APP

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(shinyBS)
library(rsconnect)

source("data/directions.R")
precomputed <- readRDS("data/precomputed_app_data.rds")

species_data       <- precomputed$species_data
COA_EMS            <- precomputed$COA_EMS
hotspot_points     <- precomputed$hotspot_points
species_metadata   <- precomputed$species_metadata
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
    sidebarPanel(width = 3,
                 div(class = "sidebar-section",
                     h4("Species Filters"),
                     selectInput("taxa_group", "Taxa Group:",
                                 choices = c("All", sort(unique(na.omit(species_metadata$Taxa_Group))))),
                     selectInput("game_status", "Game Status:",
                                 choices = c("All", sort(unique(na.omit(species_metadata$Game_Status))))),
                     selectInput("habitat_clade", "Habitat Clade:",
                                 choices = c("All", sort(unique(na.omit(species_metadata$Habitat_Clade)))))),
                 hr(),
                 div(class="sidebar-section",
                     h4("Species Selection"),
                     fluidRow(
                       column(6, actionButton("select_all_species", "Select All", icon = icon("check"))),
                       column(6, actionButton("clear_all_species", "Clear All", icon = icon("times")))),
                     br(),
                     selectizeInput(
                       "species_picker", "Select Species:",
                       choices = names(species_choices),
                       multiple = TRUE,
                       options = list(placeholder = "Search species...", plugins = list("restore_on_backspace"))
                     ),
                     br(),
                     fluidRow(
                       column(6, actionButton("run_assessment", "Run", icon = icon("play"), class="btn btn-primary btn-block")),
                       column(6, actionButton("reset_assessment", "Reset", icon = icon("refresh"), class="btn btn-secondary btn-block"))
                     ),
                     helpText("Select one or more species to assess.")
                 ),
                 hr(),
                 div(class="sidebar-section", strong("Selected species:"), uiOutput("selected_species_display")
                 ),
                 hr(),
                 div(class="sidebar-section", h4("Export Results"), downloadButton("download_results", "Download CSV")
                 )
    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Hotspot Map",
                         div(class="map-section",
                             leafletOutput("hotspot_map", height = "600px"),
                             hr(),
                             h3("Map Display Options"),
                             radioButtons(
                               "map_score_checkbox", "Map Display Metric:",
                               choices = c(
                                 "Normalized SpTEI" = "Normalized_SpTEI",
                                 "Multi-Species Hotspot" = "MultiSpecies_Hotspot",
                                 "Habitat Score" = "Normalized_Habitat_Score",
                                 "Observation Score" = "Normalized_Observation_Score",
                                 "Species Count" = "Species_count",
                                 "Species Coverage" = "Species_coverage"),
                               inline = TRUE),
                             hr(),
                             h3("Map Filters"),
                             fluidRow(
                               column(6, sliderInput("acreage_threshold", "Minimum Acres:", min = 0, max = max(COA_EMS$Acres, na.rm = TRUE), value = 0, step = 1)),
                               column(6, sliderInput("threshold", "Minimum Score:", min = 0, max = 1, value = 0, step = 0.05))),
                             hr(),
                             h3("Species Data at Selected Location"),
                             DTOutput("species_rank_table"),
                             br(),
                             h3("Observed / Expected Species"),
                             tableOutput("obs_table"))),
                tabPanel("Summary",
                         br(),
                         h3("Assessment Summary"),
                         tableOutput("summary_table"),
                         br(),
                         h3("Top Habitat Types for Selected Species"),
                         tableOutput("top_habitat_summary")),
                tabPanel("Directions", directions_ui("directions")))
    )
  )
)

# SERVER
server <- function(input, output, session) {
  directions_server("directions")
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
  threshold_debounced <- debounce(reactive(input$threshold), 200)
  acreage_debounced <- debounce(reactive(input$acreage_threshold), 200)
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
    # RESET CATEGORY FILTERS
    updateSelectInput(session, "taxa_group", selected = "All")
    updateSelectInput(session, "game_status", selected = "All")
    updateSelectInput(session, "habitat_clade", selected = "All")
    # RESET DISPLAY OPTION (map score)
    updateRadioButtons(session, "map_score_checkbox", selected="Normalized_SpTEI")
    # RESET THRESHOLDS
    updateSliderInput(session, "threshold", min = 0, max = 1, value = 0, step = 0.05)
    updateSliderInput(session, "acreage_threshold", min = 0, max = 0, value = 0, step = 1)
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
      mutate(
        Normalized_SpTEI      = SpTEI_Total / max_possible,
        MultiSpecies_Hotspot  = sqrt(Normalized_SpTEI * Species_coverage)
      )
    scores %>%
      select(
        Veg_ID, HUC12,
        SpTEI_Total, Normalized_SpTEI,
        Species_count, Species_total,
        Overall_Habitat_Score, Normalized_Habitat_Score,
        Overall_Observation_Score, Normalized_Observation_Score,
        Species_coverage, MultiSpecies_Hotspot,
        all_of(species_cols)
      )
  })
  
  # JOIN SCORES TO LIGHTWEIGHT MAP POINTS
  hotspot_map_data <- reactive({
    req(assessment_species())
    scores <- hotspot_scores()
    # Pre-trim COA_EMS BEFORE joining (Huge performance win)
    ems_small <- COA_EMS %>%
      select(Veg_ID, HUC12, Common_Name, Acres)
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
        Common_Name,
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
        MultiSpecies_Hotspot,
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
  #create species detail table
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
      select(Species, EMSRank, HUCRank) %>% 
      left_join(
        species_lookup %>% select(Species, Common_Name),
        by = "Species"
      ) %>%
      select(
        Common_Name,
        Species,
        EMSRank,
        HUCRank
      ) %>%
      arrange(desc(EMSRank))
    DT::datatable(dat, options = list(pageLength = 20, deferRender = TRUE)) %>%
      DT::formatStyle("EMSRank",backgroundColor = DT::styleInterval(4, c("white", "lightyellow")))
  })
  #create species observation table - EMSRank 3+
  output$obs_table <- renderTable({
    huc <- selected_huc12()
    veg <- selected_vegID()
    req(huc, veg)
    # Join obs → species_lookup → species_data to add Veg_ID + EMSRank
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
    {selected <- assessment_species()
      req(selected)
      if (map_score_selected() %in%
        c("Normalized_SpTEI",
          "MultiSpecies_Hotspot",
          "Normalized_Habitat_Score",
          "Normalized_Observation_Score",
          "Species_coverage")
      ) {updateSliderInput(session,"threshold", min = 0, max = 1, value = 0, step = 0.05)
      } else if (map_score_selected() == "Species_count") {updateSliderInput(session,"threshold", min = 0, max = length(selected), value = 0, step = 1)
      } else if (map_score_selected() == "SpTEI_Total"
      ) {updateSliderInput(session, "threshold", min = 0, max = length(selected) * 15, value = 0, step = 1)}
    },ignoreInit = FALSE)
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
    map_data <- hotspot_map_data() %>% filter(!is.na(Normalized_SpTEI))
    score_values <- map_data[[map_score_selected()]]
    map_data <- map_data %>% filter(score_values > 0)
    keep_locations <- score_values[score_values > 0] >= threshold_debounced()
    map_data <- map_data[keep_locations, ]
    map_data <- map_data %>% filter(Acres >= acreage_debounced())
    proxy <- leafletProxy("hotspot_map", data = map_data)
    proxy %>% clearMarkers() %>% clearControls()
    # your color logic
    if (map_score_selected() %in% c("Normalized_SpTEI","MultiSpecies_Hotspot", "Normalized_Habitat_Score", "Normalized_Observation_Score", "Species_coverage")) {
      color_domain <- c(0,1)
    } else if (map_score_selected() == "Species_count") {
      color_domain <- c(0, length(assessment_species()))
    } else {color_domain <- c(0, length(assessment_species()) * 15)}
    pal <- colorNumeric("magma", domain = color_domain)
    # marker radius logic
    if (map_score_selected() %in% c("Normalized_SpTEI","MultiSpecies_Hotspot","Normalized_Habitat_Score","Normalized_Observation_Score")) {
      marker_radius <- 3 + 8 * score_values
    } else {marker_radius <- rep(6, length(score_values))}
    # popup_text stays the same, use your existing block
    popup_text <- sprintf(
      "<b>Veg_ID:</b> %s<br>
        <b>Common_Name:</b> %s<br>
        <b>HUC12:</b> %s<br>
        <b>Total Acres:</b> %s<br>
        <b>County(ies):</b> %s<hr>
        <b>Overall Score:</b> %.3f<br>
        <b>Habitat Score:</b> %.3f<br>
        <b>Observation Score:</b> %.3f<br>
        <b>Species Count:</b> %s / %s<br>
        <b>%% Species in Range:</b> %.1f%%<br>
        <b>Potential Quality:</b> %.3f",
      map_data$Veg_ID,
      map_data$Common_Name,
      map_data$HUC12,
      formatC(map_data$Acres, digits = 2, format = "f", big.mark = ","),
      map_data$County_List,
      map_data$Normalized_SpTEI,
      map_data$Normalized_Habitat_Score,
      map_data$Normalized_Observation_Score,
      map_data$Species_count,
      map_data$Species_total,
      map_data$Species_coverage * 100,
      map_data$MultiSpecies_Hotspot
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
      values = color_domain,
      title = switch(
        map_score_selected(),
        "Normalized_SpTEI" = "Normalized SpTEI",
        "MultiSpecies_Hotspot" = "Multi-Species Hotspot",
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
  
  # SUMMARY TABLES
  #top habitat summary
  top_habitat_summary <- reactive({
    req(length(assessment_species()) > 0)
    # Filter to selected species with EMSRank 4 or 5
    dat <- species_data %>%
      filter(
        Species %in% assessment_species(),
        EMSRank %in% c(4, 5)
      ) %>%
      distinct(Veg_ID, Species, EMSRank)   # important fix
    # Veg_ID + Species duplicates removed
    if (nrow(dat) == 0) {
      return(tibble(
        Veg_ID = character(0),
        Common_Name = character(0),
        Species_Count = numeric(0),
        EMS_Score = numeric(0),
        Species_List = character(0)
      ))
    }
    habitat_summary <- dat %>%
      group_by(Veg_ID) %>%
      summarise(
        Species_Count = n_distinct(Species),
        EMS_Score = max(EMSRank, na.rm = TRUE),
        Species_List = paste(sort(unique(Species)), collapse = ", "),
        .groups = "drop"
      ) %>%
      left_join(
        COA_EMS %>%
          distinct(Veg_ID, .keep_all = TRUE) %>%    # <-- solves duplicate issue
          select(Veg_ID, Common_Name),
        by = "Veg_ID"
      ) %>%
      arrange(desc(EMS_Score), desc(Species_Count))  # <-- correct ordering
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
      tibble(Metric = c("Locations assessed", "Maximum possible SpTEI", "Mean Hotspot Score", "Maximum Hotspot Score",
          "Mean Species Coverage", "Maximum Species Count", "Mean Multi-Species Hotspot"),
        Value = c(nrow(scores),
          length(assessment_species()) * 15,
          round(mean(scores$Normalized_SpTEI, na.rm = TRUE), 3),
          round(max(scores$Normalized_SpTEI, na.rm = TRUE), 3), 
          round(mean(scores$Species_coverage, na.rm = TRUE), 3),
          max(scores$Species_count, na.rm = TRUE), 
          round(mean(scores$MultiSpecies_Hotspot, na.rm = TRUE), 3)))})
  # SELECTED SPECIES TABLE
  output$selected_species_table <-
    renderTable({
      selected <- assessment_species()
      species_metadata %>%
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