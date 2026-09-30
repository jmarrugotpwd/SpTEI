#SpTEI App Directions and Explanations

directions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Title
    h1("How to Use the Multi‑SGCN Assessment Tool", 
       style = "margin-bottom:20px;"),
    # Intro
    p("This tool is designed to harness the species data used in the Conservation Opportunity Areas of Texas model to assess localities
      and habitats associated to the modeled SGCN."),
    br(),
    # ABOUT TOOL
    h2("Why use this tool?"),
    p("This tool allows the user to mix and match SGCN data and assess hotspot areas for the selected species. Metrics regarding SGCN habitats
      and observations are calculated in the tool to provide a final pictures of areas or habitats that can be prioritized for conservation."
    ),
    h2("How the tool works"),
    p("The tool provides custom data for each Ecological Mapping System (EMS) type within the state. To reduce processing power for the app, each EMS type
      in each HUC12 are represented in the tool by a single point. The tool assesses the data based for selected species and provides metrics. Clicking
      on any point on the map provides insight to the specific metrics for each EMS type within the HUC12. The tool also provides summary tables highlighting
      the top scoring habitat types for the selected species and the data is also available for export in .csv format that can join to the EMS data found in 
      the COA_EMS.gdb to view the data in detail in GIS software."),
    tags$ul(
      tags$li("Species‑specific EMS and HUC ranks"),
      tags$li("Observed and expected species for that location"),
      tags$li("Habitat quality scores"),
      tags$li("Species coverage values")
    ),
    br(),
    # METRICS CALCULATIONS
    h3("Calculated Metrics"),
    p("The metrics calculated in the tool include:"),
    tags$ul(
      tags$li(strong("Overall Score:"), 
              "Assesses the total SpTEI score for each species and provides an overall value for all selected species"),
      tags$li(strong("Habitat Score:"), 
              "Assesses the individual habitat ranks for the selected species and provides a normalized value of the overall habitat score."),
      tags$li(strong("Observation Score:"), 
              "Assesses the HUC12 observation scores for each selected species and provides an overall values of the occupancy confidence for the HUC12."),
      tags$li(strong("Observation Score:"), 
              "Assesses the HUC12 observation scores for each selected species and provides an overall values of the occupancy confidence for the HUC12."),
      tags$li(strong("Species Count:"), 
              "How many of the selected species are within range and considered in the calculated metrics"),
      tags$li(strong("% Species in Range:"), 
              "Depicts how many of the selected species are within range at the selected point."),
      tags$li(strong("Potential Quality:"), 
              "The overall quality?."),
    ),
    br(),
    # TOOL OPTIONS
    h3("Map Display Options"),
    p("Use the display options above the map to choose the metric used to color and size each point.
       The map updates instantly without resetting your zoom or pan position."),
    tags$ul(
      tags$li(strong("Normalized SpTEI:"), " Scaled habitat quality score (0–1)."),
      tags$li(strong("Multi‑Species Hotspot:"), 
              " Composite score combining habitat quality and species coverage."),
      tags$li(strong("Total SpTEI:"), 
              " Sum of raw SpTEI values for all selected species."),
      tags$li(strong("Species Count:"), 
              " Number of selected species present at each location."),
      tags$li(strong("Species Coverage:"), 
              " Proportion (%) of selected species present.")
    ),
    br(),
    h3("Threshold Controls"),
    p("Use threshold sliders to refine the hotspots shown on the map."),
    tags$ul(
      tags$li(strong("Minimum Score:"), 
              " Filters points by the selected map metric."),
      tags$li(strong("Minimum Acres:"), 
              " Filters habitat locations by total acreage.")),
    br(),
    h3("Summary Tables"),
    p("The Summary tab contains multiple data summaries to help you understand broad patterns."),
    tags$ul(
      tags$li(strong("Assessment Summary:"), 
              " Overall statistics such as mean hotspot scores, species coverage, and locations assessed."),
      tags$li(strong("Top Habitat Types:"), 
              " Highlights habitats (Veg_IDs) that support the most high‑quality species (EMSRank 4–5)."),
      tags$li(strong("Top Hotspot Locations:"), 
              " Shows the highest‑valued multi‑species hotspots based on Normalized SpTEI.")
    ),
    br(),
    h3("Exporting Results"),
    p("Use the Download Results button on the left to export your full assessment, including all 
       Veg_ID locations, species scores, composite metrics, and summary statistics. The file is designed to easily join to
       the EMS dataset in the COA_EMS.gdb for detailed analysis in ArcPro."),
    br(),
    # ABOUT THE SPTEI FRAMEWORK
    h2("About SpTEI from the COA model"),
    p("SpTEI (Species Terrestrial Ecological Index) is the deductive modeling approach taken in the Conservation Opportunity Areas 
       of Texas model that uses ranked Ecological Mapping Systems (EMS) on a 0 - 5 scale and ranked HUC12 polygons using ranked observation data (PQI). 
       The final values are the result of multiplication of the EMSRank and HUCRank on a 0 - 15 scale"),
    p("This application uses SpTEI values at the Veg_ID scale to provide powerful multi‑species insights."),
    p(em("For questions or support, contact Jennifer Marrugo (jennifer.marrugo@tpwd.texas.gov).")
    )
  )
}

# Server function (empty for static content)
directions_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

