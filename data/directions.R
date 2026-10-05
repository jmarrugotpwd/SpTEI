#SpTEI App Directions and Explanations

directions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Title
    br(),
    h1(strong("How to Use the Multi‑SGCN Assessment Tool"), 
       style = "margin-bottom:20px;"),
    # Intro
    p("This tool is designed to harness the species data used in the Conservation Opportunity Areas of Texas model to assess localities
      and habitats associated to the modeled SGCN."),
    h3("About SpTEI from the COA model"),
    p("SpTEI (Species Terrestrial Ecological Index) is the deductive modeling approach taken in the Conservation Opportunity Areas 
       of Texas model that uses ranked Ecological Mapping Systems (EMS) on a 0 - 5 scale and ranked HUC12 polygons using ranked observation data. 
       The final values are the result of multiplication of the EMSRank and HUCRank on a 0 - 15 scale"),
    # ABOUT TOOL
    h3("Why use this tool?"),
    p("This tool allows the user to mix and match SGCN data and assess hotspot areas for the selected species. Metrics regarding SGCN habitats
      and observations are calculated in the tool to provide a final pictures of areas or habitats that can be prioritized for conservation."
    ),
    h2("How the tool works"),
    p("The tool provides custom data for each Ecological Mapping System (EMS) type within the state. To reduce processing power for the app, each EMS type
      in each HUC12 are represented in the tool by a single point. The tool assesses the data based for selected species and provides metrics. Clicking
      on any point on the map provides insight to the specific metrics for each EMS type within the HUC12. The tool also provides summary tables highlighting
      the top scoring habitat types for the selected species and the data is also available for export in .csv format that can join to the EMS data found in 
      the COA_EMS.gdb to view the data in detail in GIS software."),
    # METRICS CALCULATIONS
    h3("Calculated Metrics"),
    p("The metrics calculated in the tool include:"),
    tags$ul(
      tags$li(strong("Overall Score:"), 
              "Assesses the total SpTEI score for each species and provides an overall value for all selected species"),
      tags$li(strong("Habitat Score:"), 
              "Sums the individual habitat ranks for the selected species and habitat type and provides a normalized value of the overall habitat score on a 0 - 1 scale."),
      tags$li(strong("Observation Score:"), 
              "Sums the HUC12 occupancy scores for the selected species and provides a normalized observation value on a 0 - 1 scale."),
      tags$li(strong("Selected Species in Range:"), 
              "Counts the number of the selected species with potential to occur at the selected location regardless of habitat rank. Specific details on each of the selected species
              can be assessed in the Species Data at Selected Location table below the map."),
    ),
    # TOOL OPTIONS
    h3("Map Display Options"),
    p("The map display options allow for visual representation of the calculated metrics."),
    h3("Threshold Controls"),
    p("Use threshold sliders to refine the hotspots shown on the map."),
    tags$ul(
      tags$li(strong("Minimum Score:"), 
              " Filters points by the selected map metric."),
      tags$li(strong("Minimum Acres:"), 
              " Filters points by total acreage."),
      tags$li(strong("Filter by County:"), 
              " Filters points associated with the selected counties. Because each point is a representation of the habitat type within a HUC12, 
              some points may overlap multiple counties and are reflected in the popup."),
      ),
    h3("Summary Tab"),
    p("The Summary tab contains multiple data summaries to help you understand broad patterns, including an assessment summary of overall metrics and 
      top ranked habitat types for the selected species."),
    p(strong("Assessment Summary:"), "Overall metrics for the selected species."),
    tags$ul(
      tags$li("Number of Potential Habitat Points: Depicts the total number of points that are ranked 3 or above for any of the selected species."),
      tags$li("Mean Hotspot Score: The mean total SpTEI score normalized on a 0 - 1 scale."),
      tags$li("Maximum Hotspot Score: Summarizes the highest overall value possible for the selected species. If species have similar habitat rankings the number is higher and significant differences see a 
              lower value."),
      tags$li("Mean Species Coverage: Summarizes the mean value of selected species range overlap. The more range overlap the higher the mean."),
      tags$li("Maximum Species Count: Summarizes the maximum number of selected species within a given point. If species do not overlap in range, the value will be 1.")),
    p(strong("Top Habitat Types for Selected Species:"), "Summarizes the highest potential quality habitats for the selected species and the respective associated species."),
    h3("Exporting Results"),
    p("Use the Download Results button on the left to export your full assessment, including all 
       Veg_ID locations, species scores, composite metrics, and summary statistics. The file is designed to easily join to
       the EMS dataset in the COA_EMS.gdb for detailed analysis in ArcPro."),
    p(em("For questions or support, contact Jennifer Marrugo (jennifer.marrugo@tpwd.texas.gov).")
    )
  )
}

# Server function (empty for static content)
directions_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

