#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

library(shinydashboard)
library(magrittr)
library(shinydashboardPlus)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      # skin = "midnight",
      header = shinydashboardPlus::dashboardHeader(title = "Parcel Overpack Simulator", titleWidth = 350),
      sidebar = shinydashboardPlus::dashboardSidebar(
        minified = FALSE,
        width = 350,
        column(
          width = 12,
          sidebarPanel(
            width = 12,
            fluidRow(
              h4('Step 1: Upload Shipments')
            ),
            fluidRow(
              helpText("Select a .csv, .tsv/.txt, .xlsx, or .xls file, containing shipment data.")
            ),
              mod_file_upload_ui("file_upload_ui_1"),
            fluidRow(
              helpText("Shipment data should contain, at minimum, the following fields: order id, material id, material length, material width, material height, material weight, material quantity."),
            ),
            fluidRow(
              hr()
            )
          ),
          sidebarPanel(
            width = 12,
            fluidRow(
              h4("Step 2: Assign input columns")
            ),
            fluidRow(
              helpText("Drag and drop each column name (from the input data) to it's corresponding attribute."),
            ),
            fluidRow(
              hr()
            )
          ),
          sidebarPanel(
            width = 12,
            fluidRow(
              h4("Step 3: Select Overpack Sizes")
            ),
            fluidRow(
              helpText("Add as many (or, as few) overpack box sizes, to be utilized in the simulation."),
            ),
            fluidRow(
              mod_boxes_ui("boxes_ui_1")
            ),
            fluidRow(
              hr()
            )
          ),
          sidebarPanel(
            width = 12,
            fluidRow(
              h4("Step 4: Run Simulation"),
              helpText(
                paste0(
                  "Once a data field has been associated with each",
                  " required field, and, at least, one overpack box", 
                  " has been added, the Run Simulation section will",
                  " appear."
                ), 
                br(), 
                br(),
                paste0(
                  "Select the respective buttons",
                  " to run the simulation, and to download the", 
                  " simulation results."
                ), 
                br(), 
                br(), 
                paste0(
                  "The output data",
                  " contains several items: Order ID, Unique Bin ID",
                  " (within the respective order), and the x, y,",
                  " and z coordinates of each item, within the",
                  " respective overpack box."
                ), 
                br(), 
                br(),
                paste0(
                  "A bin id of '0'",
                  ", which will be accompanied by x, y and z values",
                  " of '-1', indicates an item that was not fit into",
                  " an overpack box; either add larger overpack boxes",
                  " , or consider those items for 'free case' shipping."
                )
              )
            )
          )
        )
      ),
      body = shinydashboard::dashboardBody(
        mod_shipmentDT_ui("shipmentDT_ui_1"),
        mod_colnames_ui("colnames_ui_1"),
        mod_boxes_body_ui("boxes_body_ui_1"),
        mod_simulation_ui("simulation_ui_1")
      ),
      scrollToTop = TRUE
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'parcel'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

