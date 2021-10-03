#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

library(shinydashboard)
library(shinydashboardPlus)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "Parcel Overpack Simulator", titleWidth = 500),
      dashboardSidebar(
        width = 300,
        mod_file_upload_ui("file_upload_ui_1")
      ),
      dashboardBody(
        mod_shipmentDT_ui("shipmentDT_ui_1"),
        mod_dimselector_ui("dimselector_ui_1")
      )
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

