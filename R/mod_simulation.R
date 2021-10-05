#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("var"))
  )
}
    
#' simulation Server Functions
#'
#' @noRd 
mod_simulation_server <- function(id, box_data, shipment_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$var <- renderText({shipment_data$weight_col()})
  })
}
    
## To be copied in the UI
# mod_simulation_ui("simulation_ui_1")
    
## To be copied in the server
# mod_simulation_server("simulation_ui_1")
