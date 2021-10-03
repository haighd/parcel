#' dimselector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dimselector_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dimselector Server Functions
#'
#' @noRd 
mod_dimselector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_dimselector_ui("dimselector_ui_1")
    
## To be copied in the server
# mod_dimselector_server("dimselector_ui_1")
