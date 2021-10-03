#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  file <- mod_file_upload_server("file_upload_ui_1")
  mod_shipmentDT_server("shipmentDT_ui_1", file)
  mod_dimselector_server("dimselector_ui_1")
}
