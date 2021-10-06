#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  # values <- reactiveValues(data = data.frame(), new_data = data.frame())
  values <- reactiveValues(
    data = NULL, 
    new_data = NULL
  )
  
  file <- mod_file_upload_server(
    "file_upload_ui_1"
  )
  
  shipment_data <- mod_shipmentDT_server(
    "shipmentDT_ui_1", 
    file
  )
  
  colnames <- mod_colnames_server(
    "colnames_ui_1", 
    shipment_data
    )
  
  boxData <- mod_boxes_server(
    "boxes_ui_1",
    reactive(values$data)#,
    # reactive(values$new_data)
  )
  
  finalBoxData <- mod_boxes_body_server(
    "boxes_body_ui_1", 
    boxData)
  
  mod_simulation_server(
    "simulation_ui_1",
    finalBoxData,
    colnames
  )
}
