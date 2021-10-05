#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

library(stringr)
library(readxl)
library(readr)
library(DT)

mod_file_upload_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shiny::fileInput(
      ns('file1'),
      label = NULL,
      accept = c(
        'text/csv',
        'text/comma-separated-values,text/plain',
        '.tsv',
        '.csv',
        '.xlsx'
      )
    )
  )
}

#' file_upload Server Functions
#'
#' @noRd
mod_file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    reactive(input$file1)
    
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_ui_1")

## To be copied in the server
# mod_file_upload_server("file_upload_ui_1")
