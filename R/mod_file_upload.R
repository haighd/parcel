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
library(here)

mod_file_upload_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      div(
          shiny::actionLink(
            inputId = ns('demo'),
            label = 'Load demo shipment data.'
          ),
          style="display:flex;"
      )
    ),
    fluidRow(
      div(
          shiny::downloadLink(
            outputId = ns("template"),
            label = 'Download shipment data template.'
          ),
          style="display:flex;"
      )
    ),
    fluidRow(
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
  )
}

#' file_upload Server Functions
#'
#' @noRd
mod_file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rvFile <- reactiveValues()
    
    observeEvent(input$file1, {
      rvFile$file <- input$file1
      
      ext <- tools::file_ext(rvFile$file$datapath)
      
      validate(need(ext %in% c("csv", "tsv", "xls", "xlsx"), "Please upload a csv, tsv/txt, xls, or xlsx file"))
      
      if (ext == "csv") {
        df <- file_csv(rvFile$file$datapath)
      } else if (ext %in% c("tsv", "txt")) {
        df <- file_tsv(rvFile$file$datapath)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- readxl::read_excel(rvFile$file$datapath, col_types = 'text')
      }
      
      rvFile$data <- df
      
    })
    
    observeEvent(input$demo, {
      # rvFile$data <- file_csv(here::here("inst", "app", "www", "sample1.csv"))
      rvFile$data <- file_csv(system.file("extdata", "sample1.csv", package = "parcel"))
    })
    
    
    templateDf <- reactive({data.frame(
      order_id = c("1", "1", "2"),
      material_id = c("ABC123", "DEF456", "ABC123"),
      material_length = c("11.25", "13.5", "11.25"),
      material_width = c("6", "9", "6"),
      material_height = c("3", "2.5", "3"),
      material_weight = c("5.25", "11", "5.25"),
      material_quantity = c("1", "3", "2")
    )})
    
    output$template <- downloadHandler(
      filename = 'template.csv',
      content = function(file){
        readr::write_csv(templateDf(), file)
      }
    )
    
    return(reactive({rvFile$data}))
    
  })
  
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_ui_1")

## To be copied in the server
# mod_file_upload_server("file_upload_ui_1")
