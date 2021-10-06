#' shipmentDT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

library(shinyjs)

mod_shipmentDT_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 11,
        shinydashboardPlus::box(
          id = ns("boxShipments"),
          DT::dataTableOutput(ns("contents")),
          width = NULL,
          title = "Shipment Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          background = NULL
        )
      )
    )
  )
}

#' shipmentDT Server Functions
#'
#' @noRd 
mod_shipmentDT_server <- function(id, file1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rvShip <- reactiveValues()
    
    observeEvent(rvShip, {
      rvShip$trigger = FALSE
    }, once = TRUE)
    
    df_shipments_upload <- shiny::reactive({
      req(file1())
      ext <- tools::file_ext(file1()$datapath)
      
      validate(need(ext %in% c("csv", "tsv", "xls", "xlsx"), "Please upload a csv, tsv/txt, xls, or xlsx file"))
      
      if (ext == "csv") {
        df <-
          readr::read_csv(file1()$datapath,
                          col_types = readr::cols(.default = readr::col_character()))
      } else if (ext %in% c("tsv", "txt")) {
        df <-
          readr::read_tsv(file1()$datapath,
                          col_types = readr::cols(.default = readr::col_character()))
      } else if (ext %in% c("xls", "xlsx")) {
        df <-
          readxl::read_excel(file1()$datapath, col_types = 'text')
      }
      
      return(df)
      
    })
    
    observe({
      req(file1())
      rvShip$trigger = TRUE
    })
    
    observe({
      shinyjs::toggle(id = "boxShipments", condition=rvShip$trigger)
    })
    
    output$contents <- DT::renderDataTable({
      df <- df_shipments_upload()
      DT::datatable(
        df,
        rownames = FALSE, 
        options = list(
          autowidth = FALSE,
          scrollX = TRUE,
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        )
      )
    })
    
    return(reactive({df_shipments_upload()}))
    
  })
}

## To be copied in the UI
# mod_shipmentDT_ui("shipmentDT_ui_1")

## To be copied in the server
# mod_shipmentDT_server("shipmentDT_ui_1")
