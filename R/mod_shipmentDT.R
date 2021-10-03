#' shipmentDT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_shipmentDT_ui <- function(id){
  ns <- NS(id)
  tagList(
      shinydashboard::box(
        DT::dataTableOutput(
          ns("contents")
        ),
        width = '100%',
        title = "Input Data",
        status = "primary",
        solidHeader = TRUE
    )
  )
}
    
#' shipmentDT Server Functions
#'
#' @noRd 
mod_shipmentDT_server <- function(id, file1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    df_shipments_upload <- shiny::reactive({
      req(file1())
      
      if (stringr::str_detect(file1()$datapath, ".csv$")) {
        df <-
          readr::read_csv(file1()$datapath,
                          col_types = readr::cols(.default = col_character()))
      } else if (stringr::str_detect(file1()$datapath, ".tsv$|.txt$")) {
        df <-
          readr::read_tsv(file1()$datapath,
                          col_types = readr::cols(.default = col_character()))
      } else if (stringr::str_detect(file1()$datapath, ".xlsx?$")) {
        df <-
          readxl::read_excel(file1()$datapath, col_types = 'text')
      }
      
      return(df)
      
    })
    
    output$contents<- DT::renderDataTable({
      df <- df_shipments_upload()
      DT::datatable(
        df,
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        ))
    })
    
  })
}
    
## To be copied in the UI
# mod_shipmentDT_ui("shipmentDT_ui_1")
    
## To be copied in the server
# mod_shipmentDT_server("shipmentDT_ui_1")
