#' boxes_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boxes_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 11,
        shinydashboardPlus::box(
          DT::dataTableOutput(ns("tblBox")),
          actionButton(ns("removeRow"), "Deleted Selected Container"),
          width = NULL,
          title = "Overpack Containers",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          background = NULL
        )
      )
    )
  )
}

#' boxes_body Server Functions
#'
#' @noRd 
mod_boxes_body_server <- function(id, box_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    idx <- reactiveValues(idx = 0)
    
    data <- shiny::reactive({
      req(box_data())
      df <- box_data() %>%
        dplyr::rename_with(~ gsub('Box_id', 'Box ID', .x)) %>%
        dplyr::rename_with(~ gsub('Length', 'Interior Length', .x)) %>%
        dplyr::rename_with(~ gsub('Width', 'Interior Width', .x)) %>%
        dplyr::rename_with(~ gsub('Height', 'Interior Height', .x)) %>%
        dplyr::rename_with(~ gsub('Weight', 'Max Weight', .x)) %>%
        dplyr::filter(!(row_number() %in% idx$idx))
      return(df)
    })
    
    output$tblBox <- DT::renderDataTable({
      DT::datatable(
        df <- data(),
        rownames = FALSE,
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        )
      )
    })
    
    observeEvent(input$removeRow, {
      
      idx$idx <- c(idx$idx, input$tblBox_rows_selected)
      
    })
    
    reactive(data())
    
  })
}

## To be copied in the UI
# mod_boxes_body_ui("boxes_body_ui_1")

## To be copied in the server
# mod_boxes_body_server("boxes_body_ui_1")
