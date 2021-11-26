#' boxes_body UI Function
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

mod_boxes_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 11,
        shinyjs::hidden(
          div(
            id = ns("boxContainers"),
            shinydashboardPlus::box(
              # id = ns("boxContainers"),
              shinyjs::useShinyjs(),
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
    )
  )
}

#' boxes_body Server Functions
#'
#' @noRd 

mod_boxes_body_server <- function(id, box_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    values <- reactiveValues(
      i = NULL, 
      j = NULL,
      df = data.frame()
    )
    
    observeEvent(values,{
      shinyjs::disable(id="removeRow")
      # shinyjs::hide(id = "boxContainers")
      values$df <- box_data()
    }, once = TRUE)
    
    observeEvent(box_data(), {
      new_data <- box_data() |>
        dplyr::rename_with(~ gsub('Box_id', 'Box ID', .x)) |>
        dplyr::rename_with(~ gsub('Length', 'Interior Length', .x)) |>
        dplyr::rename_with(~ gsub('Width', 'Interior Width', .x)) |>
        dplyr::rename_with(~ gsub('Height', 'Interior Height', .x)) |>
        dplyr::rename_with(~ gsub('Weight', 'Max Weight', .x))
      values$df <- dplyr::bind_rows(values$df, new_data)
    })
    
    observeEvent(values$df, {
      if (nrow(values$df > 0)){
        shinyjs::enable(id="removeRow")
      } else {
        shinyjs::disable(id="removeRow")
      }
    })
    
    observeEvent(values$df, {
      shinyjs::show(id = "boxContainers")
    }, once = TRUE)
    
    output$tblBox <- DT::renderDataTable({
      DT::datatable(
        values$df,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE,
          autowidth = FALSE,
          scrollX = TRUE
        )
      )
    })
    
    observeEvent(input$removeRow, {
      
      values$df <- values$df |>
        dplyr::filter(!(dplyr::row_number() %in% input$tblBox_rows_selected))
      
    })
    
    
    reactive({values$df})
    
  })
}

## To be copied in the UI
# mod_boxes_body_ui("boxes_body_ui_1")

## To be copied in the server
# mod_boxes_body_server("boxes_body_ui_1")
