#' dimselector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

library(htmlwidgets)
library(sortable)
library(magrittr)

mod_dimselector_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "panel panel-heading",
      div(
        class = "panel-heading",
        h3("Dragging variables to define a plot")
      ),
      fluidRow(
        class = "panel-body",
        column(
          width = 3,
          tags$div(
            class = "panel panel-default",
            tags$div(class = "panel-heading", "Variables"),
            tags$div(
              class = "panel-body",
              id = ns("sort1"),
              colnames_to_tags(mtcars)
            )
          )
        ),
        column(
          width = 3,
          # analyse as x
          tags$div(
            class = "panel panel-default",
            tags$div(
              class = "panel-heading",
              tags$span(class = "glyphicon glyphicon-stats"),
              "Analyze as x (drag here)"
            ),
            tags$div(
              class = "panel-body",
              id = ns("sort2")
            )
          ),
          # analyse as y
          tags$div(
            class = "panel panel-default",
            tags$div(
              class = "panel-heading",
              tags$span(class = "glyphicon glyphicon-stats"),
              "Analyze as y (drag here)"
            ),
            tags$div(
              class = "panel-body",
              id = ns("sort3")
            )
          )
          
        )
      ),
      sortable_js(
        ns("sort1"),
        options = sortable_options(
          group = list(
            name = "sortGroup1",
            put = TRUE
          ),
          sort = FALSE,
          onSort = sortable_js_capture_input("sort_vars")
        )
      ),
      sortable_js(
        ns("sort2"),
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_x")
        )
      ),
      sortable_js(
        ns("sort3"),
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_y")
        )
      )
    )
  )
}

#' dimselector Server Functions
#'
#' @noRd 
mod_dimselector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$variables <- renderPrint(input[["sort_vars"]])
    output$analyse_x <- renderPrint(input[["sort_x"]])
    output$analyse_y <- renderPrint(input[["sort_y"]])
    
    
    x <- reactive({
      x <- input$sort_x
      if (is.character(x)) x %>% trimws()
    })
    
    y <- reactive({
      input$sort_y %>% trimws()
    })
    
    
  })
}

## To be copied in the UI
# mod_dimselector_ui("dimselector_ui_1")

## To be copied in the server
# mod_dimselector_server("dimselector_ui_1")
