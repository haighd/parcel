#' boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

library(tippy)
library(dplyr)

mod_boxes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        p(HTML("<b>Length</b>"),
          span(
            shiny::icon("info-circle"), 
            id = ns("info_box_l")
          ),
          numericInput(
            inputId = ns('box_l'), 
            label = NULL,
            value = NULL, 
            min = 1, 
            max = 999,
            step = 0.125
          ),
          tippy::tippy_this(
            elementId = ns("info_box_l"),
            tooltip = "Longest dimension of overpack container, in inches.",
            placement = "right")
        ),
        p(HTML("<b>Width</b>"),
          span(
            shiny::icon("info-circle"), 
            id = ns("info_box_w")
          ),
          numericInput(
            inputId = ns('box_w'), 
            label = NULL,
            value = NULL, 
            min = 1, 
            max = 999,
            step = 0.125
          ),
          tippy::tippy_this(
            elementId = ns("info_box_w"),
            tooltip = "Second longest dimension of container, in inches (can be equal to container's length)",
            placement = "right")
        )
        
      ),
      column(
        width = 6,
        p(HTML("<b>Height</b>"),
          span(
            shiny::icon("info-circle"), 
            id = ns("info_box_h")
          ),
          numericInput(
            inputId = ns('box_h'), 
            label = NULL,
            value = NULL, 
            min = 1, 
            max = 999,
            step = 0.125
          ),
          tippy::tippy_this(
            elementId = ns("info_box_h"),
            tooltip = "Shortest dimension of container, in inches (can be equal to container's length)",
            placement = "right")
        ),
        p(HTML("<b>Maximum Weight</b>"),
          span(
            shiny::icon("info-circle"), 
            id = ns("info_box_wgt")
          ),
          numericInput(
            inputId = ns('box_wgt'), 
            label = NULL,
            value = NULL, 
            min = 1, 
            max = 999,
            step = 0.125
          ),
          tippy::tippy_this(
            elementId = ns("info_box_wgt"),
            tooltip = "Maximum weight capacity of overpack container, in lbs",
            placement = "right")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        p(HTML("<b>Sidewall Thickness</b>"),
          span(
            shiny::icon("info-circle"), 
            id = ns("info_box_t")
          ),
          selectInput(
            inputId = ns('box_t'), 
            label = NULL,
            selected = "C flute (3/16 in)",
            choices = c(
              "C flute (3/16 in)" = 0.1875,
              "A flute (1/4 in)" = 0.25, 
              "B flute (1/8 in)" = 0.125,
              "E flute (1/16 in)" = 0.0625,
              "F flute (1/32 in)" = 0.03125,
              "Do not include sidewall thickness" = 0
            )
          ),
          tippy::tippy_this(
            elementId = ns("info_box_h"),
            tooltip = "Sidewall thickness of container, in inches.  Most common cardboard thickness = C",
            placement = "right")
        )
      )
    ),
    fluidRow(
      column(
        width = 12, 
        actionButton(ns("btn_go"), "Add Overpack Box", icon = icon("box-open", lib = "font-awesome")))
    )
  )
}

#' boxes Server Functions
#'
#' @noRd 
mod_boxes_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    values <- reactiveValues(data = data)
    
    observeEvent(values, {
      values$data <- values$data()
    }, once = TRUE)
    
    observeEvent(input$btn_go, {
      
      if (!is.null(values$data)){values$data <- NULL}
      
      box_thickness <- as.numeric(input$box_t)
      
      box_length <- as.numeric(input$box_l) - (2 * box_thickness)
      box_width <- input$box_w - (2 * box_thickness)
      box_height <- input$box_h - (2 * box_thickness)
      
      box_id <- paste(as.character(input$box_l), as.character(input$box_w), as.character(input$box_h), as.character(input$box_wgt), sep = "_")
      
      values$data <- data.frame(
        Box_id = box_id,
        Length = box_length,
        Width = box_width,
        Height = box_height,
        Weight = input$box_wgt,
        stringsAsFactors = F)
      
      
    })
    
    return(reactive({values$data}))
  })
}


## To be copied in the UI
# mod_boxes_ui("boxes_ui_1")

## To be copied in the server
# mod_boxes_server("boxes_ui_1")
