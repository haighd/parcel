#' colnames UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

library(shinyjs)

mod_colnames_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 11,
        shinyjs::hidden(
          div(
            id = ns("boxBucket"),
            shinydashboardPlus::box(
              # id = ns("boxBucket"),
              uiOutput(ns("bucket")),
              width = NULL,
              title = "Assign Column Names",
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

#' colnames Server Functions
#'
#' @noRd 
mod_colnames_server <- function(id, shipments){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rvCol <- reactiveValues()
    
    observeEvent(rvCol, {
      rvCol$trigger = FALSE
    }, once = TRUE)
    
    shipment_data <- shiny::reactive({
      req(shipments())
      df <- shipments()
      return(df)
    })
    
    observe({
      req(shipments())
      rvCol$trigger = TRUE
    })
    
    observe({
      shinyjs::toggle(id = "boxBucket", condition=rvCol$trigger)
    })
    
    
    output$bucket <- renderUI({
      fluidRow(
        column(
          width = 3,
          sortable::rank_list(text = "Supplied Column Names",
                              labels = colnames(shipment_data()), 
                              input_id = ns("default"),
                              options = sortable::sortable_options(group = "my_shared_group")
          )
        ),
        column(
          width = 3,
          sortable::rank_list(text = "Order ID",
                              labels = NULL,
                              input_id = ns("bucket_oid"),
                              options = max_1_item_opts),
          sortable::rank_list(text = "Material ID",
                              labels = NULL,
                              input_id = ns("bucket_sku"),
                              options = max_1_item_opts),
          sortable::rank_list(text = "Material Quantity",
                              labels = NULL,
                              input_id = ns("bucket_quantity"),
                              options = max_1_item_opts)
        ),
        column(
          width = 3,
          sortable::rank_list(text = "Material Length",
                              labels = NULL,
                              input_id = ns("bucket_dims1"),
                              options = max_1_item_opts),
          sortable::rank_list(text = "Material Width",
                              labels = NULL,
                              input_id = ns("bucket_dims2"),
                              options = max_1_item_opts)
        ),
        column(
          width = 3,
          sortable::rank_list(text = "Material Height",
                              labels = NULL,
                              input_id = ns("bucket_dims3"),
                              options = max_1_item_opts),
          sortable::rank_list(text = "Material Weight",
                              labels = NULL,
                              input_id = ns("bucket_weight"),
                              options = max_1_item_opts)
        )
      )
      
    })
    
    return(list(
      shipments = reactive({shipment_data()}),
      weight = reactive({input$bucket_weight}),
      dim1 = reactive({input$bucket_dims1}),
      dim2 = reactive({input$bucket_dims2}),
      dim3 = reactive({input$bucket_dims3}),
      oid = reactive({input$bucket_oid}),
      sku = reactive({input$bucket_sku}),
      quantity = reactive({input$bucket_quantity})
    ))
    
  })
}

## To be copied in the UI
# mod_colnames_ui("colnames_ui_1")

## To be copied in the server
# mod_colnames_server("colnames_ui_1")
