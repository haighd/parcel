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
    fluidRow(
      column(
        width = 11,
        shinydashboard::box(
          DT::dataTableOutput(ns("contents")),
          width = '100%',
          title = "Input Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        shinydashboard::box(
          uiOutput(ns("bucket")),
          width = '100%',
          title = 'Assign Column Names',
          status = "primary",
          color = "blue",
          solidheader = TRUE,
          collapsible = TRUE
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
    
    df_shipments_upload <- shiny::reactive({
      req(file1())
      ext <- tools::file_ext(file1()$datapath)
      
      validate(need(ext %in% c("csv", "tsv", "xls", "xlsx"), "Please upload a csv, tsv/txt, xls, or xlsx file"))
      
      
      if (ext == "csv") {
        df <-
          readr::read_csv(file1()$datapath,
                          col_types = readr::cols(.default = col_character()))
      } else if (ext %in% c("tsv", "txt")) {
        df <-
          readr::read_tsv(file1()$datapath,
                          col_types = readr::cols(.default = col_character()))
      } else if (ext %in% c("xls", "xlsx")) {
        df <-
          readxl::read_excel(file1()$datapath, col_types = 'text')
      }
      
      return(df)
      
    })
    
    output$contents <- DT::renderDataTable({
      df <- df_shipments_upload()
      DT::datatable(
        df,
        rownames = FALSE, 
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        )
      )
    })
    
    output$bucket <- renderUI({
      fluidRow(
        column(
          width = 3,
          rank_list(text = "Input Column Names",
                    labels = colnames(df_shipments_upload()), 
                    input_id = "default",
                    options = sortable_options(group = "my_shared_group")
          )
        ),
        column(
          width = 3,
          rank_list(text = "Order ID",
                    labels = NULL,
                    input_id = "bucket_oid",
                    options = max_1_item_opts),
          rank_list(text = "Material ID",
                    labels = NULL,
                    input_id = "bucket_sku",
                    options = max_1_item_opts)
        ),
        column(
          width = 3,
          rank_list(text = "Material Length",
                    labels = NULL,
                    input_id = "bucket_dims1",
                    options = max_1_item_opts),
          rank_list(text = "Material Width",
                    labels = NULL,
                    input_id = "bucket_dims2",
                    options = max_1_item_opts)
        ),
        column(
          width = 3,
          rank_list(text = "Material Depth",
                    labels = NULL,
                    input_id = "bucket_dims3",
                    options = max_1_item_opts),
          rank_list(text = "Material Weight",
                    labels = NULL,
                    input_id = ns("bucket_weight"),
                    options = max_1_item_opts)
        )
      )
      
    })
    return(list(shipments = reactive({df_shipments_upload()}), weight_col = reactive({input$bucket_weight})))
  })
}

## To be copied in the UI
# mod_shipmentDT_ui("shipmentDT_ui_1")

## To be copied in the server
# mod_shipmentDT_server("shipmentDT_ui_1")
