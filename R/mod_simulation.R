#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

library(gbp)
library(varhandle)
library(data.table)

mod_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      
    ),
    fluidRow(
      column(
        width = 11,
        shinydashboard::box(
          DT::dataTableOutput(ns("tblRes")),
          actionButton(ns('runsim'), "Run packing simulation"),
          width = NULL,
          title = "Simulation Result",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          background = NULL
        )
      )
    ),
  )
}

#' simulation Server Functions
#'
#' @noRd 
mod_simulation_server <- function(id, box_data, shipment_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    result <- eventReactive(input$runsim, {
      
      bin_df <- box_data() %>%
        dplyr::rename(
          "id" = `Box ID`,
          "l" = `Interior Length`,
          "d" = `Interior Width`,
          "h" = `Interior Height`,
          "w" = `Max Weight`
        ) %>%
        dplyr::mutate(
          id = as.character(id),
          across(c(l, d, h, w), ~ as.numeric(.))
        )
      
      ship_df <- shipment_data$shipments() %>%
        dplyr::rename("oid" = shipment_data$oid()) %>%
        dplyr::rename("sku" = shipment_data$sku()) %>%
        dplyr::rename("l" = shipment_data$dim1()) %>%
        dplyr::rename("d" = shipment_data$dim2()) %>%
        dplyr::rename("h" = shipment_data$dim3()) %>%
        dplyr::rename("w" = shipment_data$weight()) %>%
        dplyr::rename("quantity" = shipment_data$quantity()) %>%
        dplyr::mutate(
          sku = as.character(sku),
          across(c(l, d, h, w, quantity), ~ as.numeric(.))
        ) %>%
        tidyr::uncount(quantity)
      
      if(all(varhandle::check.numeric(ship_df$oid, na.rm = T))){
        ship_df$oid <- as.numeric(ship_df$oid)
        ship_df$oid_og <- ship_df$oid
      } else {
        ship_df <- ship_df %>%
          dplyr::mutate(oid_og = oid, oid = as.numeric(factor(oid)))
      }
      
      oid_df <- ship_df %>%
        dplyr::select(oid, oid_og)
      
      ship_df <- ship_df %>% select(-oid_og)

      it <- data.table::setDT(ship_df)

      bn <- data.table::setDT(bin_df)

      s <- gbp::bpp_solver(it = it, bn = bn)
      
      res <- s$it %>%
        dplyr::select(-otid, -x, -y, -z) %>%
        dplyr::left_join(oid_df, by = "oid") %>%
        dplyr::select(-oid, -l, -d, -h, -w) %>%
        dplyr::relocate(oid_og) %>%
        dplyr::rename(
          "bin_id_within_shipment" = tid,
          "shipment_id" = oid_og,
          "Box ID" = bid
          ) %>%
        mutate(across(where(is.numeric), ~ as.character(.)))

      return(res)
      
    })
    
    output$tblRes <- DT::renderDataTable({
      DT::datatable(
        df <- result(),
        rownames = FALSE,
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        )
      )
    })
    
  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_ui_1")

## To be copied in the server
# mod_simulation_server("simulation_ui_1")
