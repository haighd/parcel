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
library(shinyjs)

mod_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 11,
        shinydashboardPlus::box(
          id = ns("boxSim"),
          DT::dataTableOutput(ns("tblRes")),
          actionButton(ns('runsim'), "Run packing simulation"),
          downloadButton(ns('downloadData'), "Download"),
          width = NULL,
          title = "Simulation Result",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          background = NULL
        )
      )
    )
  )
}

#' simulation Server Functions
#'
#' @noRd 
#' 

library(gbp)

mod_simulation_server <- function(id, box_data, shipment_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rvSim <- reactiveValues()
    
    observeEvent(rvSim, {
      shinyjs::disable("downloadData")
      rvSim$res = data.frame()
      rvSim$cTrigger = FALSE
      rvSim$bin_df = data.frame()
      rvSim$bTrigger = FALSE
      rvSim$dTrigger = FALSE
      rvSim$rTrigger = TRUE
      rvSim$validCols = 0
    }, once = TRUE)
    
    observe({
      req(shipment_data)
      req(box_data())
      
      if(is.list(shipment_data$shipments()) &
         (length(shipment_data$oid()) > 0) &
         (length(shipment_data$sku()) > 0) &
         (length(shipment_data$dim1()) > 0) &
         (length(shipment_data$dim2()) > 0) &
         (length(shipment_data$dim3()) > 0) &
         (length(shipment_data$weight()) > 0) &
         (length(shipment_data$quantity()) > 0)
      ){
        rvSim$validCols <- shipment_data$shipments() %>%
          dplyr::rename("oid" = shipment_data$oid()) %>%
          dplyr::rename("sku" = shipment_data$sku()) %>%
          dplyr::rename("l" = shipment_data$dim1()) %>%
          dplyr::rename("d" = shipment_data$dim2()) %>%
          dplyr::rename("h" = shipment_data$dim3()) %>%
          dplyr::rename("w" = shipment_data$weight()) %>%
          dplyr::rename("quantity" = shipment_data$quantity()) %>%
          summarize(across(c(l, d, h, w, quantity), ~ all(varhandle::check.numeric(., na.rm = T)))) %>%
          rowSums()
      }
      
      print(rvSim$validCols)
      print(rvSim$cTrigger)
      print(rvSim$bTrigger)
      
      if(rvSim$validCols == 5){
        rvSim$cTrigger = TRUE
      }
    })
    
    observe({
      req(box_data())
      print(is.list(box_data()))
      if(is.list(box_data())){
        rvSim$bTrigger = TRUE
      }
    })
    
    observe({
      shinyjs::toggleState(id="runsim", condition=(rvSim$bTrigger & rvSim$cTrigger & rvSim$rTrigger))
      shinyjs::toggle(id="boxSim", condition=(rvSim$bTrigger & rvSim$cTrigger))
      shinyjs::toggleState("downloadData", condition=rvSim$dTrigger)
    })
    
    observeEvent(input$runsim, {
      
      rvSim$rTrigger = FALSE
      
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
          dplyr::across(c(l, d, h, w), ~ as.numeric(.))
        ) %>%
        dplyr::mutate(volume = l * d * h) %>%
        dplyr::arrange(volume) %>%
        dplyr::select(-volume)
      
      ship_df <- shipment_data$shipments() %>%
        dplyr::rename("oid" = shipment_data$oid()) %>%
        dplyr::rename("sku" = shipment_data$sku()) %>%
        dplyr::rename("l" = shipment_data$dim1()) %>%
        dplyr::rename("d" = shipment_data$dim2()) %>%
        dplyr::rename("h" = shipment_data$dim3()) %>%
        dplyr::rename("w" = shipment_data$weight()) %>%
        dplyr::rename("quantity" = shipment_data$quantity()) %>%
        select(oid, sku, l, d, h, w, quantity) %>%
        dplyr::mutate(
          sku = as.character(sku),
          dplyr::across(c(l, d, h, w, quantity), ~ as.numeric(.))
        ) %>%
        tidyr::uncount(quantity) %>%
        tibble::rowid_to_column("row_id") %>%
        mutate(rowid = factor(row_id)) %>%
        tidyr::pivot_longer(cols = c(l, d, h), names_to="name", values_to="value") %>%
        group_by(row_id) %>%
        arrange(desc(value)) %>%
        mutate(rank = as.character(dplyr::row_number())) %>%
        ungroup() %>%
        mutate(name = recode(rank, '1' = 'l', '2' = 'd', '3' = 'h')) %>%
        select(-row_id, -rank) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        tidyr::drop_na()
        
      
      if(all(varhandle::check.numeric(ship_df$oid, na.rm = T))){
        ship_df$oid <- as.numeric(ship_df$oid)
        ship_df$oid_og <- ship_df$oid
      } else {
        ship_df <- ship_df %>%
          dplyr::mutate(oid_og = oid, oid = as.numeric(factor(oid)))
      }
      
      it <- data.table::setDT(ship_df %>% dplyr::select(-oid_og))
      
      bn <- data.table::setDT(bin_df)
      
      s <- gbp::bpp_solver(it = it, bn = bn)
      
      rvSim$res <- s$it %>%
        dplyr::left_join(
          ship_df %>%
            dplyr::select(oid, oid_og) %>%
            distinct(), by = "oid") %>%
        dplyr::select(-oid, -otid, -l, -d, -h, -w) %>%
        dplyr::relocate(oid_og) %>%
        dplyr::rename(
          "bin_id_within_shipment" = tid,
          "shipment_id" = oid_og,
          "Box ID" = bid
        ) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), ~ as.character(.)))
      
      rvSim$rTrigger = TRUE
      rvSim$dTrigger = TRUE
      
    })
    
    output$tblRes <- DT::renderDataTable({
      DT::datatable(
        # df <- result(),
        df <- rvSim$res,
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
    
    output$downloadData <- downloadHandler(
      filename = 'sim_results.csv',
      content = function(file){
        write_csv(rvSim$res, file)
      }
    )
  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_ui_1")

## To be copied in the server
# mod_simulation_server("simulation_ui_1")
