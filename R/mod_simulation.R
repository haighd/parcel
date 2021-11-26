#' simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rlang .data
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
        shinyjs::hidden(
          div(
            id = ns("boxSim"),
            shinydashboardPlus::box(
              # id = ns("boxSim"),
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
    )
  )
}

#' simulation Server Functions
#'
#' @noRd 
#' 

library(gbp)
library(tibble)
library(tidyr)
library(dplyr)
library(tidyselect)

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
        rvSim$validCols <- shipment_data$shipments() |>
          dplyr::rename("oid" = shipment_data$oid()) |>
          dplyr::rename("sku" = shipment_data$sku()) |>
          dplyr::rename("l" = shipment_data$dim1()) |>
          dplyr::rename("d" = shipment_data$dim2()) |>
          dplyr::rename("h" = shipment_data$dim3()) |>
          dplyr::rename("w" = shipment_data$weight()) |>
          dplyr::rename("quantity" = shipment_data$quantity()) |>
          dplyr::summarize(dplyr::across(c(.data$`l`, .data$`d`, .data$`h`, .data$`w`, .data$`quantity`), ~ all(varhandle::check.numeric(., na.rm = T)))) |>
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
      
      bin_df <- box_data() |>
        dplyr::rename(
          "id" = .data$`Box ID`,
          "l" = .data$`Interior Length`,
          "d" = .data$`Interior Width`,
          "h" = .data$`Interior Height`,
          "w" = .data$`Max Weight`
        ) |>
        dplyr::mutate(
          id = as.character(.data$`id`),
          dplyr::across(c(.data$`l`, .data$`d`, .data$`h`, .data$`w`), ~ as.numeric(.))
        ) |>
        dplyr::mutate(`volume` = .data$`l` * .data$`d` * .data$`h`) |>
        dplyr::arrange(.data$`volume`) |>
        dplyr::select(-.data$`volume`)
      
      ship_df <- shipment_data$shipments() |>
        dplyr::rename("oid" = shipment_data$oid()) |>
        dplyr::rename("sku" = shipment_data$sku()) |>
        dplyr::rename("l" = shipment_data$dim1()) |>
        dplyr::rename("d" = shipment_data$dim2()) |>
        dplyr::rename("h" = shipment_data$dim3()) |>
        dplyr::rename("w" = shipment_data$weight()) |>
        dplyr::rename("quantity" = shipment_data$quantity()) |>
        dplyr::select(.data$`oid`, .data$`sku`, .data$`l`, .data$`d`, .data$`h`, .data$`w`, .data$`quantity`) |>
        dplyr::mutate(
          sku = as.character(.data$`sku`),
          dplyr::across(c(.data$`l`, .data$`d`, .data$`h`, .data$`w`, .data$`quantity`), ~ as.numeric(.))
        ) |>
        tidyr::uncount(.data$`quantity`) |>
        tibble::rowid_to_column("row_id") |>
        dplyr::mutate(rowid = factor(.data$`row_id`)) |>
        tidyr::pivot_longer(cols = c(.data$`l`, .data$`d`, .data$`h`), names_to="name", values_to="value") |>
        dplyr::group_by(.data$`row_id`) |>
        dplyr::arrange(dplyr::desc(.data$`value`)) |>
        dplyr::mutate(`rank` = as.character(dplyr::row_number())) |>
        dplyr::ungroup() |>
        dplyr::mutate(`name` = dplyr::recode(.data$`rank`, '1' = 'l', '2' = 'd', '3' = 'h')) |>
        dplyr::select(-.data$row_id, -.data$rank) |>
        tidyr::pivot_wider(names_from = .data$`name`, values_from = .data$`value`) |>
        tidyr::drop_na()
      
      
      if(all(varhandle::check.numeric(ship_df$`oid`, na.rm = T))){
        ship_df$`oid` <- as.numeric(ship_df$`oid`)
        ship_df$`oid_og` <- ship_df$`oid`
      } else {
        ship_df <- ship_df |>
          dplyr::mutate(`oid_og` = .data$`oid`, `oid` = as.numeric(factor(.data$`oid`)))
      }
      
      it <- data.table::setDT(ship_df |> dplyr::select(-.data$oid_og))
      
      bn <- data.table::setDT(bin_df)
      
      s <- gbp::bpp_solver(it = it, bn = bn)
      
      rvSim$res <- s$it |>
        dplyr::left_join(
          ship_df |>
            dplyr::select(.data$`oid`, .data$`oid_og`) |>
            dplyr::distinct(), by = "oid") |>
        dplyr::select(-.data$`oid`, -.data$`otid`, -.data$`l`, -.data$`d`, -.data$`h`, -.data$`w`) |>
        dplyr::relocate(.data$`oid_og`) |>
        dplyr::rename(
          "bin_id_within_shipment" = .data$`tid`,
          "shipment_id" = .data$`oid_og`,
          "Box ID" = .data$`bid`
        ) |>
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
        readr::write_csv(rvSim$res, file)
      }
    )
  })
}

## To be copied in the UI
# mod_simulation_ui("simulation_ui_1")

## To be copied in the server
# mod_simulation_server("simulation_ui_1")
