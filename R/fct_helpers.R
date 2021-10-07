#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(readr)

colnames_to_tags <- function(x){
  
  lapply(
    colnames(x),
    function(co) {
      tag(
        "p",
        list(
          class = "character",
          draggable = TRUE,
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
  
}

default_cols <- readr::cols(.default = readr::col_character())

file_csv <- function(path){
  return(readr::read_csv(path, col_types = default_cols))
}

file_tsv <- function(path){
  return(readr::read_tsv(path, col_types = default_cols))
}