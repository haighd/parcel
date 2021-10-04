#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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