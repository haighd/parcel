#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

max_1_item_opts <- sortable_options(
  swap = TRUE,
  group = list(
    # use a group name to allow sharing between lists
    name = "my_shared_group",
    # add a `put` function that can determine if an element may be placed
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 1;
      }
    ")
  )
)

max_2_item_opts <- sortable_options(
  swap = TRUE,
  group = list(
    # use a group name to allow sharing between lists
    name = "my_shared_group",
    # add a `put` function that can determine if an element may be placed
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 2 child already
        return to.el.children.length < 1;
      }
    ")
  )
)

max_3_item_opts <- sortable_options(
  swap = TRUE,
  group = list(
    # use a group name to allow sharing between lists
    name = "my_shared_group",
    # add a `put` function that can determine if an element may be placed
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 3 child already
        return to.el.children.length < 1;
      }
    ")
  )
)