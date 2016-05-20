#' Layout the cross_table with row-percentage.
#'
#' @param tab A \code{cross_table} object.
#'
#' @export
layout_column <- function(tab) {
  tab$layout <- "column"
  tab
}


