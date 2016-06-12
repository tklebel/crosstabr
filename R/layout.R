#' Layout the cross_table with row-percentage.
#'
#' @param tab A \code{cross_table} object.
#' @param droplevels Should unused factor levels be omitted when creating the
#'        table? Defaults to keeping the levels.
#'
#' @export
layout_column <- function(tab, droplevels = F) {
  tab$layout <- "column"
  tab$droplevels <- droplevels
  tab
}


