#' Print method for cross_table
#'
#' The cross tabulation is rendered as a html, which can be viewed in RStudio's
#' viewer pane.
#'
#' @param tab A cross_table object.
#' @keywords internal
#' @method print cross_table
#' @export
print.cross_table <- function(tab) {

  # guess layout if none is provided
  if (is.null(tab$layout)) {
    message("No layout provided, using layout_column")
    tab <- layout_column(tab)
  }

  if (tab$layout == "column") {
    tab_out <- table(tab$model_frame)
  } else {
    stop("just column layout supported until now")
  }


  # create html table
  html_table <- print(xtable::xtable(tab_out), type = "html",
                      html.table.attributes = "",
                      print.results = F)

  # prepare output by using htmltools to set up tag with html_table
  html <- tags$body(
    div(id = "table", HTML(html_table))
  )

  # output the table to temporary html_file
  html %>%
    htmltools::browsable() %>%
    htmltools::html_print()
}
