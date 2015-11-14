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

  tab_out <- build_tab(tab)


  # create html table
  html_table <- print(xtable::xtable(tab_out), type = "html",
                      html.table.attributes = "",
                      print.results = F)

  # prepare output by using htmltools to set up tag with html_table
  html <- htmltools::tags$body(
    htmltools::div(id = "table", htmltools::HTML(html_table))
  )

  # output the table to temporary html_file
  html %>%
    htmltools::browsable() %>%
    htmltools::html_print()

}
