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

  # create html_table
  html_table <- capture.output(
    print(htmlTable::htmlTable(tab_out), useViewer = F)
  )

  # pattern to remove inline css-styles
  style_pattern <- "(style).*(?=;).{2}"

  # remove inline css
  html_table <- html_table %>%
    stringr::str_replace_all(style_pattern, "") %>%
    stringr::str_c(collapse = "")


  # prepare output by using htmltools to set up tag with html_table
  html <- htmltools::tags$body(
    htmltools::div(id = "table", htmltools::HTML(html_table))
  )

  # output the table to temporary html_file
  html %>%
    htmltools::browsable() %>%
    htmltools::html_print()
}
