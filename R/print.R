#' Print method for cross_table
#'
#' The cross tabulation is rendered as a html, which can be viewed in RStudio's
#' viewer pane.
#'
#' @param tab A cross_table object.
#' @keywords internal
#' @method print cross_table
#' @export
print.cross_table <- function(tab, ...) {

  tab_out <- build_tab(tab)

  # create html_table
  html_table <- utils::capture.output(
    print(htmlTable::htmlTable(tab_out), useViewer = F)
  )

  # pattern to remove inline css-styles
  style_pattern <- "(style).*(?=;).{2}"

  # remove inline css
  html_table <- html_table %>%
    stringr::str_replace_all(style_pattern, "") %>%
    stringr::str_c(collapse = "")


  # prepare output by using htmltools to set up tag with html_table
  html <- create_page(html_table)

  # output the table to temporary html_file
  html %>%
    htmltools::browsable() %>%
    htmltools::html_print()
}


#' HTML parts of page
#'
#' This function currently serves to prepare the pagelayout.
#'
#' Later on this function should be more flexible insofar as the content of the
#' page should be dependent on the content of the table.
#'
#' @param html_table a bare HTML table, created with \code{htmlTable}.
#' @return a \code{tagList} with registered dependencies.
create_page <- function(html_table) {

  # create link to stylesheet
  style_link <- htmltools::htmlDependency(
    name = "crosstabr",
    version = as.character(packageVersion("crosstabr")),
    src = system.file(package = "crosstabr"),
    stylesheet = "css/crosstabr.css"
  )

  html <- htmltools::tagList(
    htmltools::tags$body(
      htmltools::h1(id = "title", "Crosstabr"),
      htmltools::div(id = "tables",
          htmltools::div(id = "two-way",
            htmltools::HTML(html_table)
          )
        )
    )
  )
  html <- htmltools::attachDependencies(html, style_link)

  html
}
