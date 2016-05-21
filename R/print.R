#' Print method for crosstab
#'
#' The cross tabulation is rendered as a html, which can be viewed in RStudio's
#' viewer pane.
#'
#' @param x A crosstab object.
#' @keywords internal
#' @method print crosstab
#' @export
print.crosstab <- function(x, ...) {

  # compute table
  tab_out <- build_tab(x)

  # create html table
  html_table <- prepare_table(tab_out)

  # create html page
  html_page <- create_page(html_table)

  # output the page to temporary html_file
  html_page %>%
    browsable() %>%
    html_print()
}


#' HTML parts of page
#'
#' This function currently serves to prepare the pagelayout.
#'
#' Later on this function should be more flexible insofar as the content of the
#' page should be dependent on the content of the table.
#'
#' @param x A bare HTML table, created with \code{htmlTable}.
#' @return A \code{tagList} with registered dependencies.
create_page <- function(x) {

  # create link to stylesheet
  style_link <- htmltools::htmlDependency(
    name = "crosstabr",
    version = as.character(utils::packageVersion("crosstabr")),
    src = system.file(package = "crosstabr"),
    stylesheet = "css/crosstabr.css"
  )

  html <- tagList(
    tags$body(
      div(id = "tables",
          div(id = "two-way",
            HTML(x)
          )
        )
    )
  )
  html <- attachDependencies(html, style_link)

  html
}


#' Creates a HTML table
#'
#' @param x A matrix, created by \code{build_tab}.
#' @return A table in HTML format, without inline styling.
prepare_table <- function(x) {
  # create html_table
  x <- utils::capture.output(
    print(htmlTable::htmlTable(x), useViewer = F)
  )

  # pattern to remove inline css-styles
  style_pattern <- "(style).*(?=;).{2}"

  # remove inline css
  x %>%
    stringr::str_replace_all(style_pattern, "") %>%
    stringr::str_c(collapse = "")
}
