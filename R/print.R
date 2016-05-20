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

  tab_out <- build_tab(x)

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
#' @param html_table a bare HTML table, created with \code{htmlTable}.
#' @return a \code{tagList} with registered dependencies.
create_page <- function(html_table) {

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
            HTML(html_table)
          )
        )
    )
  )
  html <- attachDependencies(html, style_link)

  html
}
