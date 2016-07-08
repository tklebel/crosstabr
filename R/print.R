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

  html_page <- combine_parts(x)

  # output the page to temporary html_file
  html_page %>%
    browsable() %>%
    html_print()

  invisible(x)
}


#' Knit print method for crosstab
#'
#' @keywords internal
#' @export
knit_print.crosstab <- function(x, ...) {

  html_page <- combine_parts(x)
  deps <- htmltools::findDependencies(html_page)

  knitr::asis_output(htmltools::htmlPreserve(as.character(html_page)),
                     meta = deps)
}

# Helper functions -----------

combine_parts <- function(x) {
  # compute table
  tab_out <- build_tab(x)

  # prepare test statistics
  html_tests <- prepare_stats(x)

  # create html table
  html_table <- prepare_table(tab_out)

  # add headings
  html_table <- add_headings(html_table, tab_out, x)

  # create html page
  html_page <- create_page(html_table, html_tests)

  html_page
}


#' HTML parts of page
#'
#' This function currently serves to prepare the pagelayout.
#'
#' Later on this function should be more flexible insofar as the content of the
#' page should be dependent on the content of the table.
#'
#' @param table A bare HTML table, created with \code{htmlTable}.
#' @param stats Character output from a statistical test.
#' @return A \code{tagList} with registered dependencies.
#' @keywords internal
create_page <- function(table, stats) {

  # create link to stylesheet
  style_link <- htmltools::htmlDependency(
    name = "crosstabr",
    version = as.character(utils::packageVersion("crosstabr")),
    src = system.file(package = "crosstabr"),
    stylesheet = "css/crosstabr.css"
  )

  # Create page without statistics
  if (is.null(stats)) {
    html <- tagList(
      tags$body(
        div(id = "tables",
            div(id = "two-way",
                HTML(table)
            )
        )
      )
    )
  } else if (!is.null(stats)) { # Create page with statistics

    style_link$stylesheet <- c(style_link$stylesheet, "css/with_stats.css")

    html <- tagList(
      tags$body(
        div(id = "tables",
            div(id = "two-way",
                HTML(table)
            )
        ),
        div(id = "stats",
            HTML(stats)
        )
      )
    )
  }

  html <- attachDependencies(html, style_link)

  html
}


#' Creates a HTML table
#'
#' @param x A matrix, created by \code{build_tab}.
#' @return A table in HTML format, without inline styling.
#' @keywords internal
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

#' @keywords internal
add_headings <- function(html_table, tab_out, x) {

  # find headings
  vars <- attr(x[["terms_model"]], "factors")
  vars <- attr(vars, "dimnames")[[1]]

  dependent <- vars[1]
  independent <- vars[2]

  # insert heading into original table
  html_table <- stringr::str_replace(html_table, "<th >",
                                     paste0("<th>", dependent))

  # find number of cols and rows (excluding total col)
  dimensions <- dim(tab_out) - 1
  cols <- dimensions[2]
  rows <- dimensions[1]

  top <- paste0("<table id='outer_table'><tbody><tr id='headings'><td></td>",
                "<td id='independent' colspan='",
                cols,
                "'>",
                independent,
                "</td><td></td></tr><tr><td colspan='",
                cols + 2,
                "'>")
  bottom <- "</td></tr></tbody></table>"

  result <- paste(top, html_table, bottom)
  result
}
