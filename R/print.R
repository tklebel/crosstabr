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

  # prepare test statistics
  html_tests <- prepare_stats(x)

  # create html table
  html_table <- prepare_table(tab_out)

  # create html page
  html_page <- create_page(html_table, html_tests)

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
  # compute table
  tab_out <- crosstabr:::build_tab(x)

  # prepare test statistics
  html_tests <- crosstabr:::prepare_stats(x)

  # create html table
  html_table <- crosstabr:::prepare_table(tab_out)

  html_page <- create_page(html_table, html_tests)


  #out <- knitr::asis_output(html_page)
  style_link <- htmltools::htmlDependency(
    name = "crosstabr",
    version = as.character(utils::packageVersion("crosstabr")),
    src = system.file(package = "crosstabr"),
    stylesheet = "css/crosstabr.css"
  )

  out <- htmltools::attachDependencies(
    htmltools::tagList(
      HTML(html_table)
    ), style_link
  )
  knitr::asis_output(out)

  invisible(x)
}

# Helper functions -----------

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
