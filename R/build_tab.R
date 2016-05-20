#' Build a table
#'
#' Builds a \code{matrix} for use with \code{\link[htmlTable]{htmlTable}}.
#'
#' @param tab A tab object
#'
#' @return Returns a matrix with counts and proportions, seperated by "<br>".
#'    Currently just a single two dimensional table is created.
#'
#' @keywords internal
build_tab <- function(tab) {
  # guess layout if none is provided
  if (is.null(tab$layout)) {
    message("No layout provided, using layout_column")
    tab <- layout_column(tab)
  }

  prop_count <- compute_frequencies(tab)

  # format counts and frequencies accordingly
  prop_count <- prop_count %>%
    purrr::map_at("prop", format_freq)

  # combine counts and proportions into one string
  combinded <- paste(prop_count$Freq, prop_count$prop, sep = "<br>")

  # prepare dimnames for matrix
  levels_dependent <- levels(prop_count[[1]])
  levels_independent <- levels(prop_count[[2]])

  # create matrix from combined counts and proportions
  two_d_table <- matrix(combinded, ncol = length(levels_independent),
                        dimnames = list(
                          as.list(levels_dependent),
                          as.list(levels_independent)
                        ))

  if (identical(tab$layout, "column")) {
    tab_out <- two_d_table
  } else {
    stop("just column layout supported until now")
  }
  tab_out
}


#' Compute counts and frequencies
#'
#' Computes counts and frequencies from a \code{\link{tab}}.
#'
#' Unused factor levels are retained
#'
#' @param x A tab
#'
#' @return A \code{data.frame} with counts and proportions of for dependent and
#' independent variables.
compute_frequencies <- function(x) {
  # find variable names
  model_variables <- x$terms_model %>%
    attr("variables")

  dependent <- model_variables[[2]]
  independent <- model_variables[[3]]

  # calculate frequencies and counts
  prop_count <- stats::xtabs(data = x$model_frame) %>%
    as.data.frame()

  prop_count <- prop_count %>%
    dplyr::group_by_(independent) %>%
    dplyr::mutate_(prop = quote(Freq / sum(Freq))) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::arrange_(independent)
  prop_count
}


