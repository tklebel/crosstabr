#' Is a formula?
#'
#' @export
#' @examples
#' x <- disp ~ am
#' is_formula(x)
is_formula <- function(x) inherits(x, "formula")


format_freq <- function(x) {
  x <- x*100
  x %>%
    round(digits = 1) %>% # round to one digit after decimal mark
    stringr::str_c("%")
}

#' Expand factor
#'
#' \code{expand_factor} expands a factor to include all levels, even those,
#' which were not observed in the data. It returns a \code{data.frame} which
#' can subsequently used to condunct a \code{left_join}.
#'
#' @param data A \code{data.frame}
#' @param x a quoted name of the variable within the \code{data.frame}, which
#'    should be expanded
#' @return A \code{data.frame} with one named variable, containing all levels
#'    of x.
expand_factor <- function(data, x) {
  bare_name <- as.name(x)
  levels_list <- with(data, list(x = levels(eval(bare_name))))
  names(levels_list) <- x
  expanded <- with(data, expand.grid(levels_list))
  expanded
}

