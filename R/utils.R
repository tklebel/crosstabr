#' Is a formula?
#'
#' Checks if input is a formula.
#'
#' @param x An atomic vector.
#' @keywords internal
#' @examples
#' x <- disp ~ am
#' crosstabr:::is_formula(x)
is_twoside_formula <- function(x) {
  inherits(x, "formula") && identical(length(x), 3L)
}


format_freq <- function(x) {
  x <- x*100
  x %>%
    round(digits = 1) %>% # round to one digit after decimal mark
    stringr::str_c("%")
}

