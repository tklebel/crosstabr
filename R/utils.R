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
    str_c("%")
}
