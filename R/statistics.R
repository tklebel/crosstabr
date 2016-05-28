#' Add test statistics to output
#'
#' Currently this function computes a \code{\link{fisher.test}} on the data and
#' displays the resulting output next to the crosstab.
#'
#' @param tab A \code{cross_table} object.
#' @param ... Further arguments, currently passed down to
#'    \code{\link{fisher.test}}.
#'
#' @export
#' @examples
#' titanic %>%
#'   crosstab(Survived ~ Sex) %>%
#'   add_stats()
add_stats <- function(tab, ...) {
  fisher_result <- tab[["model_frame"]] %>%
    table() %>%
    fisher.test(...)

  tab$statistics <- fisher_result
  tab
}


prepare_stats <- function(x) {
  if (!is.null(x$statistics)) {
    out <- utils::capture.output(
      print(x$statistics)
    )
    out <- paste(out, collapse = "<br>")
  } else {
    out <- NULL
  }
  out
}
