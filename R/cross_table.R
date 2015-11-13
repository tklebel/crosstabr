#' cross_table
#'
#' build cross table from formula
#'
#' @param data \code{data.frame} which contains the data, given in \code{x}
#' @param x A formula specifying \code{dependent ~ independent}.
#'
#' Strictly speaking there is no dependent or independet variable in
#' cross-tabulation. Nevertheless it is common to calculate percentages, for
#' which the distinction between dependent and independent variable is
#' meaningful. \code{cross_table} lets you specify the model in a memorizable
#' way.
#'
#' @export
cross_table <- function(data, x) {
  assertthat::assert_that(is.data.frame(data), is_formula(x))

  # evaluate formula and create terms object
  forumla <- substitute(x)
  model <- eval(forumla, data) %>% terms

  # build data.frame from terms object.
  model_frame <- model.frame(model)


  tab <- structure(
    list(
      model_frame = model_frame,
      layout = NULL
    ),
    class = "cross_table"
  )
  tab
}
