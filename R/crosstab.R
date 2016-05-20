#' crosstab
#'
#' build cross table from formula
#'
#' @param data \code{data.frame} which contains the data, given in \code{x}
#' @param x A formula specifying \code{dependent ~ independent}.
#' @param layer A third variable which is used to construct a three-dimensional
#'    crosstab.
#'
#' Strictly speaking there is no dependent or independet variable in
#' cross-tabulation. Nevertheless it is common to calculate percentages, for
#' which the distinction between dependent and independent variable is
#' meaningful. \code{crosstab} lets you specify the model in a memorizable
#' way.
#'
#' @export
crosstab <- function(data = NULL, x = NULL, layer = NULL) {
  assertthat::assert_that(is.data.frame(data), is_formula(x))

  if (!is.null(layer))
    stop("Adding a layer is not implemented yet.")

  # evaluate formula and create terms object
  forumla <- substitute(x)
  model <- eval(forumla, data) %>% terms

  # build data.frame from terms object.
  model_frame <- model.frame(model)


  tab <- structure(
    list(
      terms_model = model,
      model_frame = model_frame,
      layout = NULL,
      layer = NULL
    ),
    class = "crosstab"
  )
  tab
}

#' A tab-object
#'
#' The \code{tab} is the core of \code{crosstabr}.
#'
#' The \code{tab}is an object of class \code{crosstab}. It contains the
#' formula of the \code{crosstab}, from which a \code{\link{model.frame}} was
#' built, as well as other arguments which determine the apperance of the final
#' cross table.
#'
#' @keywords internal
#' @name tab
NULL