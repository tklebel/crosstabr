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
#' @examples
#' titanic %>%
#'   crosstab(Survived ~ Sex)
crosstab <- function(data = NULL, x = NULL, layer = NULL) {
  if (!is.data.frame(data)) {
    stop("Data must be a 'data.frame'.")
  }
  if (!is_twoside_formula(x)) {
    stop("'x' must be a two-sided formula.")
  }

  if (!is.null(layer))
    stop("Adding a layer is not implemented yet.")

  # find vars and subset data
  dependent <- lazyeval::f_lhs(x) %>% as.character()
  independent <- lazyeval::f_rhs(x) %>% as.character()

  model_data <- data[c(dependent, independent)]

  tab <- structure(
    list(
      model_data = model_data,
      dependent = dependent,
      independent = independent,
      layout = NULL,
      droplevels = NULL,
      layer = NULL,
      tests = NULL
    ),
    class = "crosstab"
  )
  tab
}

#' A tab-object
#'
#' The \code{tab} is the core of \code{crosstabr}.
#'
#' The \code{tab} is an object of class \code{crosstab}. It contains the
#' formula of the \code{crosstab}, from which a \code{\link{model.frame}} was
#' built, as well as other arguments which determine the apperance of the final
#' cross table.
#'
#' @keywords internal
#' @name tab
NULL
