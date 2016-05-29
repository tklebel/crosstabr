#' Add test statistics to output
#'
#' Currently this function computes a \code{\link{fisher.test}} on the data and
#' displays the resulting output next to the crosstab.
#'
#' @param tab A \code{cross_table} object.
#' @param ... Names of statistical tests to perform on the data, i.e.
#' \code{\link{fisher.test}} or \code{\link{chisq.test}}. The function must
#' accept a \code{table} as input an can be an anonymous function.
#' If no test is supplied, \code{\link[vcd]{assocstats}} is performed as
#' default.
#'
#' @export
#' @examples
#' titanic %>%
#'   crosstab(Survived ~ Sex) %>%
#'   add_stats()
add_stats <- function(tab, ...) {
  base_table <- tab[["model_frame"]] %>%
    table()

  stats <- list(...)

  if (identical(length(stats), 0L)) {
    stats <- list(function(x) summary(vcd::assocstats(x)))
    message("No test statistics supplied, using vcd::assocstats")
  }

  safe_stats <- purrr::map(stats, purrr::safely)
  stats_out <- purrr::invoke_map(safe_stats, list(list(base_table))) %>%
    purrr::transpose()

  errors <- stats_out[["error"]]
  sum_errors <- purrr::map(errors, ~!is.null(.)) %>%
    unlist() %>%
    sum()

  if (sum_errors > 0) {
    errors <- purrr::keep(errors, ~!is.null(.))
    warning("Not all statistics were computed correctly:\n", unlist(errors),
            call. = F)
  }

  stats_out <- stats_out[["result"]]
  stats_out <- purrr::keep(stats_out, ~!is.null(.))

  # setting of names for errors and results is missng
  #names(stats_out) <- as.character(stats)

  tab$statistics <- stats_out
  tab
}

# do some cleaning
prepare_stats <- function(x) {
  if (!is.null(x$statistics)) {

    # update class for custom print method
    stats <- x$statistics %>%
      purrr::at_depth(1, add_class)

    out <- utils::capture.output(
      print(stats)
    )

    out <- paste(out, collapse = "<br>")
    out <- stringr::str_replace_all(out, "\\[\\[[:digit:]\\]\\]", "<br><hr>")
    out <- stringr::str_replace_all(out, "\\\\n", "<br>")
  } else {
    out <- NULL
  }
  out
}


add_class <- function(x) {
  if (class(x) == "htest") {
    attributes(x)$class <- c("htest_custom")
    x
  } else {
    x
  }
}


#' Rewrite print.method for tests to exclude data.
#'
#' @param x The result of a statistical test of dependence (chisq, fisher, ...)
#' @keywords internal
#' @export
print.htest_custom <- function(x, digits = getOption("digits"), prefix = "\t", ...)
{
  cat("\n")
  cat(strwrap(x$method, prefix = prefix), sep = "\n")
  cat("\n")
  out <- character()
  if (!is.null(x$statistic))
    out <- c(out, paste(names(x$statistic), "=", format(signif(x$statistic,
                                                               max(1L, digits - 2L)))))
  if (!is.null(x$parameter))
    out <- c(out, paste(names(x$parameter), "=", format(signif(x$parameter,
                                                               max(1L, digits - 2L)))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits -
                                                3L))
    out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) ==
                                       "<") fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  if (!is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      if (length(x$null.value) == 1L) {
        alt.char <- switch(x$alternative, two.sided = "not equal to",
                           less = "less than", greater = "greater than")
        cat("true ", names(x$null.value), " is ", alt.char,
            " ", x$null.value, "\n", sep = "")
      }
      else {
        cat(x$alternative, "\nnull values:\n", sep = "")
        print(x$null.value, digits = digits, ...)
      }
    }
    else cat(x$alternative, "\n", sep = "")
  }
  if (!is.null(x$conf.int)) {
    cat(format(100 * attr(x$conf.int, "conf.level")), " percent confidence interval:\n",
        " ", paste(format(c(x$conf.int[1L], x$conf.int[2L])),
                   collapse = " "), "\n", sep = "")
  }
  if (!is.null(x$estimate)) {
    cat("sample estimates:\n")
    print(x$estimate, digits = digits, ...)
  }
  cat("\n")
  invisible(x)
}
