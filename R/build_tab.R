

build_tab <- function(tab) {
  # guess layout if none is provided
  if (is.null(tab$layout)) {
    message("No layout provided, using layout_column")
    tab <- layout_column(tab)
  }

  if (identical(tab$layout, "column")) {
    tab_out <- table(tab$model_frame)
  } else {
    stop("just column layout supported until now")
  }
  tab_out
}

