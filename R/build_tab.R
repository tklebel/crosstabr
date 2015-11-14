

build_tab <- function(tab) {
  # guess layout if none is provided
  if (is.null(tab$layout)) {
    message("No layout provided, using layout_column")
    tab <- layout_column(tab)
  }

  # find variable names
  model_variables <- tab$terms_model %>%
    attr("variables")

  dependent <- model_variables[[2]]
  independent <- model_variables[[3]]

  # calcualte frequencies and counts
  prop_count <- tab$model_frame %>%
    dplyr::group_by_(dependent, independent) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by_(independent) %>%
    dplyr::mutate(freq = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(independent)

  # format counts and frequencies accordingly
  prop_count <- prop_count %>%
    purrr::map_at("freq", format_freq) %>%
    purrr::map_at("n", ~paste(.x, "")) # add one WS for correct alignment

  # combine counts and freqs into one string
  combinded <- paste(prop_count$n, prop_count$freq, sep = "<br>")

  # prepare dimnames for matrix
  levels_dependent <- levels(prop_count[[1]]) # check, if this is always valid
  levels_independent <- levels(prop_count[[2]])

  # create matrix from combined counts and freqs
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

