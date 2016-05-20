# Clean Titanic data

library(dplyr)

titanic <- Titanic %>%
  as.data.frame() %>%
  reshape::untable(., num = .$Freq) %>%
  as_data_frame() %>%
  select(-Freq)

titanic

devtools::use_data(titanic)
