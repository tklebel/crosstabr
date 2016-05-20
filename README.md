[![Travis-CI Build Status](https://travis-ci.org/tklebel/crosstabr.svg?branch=master)](https://travis-ci.org/tklebel/crosstabr)
[![Coverage Status](https://img.shields.io/codecov/c/github/tklebel/crosstabr/master.svg)](https://codecov.io/github/tklebel/crosstabr?branch=master)

# crosstabr
crosstabr is a package to create crosstabs with a simple syntax. Current
implementations like `gmodels:CrossTable` lack a simple and understandable
way of setting up all common options.

The default output for `cross_table` will be HTML, similar to ggvis. A method
when using `cross_table` within RMarkdown will be added.

## Installation
```R
# install.packages("devtools")
devtools::install_github("tklebel/crosstabr")
```

## Basics

```R
data %>% 
  cross_table(dependent_var ~ independent_var) %>% 
  row_layout() %>% 
  add_stats()
```
