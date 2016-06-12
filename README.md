[![Travis-CI Build Status](https://travis-ci.org/tklebel/crosstabr.svg?branch=master)](https://travis-ci.org/tklebel/crosstabr)
[![Coverage Status](https://img.shields.io/codecov/c/github/tklebel/crosstabr/master.svg)](https://codecov.io/github/tklebel/crosstabr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/crosstabr)](https://cran.r-project.org/package=crosstabr)

# crosstabr
crosstabr is a package to create crosstabs with a simple syntax. Current
implementations like `gmodels::CrossTable` lack a simple and understandable
way of setting up all common options and deliver unusable output if there are
many columns.


The default output for `crosstab` is HTML, similar to
[ggvis](https://github.com/rstudio/ggvis). The resulting `crosstable` can be
viewed within RStudio's viewer pane, or can be displayed in a separate browser
window.

Currently `crosstab` lacks compatibility with RMarkdown, which will be added in
the future.

## Installation
```R
# install.packages("devtools")
devtools::install_github("tklebel/crosstabr")
```

## Basic Usage
The package builds upon the idea, that when investigating categorial data, in
most cases there will be some sort of dependent variable: a variable the
researcher is interested. On the other hand there will be one or more independent
variables, where the goal is to determine, if there is a difference in the
DV regarding to different outcomes of the IV's. Usually one would calculate
percentages on rows or columns, depending on if the DV is in the rows or cols.

`crosstabr`s syntax is similar to linear modeling: you specify a model via a
formula, with the DV on the left-hand-side, the IV on the right-hand-side.
For example, we could be interested whether females or males were more likely to
survive the sinking of the Titanic. We would therefore place the DV (Survived)
on the left-hand-side, the IV (Sex) on the right-hand-side:

```R
titanic %>% 
  crosstab(Survived ~ Sex)
```

If no layout is provided `crosstabr` defaults to `layout_column`, where the IV
is placed in the columns. We could however be explicit about the layout:

```R
titanic %>% 
  crosstab(Survived ~ Sex) %>% 
  layout_column()
```

Currently `layout_row()` is not yet implemented, but it is planned for a future
release.

## Statistical Tests
In many cases a simple table won't be enough; possibly one wants to conduct
statistical tests on the table. This is fairly easy:

```R
titanic %>% 
  crosstab(Survived ~ Sex) %>% 
  add_stats()
```

When no specific tests are supplied, `add_stats()` defaults to
`vcd::assocstats()` which yields the result of a `chisq.test`, the likelihood
ratio and three coefficients:

- Phi-coefficient
- Contingency coefficient
- Cramer's V

If desired, other tests can be supplied:

```R
titanic %>% 
  crosstab(Survived ~ Sex) %>% 
  add_stats(fisher.test)
```

It is possible to supply several functions, as well as anonymous functions in
order to add arguments:

```R
titanic %>% 
  crosstab(Survived ~ Sex) %>% 
  add_stats(chisq.test, function(x) fisher.test(x, alternative = "greater"))
```

