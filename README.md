# Parser Combinators for R
<!-- badges: start -->
[![R-CMD-check](https://github.com/SWotherspoon/Combin8R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SWotherspoon/Combin8R/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


A parser combinator package to assist the construction of recursive
descent parsers. A parser is a function that accepts a string as
input, and upon success returns a list containing any unconsumed input
and the result of the parse.  A parser combinator is a higher order
function that combines one or more simple parses to form a more
complex parser.

## Installing

The package is easily installed from GitHub, using the devtools package.

```R
# install.packages("remotes")
remotes::install_github("SWotherspoon/Combin8R")
```

The vignette provides a simple example to get you started.
