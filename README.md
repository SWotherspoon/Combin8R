# Parser Combinators for R

A parser combinator package to assist the construction of recursive
descent parsers. A parser is a function that accepts a string as
input, and upon success returns a list containing any unconsumed input
and the result of the parse.  A parser combinator is a higher order
function that combines one or more simple parses to form a more
complex parser.

## Installing

The package is easily installed from GitHub, using the devtools package.

```R
devtools::install_github("SWotherspoon/Combin8R")
```

If you don't have `devtools` installed already, install it first.

```R
install.packages("devtools")
```

Combin8R otherwise does not need devtools for normal use.



