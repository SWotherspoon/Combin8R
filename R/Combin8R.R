##' Format an abstract syntax tree as a string
##'
##' @title Format an Abstract Syntax Tree
##' @param x The result of a parser.
##' @param indent The initial indentation level.
##' @return A string representation of the abstract syntax tree
##' @export
formatAST <- function(x,indent=0) {
  UseMethod("formatAST")
}

formatAST.default <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,deparse(x),sep="")
}


##' Create a parser to parse text matching a regular expression.
##'
##' This function creates a parser that accepts input that matches a
##' regular expression.
##'
##' The regex defining a match will usually begin with the "^"
##' beginning of string anchor.
##'
##' The \code{cls} argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse, given the first row of the character
##' matrix returned by \code{stri_match_first_regex}.
##'
##' A parser is a function that accepts a string as input and returns
##' \code{NULL} on failure or a list with elements
##' \tabular{ll}{
##' \code{input} \tab the unconsumed input \cr
##' \code{result} \tab an object representing the result of the parse \cr
##' }
##' if the input satisfies the parser.
##'
##'
##'
##' @title Create a Primitive Parser from a Regex
##' @param cls a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param regex A regular expression that defines the text to be parsed.
##' @return Returns a parser.
##' @importFrom stringi stri_match_first_regex stri_locate_first_regex
##' @export
pRegex <- function(cls,regex) {
  result <- switch(class(cls),
                   "character"=cls,
                   "function"=function(value) structure(list(value=value),class=c(cls,"pRegex")),
                   stop("pRegex: cls should be a class name or a function"))

  ## Parser
  function(input) {
    match <- stri_match_first_regex(input,regex)
    ## If the regex matched, consume input and return any captured
    ## groups as values
    if(!is.na(match[1,1]))
      list(input=substr(input,stri_locate_first_regex(input,regex)[1,2]+1,nchar(input)),
           result=result(match[1,]))
  }
}

formatAST.pRegex <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"[",class(x)[1],"; ",paste(x$value,collapse=" "),"]\n",sep="")
}

print.pRegex <- function(x,indent=0,...)
  cat(formatAST(x,indent))


##' Creates a parser that accepts input that is accepted by any one of
##' a number of simpler parsers.
##'
##' The \code{cls} argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the result of the first alternative
##' parser satisfied by the input.
##'
##' @title "Alt" Parser Combinator
##' @param cls a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param ... Alternative simpler parsers.
##' @return Returns a parser.
##' @export
pAlt <- function(cls,...) {
  ps <- list(...)
  result <- switch(class(cls),
                   "character"=cls,
                   "function"=function(value) structure(list(value=value),class=c(cls,"pAlt")),
                   stop("pAlt: cls should be a class name or a function"))
  ## Parser
  function(input) {
    ## Try each parser in turn
    for(p in ps) {
      ## Return first successful parse
      if(!is.null(parse <- p(input)))
        return(list(input=parse$input,result=result(parse$result)))
    }
    ## All parses failed
    NULL
  }
}

formatAST.pAlt <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"||",class(x)[1],"; ",formatAST(x$value),sep="")
}

print.pAlt <- function(x,indent=0,...)
  cat(formatAST(x,indent))


##' Create a parser that succeeds on input accepted by a sequence of
##' simpler parsers applied in succession.
##'
##' The \code{cls} argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parsers.
##'
##' @title "Seq" Parser Combinator
##' @param cls a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param ... A sequence of parsers to matched in sequence.
##' @return Return a parser.
##' @export
pSeq <- function(cls,...) {
  ps <- list(...)
  result <- switch(class(cls),
                   "character"=cls,
                   "function"=function(value) structure(list(value=value),class=c(cls,"pSeq")),
                   stop("pSeq: cls should be a class name or a function"))
  ## Parser
  function(input) {
    ## Try each parser in sequence, accumulating parse results
    values <- vector(mode="list",length(ps))
    for(k in seq_along(ps)) {
      parse <- ps[[k]](input)
      ## If any match fails, the sequence fails
      if(is.null(parse)) return(NULL)
      values[[k]] <- parse$result
      input <- parse$input
    }
    list(input=input,result=result(values))
  }
}

formatAST.pSeq <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"(",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,")",sep="",collapse="")
}

print.pSeq <- function(x,indent=0,...)
  cat(formatAST(x,indent))

##' Create a parser that succeeds on input for which a simpler parser
##' succeeds zero or more times in succession.
##'
##' The \code{cls} argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parser.
##'
##' @title "Many" Parser Combinator
##' @param cls a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param p A parser to match zero or more times.
##' @return Return a parser.
##' @export
pMany <- function(cls,p) {
  result <- switch(class(cls),
                   "character"=cls,
                   "function"=function(value) structure(list(value=value),class=c(cls,"pMany")),
                   stop("pMany: cls should be a class name or a function"))
  ## Parser
  function(input) {
    k <- 0
    values <- list()
    ## Repeatedly try parser, accumulating parse results
    repeat {
      parse <- p(input)
      ## Finish when a parse fails
      if(is.null(parse)) break
      values[[k <- k+1]] <- parse$result
      input <- parse$input
    }
    list(input=input,result=result(values))
  }
}

formatAST.pMany <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"*{",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,"}",sep="")
}

print.pMany <- function(x,indent=0,...)
  cat(formatAST(x,indent))



##' Create a parser that succeeds on input for which a simpler parser
##' succeeds one or more times in succession.
##'
##' The \code{cls} argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parser.
##'
##' @title "Some" Parser Combinator
##' @param cls a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param p A parser to match zero or more times.
##' @return Return a parser.
##' @export
pSome <- function(cls,p) {
  result <- switch(class(cls),
                   "character"=cls,
                   "function"=function(value) structure(list(value=value),class=c(cls,"pSome")),
                   stop("pSome: cls should be a class name or a function"))
  ## Parser
  function(input) {
    k <- 0
    values <- list()
    parse <- p(input)
    if(is.null(parse)) return(NULL)
    values[[k <- k+1]] <- parse$result
    input <- parse$input
    ## Continue trying parser, accumulating parse results
    repeat {
      parse <- p(input)
      ## Finish when a parse fails
      if(is.null(parse)) break
      values[[k <- k+1]] <- parse$result
      input <- parse$input
    }
    list(input=input,
         result=structure(list(value=values),class=c(cls,"pSome")))
  }
}

formatAST.pSome <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"+{",class(x)[1],";\n",
      paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
      pad,"}",sep="")
}

print.pSome <- function(x,indent=0,...)
  cat(formatAST(x,indent))
