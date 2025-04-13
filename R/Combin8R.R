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

##' @export
formatAST.default <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,deparse(x),sep="")
}


##' Create a parser to parse text matching a literal string.
##'
##' This function creates a parser that accepts input that matches a
##' given string.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse, given the matched literal string.
##'
##' @title Create a Primitive Parser from a Regex
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param string The string literal to match.
##' @return Returns a parser.
##' @export
pLiteral <- function(tag,string=tag) {
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pLiteral"))
  ## Parser
  function(input) {
    ## If the string is matched, consume input and return the string
    if(string==substr(input,0,nchar(string)))
      list(input=substr(input,nchar(string)+1,nchar(input)),
           result=result(string))
  }
}

##' @export
formatAST.pLiteral <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"[",class(x)[1],"; ",x$value,"]\n",sep="")
}

##' @export
print.pLiteral <- function(x,indent=0,...)
  cat(formatAST(x,indent))


##' Create a parser to parse text matching a regular expression.
##'
##' This function creates a parser that accepts input that matches a
##' regular expression.
##'
##' The regex defining a match will usually begin with the "^"
##' beginning of string anchor.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse, given the first row of the character
##' matrix returned by \code{stri_match_first_regex}.
##'
##' @title Create a Primitive Parser from a Regex
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param regex A regular expression that defines the text to be parsed.
##' @return Returns a parser.
##' @importFrom stringi stri_match_first_regex stri_locate_first_regex
##' @export
pRegex <- function(tag,regex) {
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pRegex"))

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

##' @export
formatAST.pRegex <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"[",class(x)[1],"; ",paste(x$value,collapse=","),"]\n",sep="")
}

##' @export
print.pRegex <- function(x,indent=0,...)
  cat(formatAST(x,indent))


##' Creates a parser that accepts input that is accepted by any one of
##' a number of simpler parsers.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the result of the first alternative
##' parser satisfied by the input.
##'
##' @title "Alt" Parser Combinator
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param ... Alternative simpler parsers.
##' @return Returns a parser.
##' @export
pAlt <- function(tag,...) {
  ps <- list(...)
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pAlt"))
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

##' @export
formatAST.pAlt <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"||",class(x)[1],"; ",formatAST(x$value),sep="")
}

##' @export
print.pAlt <- function(x,indent=0,...)
  cat(formatAST(x,indent))


##' Create a parser that succeeds on input accepted by a sequence of
##' simpler parsers applied in succession.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parsers.
##'
##' @title "Seq" Parser Combinator
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param ... A sequence of parsers to matched in sequence.
##' @return Return a parser.
##' @export
pSeq <- function(tag,...) {
  ps <- list(...)
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pSeq"))
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

##' @export
formatAST.pSeq <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"(",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,")",sep="",collapse="")
}

##' @export
print.pSeq <- function(x,indent=0,...)
  cat(formatAST(x,indent))

##' Create a parser that succeeds on input for which a simpler parser
##' succeeds zero or more times in succession.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parser.
##'
##' @title "Many" Parser Combinator
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param p A parser to match zero or more times.
##' @return Return a parser.
##' @export
pMany <- function(tag,p) {
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pMany"))
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

##' @export
formatAST.pMany <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"*{",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,"}",sep="")
}

##' @export
print.pMany <- function(x,indent=0,...)
  cat(formatAST(x,indent))



##' Create a parser that succeeds on input for which a simpler parser
##' succeeds one or more times in succession.
##'
##' The `tag` argument should either be a string naming the class
##' of the result, or a function that constructs a object representing
##' the result of the parse given the list of results of the
##' constituent parser.
##'
##' @title "Some" Parser Combinator
##' @param tag a class name or a function of a single argument that
##'   constructs an object representing the result of the parse.
##' @param p A parser to match zero or more times.
##' @return Return a parser.
##' @export
pSome <- function(tag,p) {
  result <- if(is.function(tag)) tag else function(value) structure(list(value=value),class=c(tag,"pSome"))

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
    list(input=input,result=result(values))
  }
}

##' @export
formatAST.pSome <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"+{",class(x)[1],";\n",
      paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
      pad,"}",sep="")
}

##' @export
print.pSome <- function(x,indent=0,...)
  cat(formatAST(x,indent))
