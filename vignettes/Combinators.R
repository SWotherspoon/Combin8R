## -----------------------------------------------------------------------------
library(Combin8R)
pDog <- pLiteral("Tag","dog")
pDog("monkey")
pDog("dog")

## -----------------------------------------------------------------------------
unclass(pDog("dog")$result)

## -----------------------------------------------------------------------------
pDog("dog")$result

## -----------------------------------------------------------------------------
pLabel <- pRegex("Label","label (\\d+)")
pLabel("label 7")

## -----------------------------------------------------------------------------
pLabel <- pRegex(function(value) structure(list(value=as.numeric(value[[2]])),class=c("Label","pRegex")),
                 "label (\\d+)")
pLabel("label 7")

## -----------------------------------------------------------------------------
program <- "repeat 10 [right 36 repeat 5 [forward 54 right 72]]"

## -----------------------------------------------------------------------------
pInteger <- pRegex("Integer","\\d+")
pSpaces <- pRegex("Spaces","\\s*")
pSpaces1 <- pRegex("Spaces1","\\s+")
pCommands <-
  pSome("Commands",
    pSeq("CommandWhite",
      pAlt("Command",
        pSeq("forwardCmd",pLiteral("forward"),pSpaces1,pInteger),
        pSeq("rightCmd",pLiteral("right"),pSpaces1,pInteger),
        pSeq("leftCmd",pLiteral("left"),pSpaces1,pInteger),
        pSeq("repeatCmd",pLiteral("repeat"),pSpaces1,pInteger,pSpaces1,pBlock)
      ),
      pSpaces))
pBlock <- pSeq("Block",pLiteral("["),pSpaces,pCommands,pLiteral("]"))

## -----------------------------------------------------------------------------
p <- pCommands(program)
p

## -----------------------------------------------------------------------------
formatAST.Integer <- function(x,indent,...)
  as.character(x$value)
formatAST.forwardCmd <- function(x,indent,...)
  paste("forward",formatAST(x$value[[3]]))
formatAST.rightCmd <- function(x,indent,...)
  paste("right",formatAST(x$value[[3]]))
formatAST.leftCmd <- function(x,indent,...)
  paste("left",formatAST(x$value[[3]]))
formatAST.repeatCmd <- function(x,indent,...)
  paste("repeat",formatAST(x$value[[3]]),formatAST(x$value[[5]]))
formatAST.Command <- function(x,indent,...)
  formatAST(x$value)
formatAST.CommandWhite <- function(x,indent,...)
  formatAST(x$value[[1]])
formatAST.Commands <- function(x,indent,...)
  paste(sapply(x$value,formatAST),collapse=" ")
formatAST.Block <- function(x,indent,...)
  paste("[",formatAST(x$value[[3]]),"]",sep="")

## -----------------------------------------------------------------------------
p

## -----------------------------------------------------------------------------
mkprm <- function(value) structure(list(value[[1]]$value,value[[3]]),class="prm")
mkrpt <- function(value) structure(list(value[[3]],value[[5]]),class="rpt")
mkblk <- function(value) structure(value[[3]],class="blk")
pInteger <- pRegex(as.numeric,"\\d+")
pSpaces <- pRegex("Spaces","\\s*")
pSpaces1 <- pRegex("Spaces1","\\s+")
pCommands <-
  pSome(function(value) value,
        pSeq(function(value) value[[1]],
             pAlt(function(value) value,
                  pSeq(mkprm,pLiteral("forward"),pSpaces1,pInteger),
                  pSeq(mkprm,pLiteral("right"),pSpaces1,pInteger),
                  pSeq(mkprm,pLiteral("left"),pSpaces1,pInteger),
                  pSeq(mkrpt,pLiteral("repeat"),pSpaces1,pInteger,pSpaces1,pBlock)
             ),
             pSpaces))
pBlock <- pSeq(mkblk,pLiteral("["),pSpaces,pCommands,pLiteral("]"))

## -----------------------------------------------------------------------------
p <- pCommands(program)
p

## -----------------------------------------------------------------------------
formatAST.prm <- function(x,indent,...)
  paste(x[[1]],x[[2]],collapse=" ")
print.prm <- function(x,indent=0,...)
  cat(formatAST(x,indent))
formatAST.rpt <- function(x,indent,...)
  paste("repeat",x[[1]],formatAST(x[[2]]),collapse=" ")
print.rpt <- function(x,indent=0,...)
  cat(formatAST(x,indent))
formatAST.blk <- function(x,indent,...)
  paste("[",paste(sapply(x,formatAST),collapse=" "),"]",sep="")
print.blk <- function(x,indent=0,...)
  cat(formatAST(x,indent))

## -----------------------------------------------------------------------------
p

## -----------------------------------------------------------------------------
mkiter <- local({n <- 0
  function() {
    n <<- n+1
    as.name(paste("k",n,sep=""))
  }})
cmpfwd <- function(value)
  substitute({
      x0 <- x1
      x1 <- x1+r*c(cos(theta),sin(theta))
      segments(x0[1],x0[2],x1[1],x1[2])
    },list(r=value[[3]]))
cmprgt <- function(value)
  substitute(theta <- theta + pi/180*a,list(a=value[[3]]))
cmplft <- function(value)
  substitute(theta <- theta - pi/180*a,list(a=value[[3]]))
cmprpt <- function(value)
  substitute(for(k in 1:n) b,
    list(k=mkiter(),n=value[[3]],b=value[[5]]))
cmpblk <- function(value)
  if(length(value)>1) as.call(c(as.name("{"),value[[3]])) else value
pInteger <- pRegex(as.numeric,"\\d+")
pSpaces <- pRegex("Spaces","\\s*")
pSpaces1 <- pRegex("Spaces1","\\s+")
pCommands <-
  pSome(function(value) value,
        pSeq(function(value) value[[1]],
             pAlt(function(value) value,
                  pSeq(cmpfwd,pLiteral("forward"),pSpaces1,pInteger),
                  pSeq(cmprgt,pLiteral("right"),pSpaces1,pInteger),
                  pSeq(cmplft,pLiteral("left"),pSpaces1,pInteger),
                  pSeq(cmprpt,pLiteral("repeat"),pSpaces1,pInteger,pSpaces1,pBlock)
             ),
             pSpaces))
pBlock <- pSeq(cmpblk,pLiteral("["),pSpaces,pCommands,pLiteral("]"))
pLogo <- function(input,xlim=c(-100,100),ylim=c(-100,100)) {
  p <- pCommands(input)
  if(!is.null(p)) {
    body <- as.call(c(as.name("{"),
                      quote(plot.new()),
                      substitute(plot.window(xlim,ylim)),
                      quote(x1 <- c(0,0)),
                      quote(theta <- 0),
                      p$result))
    list(input=p$input,
         result=substitute(local(body),list(body=body)))
    }
}

## -----------------------------------------------------------------------------
p <- pLogo(program)
p

## -----------------------------------------------------------------------------
eval(p$result)

