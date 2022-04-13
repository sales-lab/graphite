\name{runTopologyGSA}
\docType{methods}
\alias{runTopologyGSA}
\alias{runTopologyGSAMulti}
\alias{runTopologyGSA,list-method}
\title{
  Run a topological analysis on an expression dataset using topologyGSA.
}
\description{
  Use graphical models to test the pathway components highlighting those
  involved in its deregulation.

  If the option \code{Ncpus} is set to a value larger than 1 and the package
  \code{parallel} is installed, the conversion procedure will automatically
  use multiple cores.
}
\usage{
  runTopologyGSA(x, test, exp1, exp2, alpha, ...)
}
\arguments{
  \item{x}{
    a \code{\linkS4class{PathwayList}}, a list of \code{\linkS4class{Pathway}}s
    or a single \code{\linkS4class{Pathway}} object.
  }
  \item{test}{
    Either \code{"var"} and \code{"mean"}. Determine the type of test used by topologyGSA.
  }
  \item{exp1}{
    Experiment matrix of the first class, genes in columns.
  }
 \item{exp2}{
   Experiment matrix of the second class, genes in columns.
 }
 \item{alpha}{
   Significance level of the test.
 }
 \item{...}{
   Additional parameters forwarded to \code{topologyGSA}.

   When invoked on a \code{\linkS4class{PathwayList}}, can use the named
   option "maxNodes" to limit the analysis to those pathways having up to
   this given number of nodes.
 }
}
\value{
  See documentation of
  \code{\link[topologyGSA:pathway.var.test]{pathway.var.test}}
  and
  \code{\link[topologyGSA:pathway.mean.test]{pathway.mean.test}}.
}
\details{
  This function produces a warning and returns NULL when the number of
  genes in common between the expression matrices and the pathway is
  less than 3.
}
\references{
  Massa MS, Chiogna M, Romualdi C. Gene set analysis exploiting the
  topology of a pathway. BMC System Biol. 2010 Sep 1;4:121.
}
\examples{
if (require(topologyGSA)) {
  data(examples)
  colnames(y1) <- paste("SYMBOL", colnames(y1), sep = ":")
  colnames(y2) <- paste("SYMBOL", colnames(y2), sep = ":")

  k <- pathways("hsapiens", "kegg")
  p <- convertIdentifiers(k[["Fc epsilon RI signaling pathway"]], "SYMBOL")
  runTopologyGSA(p, "var", y1, y2, 0.05)
}
}
\keyword{topology}
\keyword{analysis}
\keyword{topologyGSEA}

