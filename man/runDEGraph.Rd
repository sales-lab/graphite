\name{runDEGraph}
\docType{methods}
\alias{runDEGraph}
\alias{runDEGraphMulti}
\alias{runDEGraph,list-method}
\title{
  Run a topological analysis on an expression dataset using DEGraph package.
}
\description{
  DEGraph implements recent hypothesis testing methods which directly
  assess whether a particular gene network is differentially expressed
  between two conditions.
}
\usage{
  runDEGraph(x, expr, classes, ...)
}
\arguments{
  \item{x}{
    a \code{\linkS4class{PathwayList}}, a list of \code{\linkS4class{Pathway}}s
    or a single \code{\linkS4class{Pathway}} object.
  }
  \item{expr}{
    a \code{matrix} (size: number \code{p} of genes x number \code{n} of
    samples) of gene expression.
  }
  \item{classes}{
    a \code{vector} (length: \code{n}) of class assignments.
  }
  \item{\dots}{
    when invoked on a \code{\linkS4class{PathwayList}}, can use the named
    option "maxNodes" to limit the analysis to those pathways having up to
    this given number of nodes.
  }
}
\details{
  The expression data and the pathway have to be annotated in the same set of identifiers.
}
\references{
  L. Jacob, P. Neuvial, and S. Dudoit. Gains in power from structured
  two-sample tests of means on graphs. Technical Report
  arXiv:q-bio/1009.5173v1, arXiv, 2010.
}
\examples{
if (require(DEGraph)) {
  data("Loi2008_DEGraphVignette")

  b <- pathways("hsapiens", "biocarta")
  p <- convertIdentifiers(b[["actions of nitric oxide in the heart"]], "entrez")
  runDEGraph(p, exprLoi2008, classLoi2008)
}
}
\seealso{
  \code{\link[DEGraph:testOneGraph]{testOneGraph}}
}
\keyword{topology}
\keyword{analysis}
\keyword{DEGraph}
