\name{runDEGraph}
\alias{runDEGraph}
\docType{data}
\title{
Run topological analysis on expression dataset using DEGraph package. See \code{\link[DEGraph:testOneGraph]{testOneGraph}}.
}
\description{
DEGraph implements recent hypothesis testing methods which directly assess whether a particular gene network is differentially expressed between two conditions. See \code{\link[DEGraph:testOneGraph]{testOneGraph}} for more details.
}
\usage{
runDEGraph(pathway, expr, classes)
}
\arguments{
  \item{pathway}{
    One of the pathways contained in \link{biocarta}, \link{kegg},\link{nci} or \link{reactome}.
  }
  \item{expr}{
    A \code{matrix} (size: number \code{p} of genes x number \code{n} of
    samples) of gene expression.
  }
  \item{classes}{
    A \code{vector} (length: \code{n}) of class assignments.
  }
}
\details{
  The expression data and the pathway have to be annotated in the same set of identifiers.
}
\references{
L. Jacob, P. Neuvial, and S. Dudoit. Gains in power from structured two-sample tests of means on graphs. Technical Report arXiv:q-bio/1009.5173v1, arXiv, 2010.
}
\examples{
if (require(DEGraph)) {
  data("Loi2008_DEGraphVignette")

  p <- convertIdentifiers(biocarta[["actions of nitric oxide in the heart"]], "entrez")
  runDEGraph(p, exprLoi2008, classLoi2008)
}
}
\keyword{topology}
\keyword{analysis}
\keyword{DEGraph}
