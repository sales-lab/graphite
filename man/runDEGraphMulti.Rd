\name{runDEGraphMulti}
\alias{runDEGraphMulti}
\docType{data}
\title{
  Run topological analyses on an expression dataset, calling DEGraph
  over all the pathways in a list.
}
\description{
  DEGraph implements recent hypothesis testing methods which directly
  assess whether a particular gene network is differentially expressed
  between two conditions.
}
\usage{
runDEGraphMulti(pathways, expr, classes, maxNodes=150)
}
\arguments{
  \item{pathways}{
    A list of pathways, such as \link{biocarta}, \link{kegg},\link{nci} or \link{reactome}.
  }
  \item{expr}{
    A \code{matrix} (size: number \code{p} of genes x number \code{n} of
    samples) of gene expression.
  }
  \item{classes}{
    A \code{vector} (length: \code{n}) of class assignments.
  }
  \item{maxNodes}{
    Ignore pathways with more than \code{"maxNodes"} nodes. Set to
    \code{"NULL"} to disable the filter.
  }
}
\value{
  A list with two elements:
  \itemize{
    \item{results}: a list with one entry for each successfully analyzed pathway;
    \item{errors}: a vector containing the error messages of failed analyses.
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

  ps <- lapply(biocarta[1:3],
               function(p) convertIdentifiers(p, "entrez"))
  runDEGraphMulti(ps, exprLoi2008, classLoi2008)
}
}
\seealso{
  \code{\link[DEGraph:testOneGraph]{testOneGraph}}
}
\keyword{topology}
\keyword{analysis}
\keyword{DEGraph}
