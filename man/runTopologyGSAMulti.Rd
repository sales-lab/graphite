\name{runTopologyGSAMulti}
\alias{runTopologyGSAMulti}
\title{
  Run a topological analysis on an expression dataset using topologyGSA.
}
\description{
  This function is deprecated and will be removed in a future release.
  You can use \code{\link{runTopologyGSA}} instead.
}
\usage{
  runTopologyGSAMulti(pathways, test, exp1, exp2, alpha, maxNodes=150, ...)
}
\arguments{
  \item{pathways}{a \code{\linkS4class{PathwayList}} object.}
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
  \item{maxNodes}{
    Ignore pathways with more than \code{"maxNodes"} nodes. Set to
    \code{"NULL"} to disable the filter.
  }
  \item{...}{
    Additional parameters for topologyGSA.
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
  This function produces a warning and whenever the number of
  genes in common between the expression matrices and a
  pathway is less than 3.
}
\references{
  Massa MS, Chiogna M, Romualdi C. Gene set analysis exploiting the
  topology of a pathway. BMC System Biol. 2010 Sep 1;4:121.
}
\examples{
if (require(topologyGSA)) {
  data(examples)

  k <- pathways("hsapiens", "kegg")
  ps <- convertIdentifiers(
    k[c("Acute myeloid leukemia", "Fc epsilon RI signaling pathway")],
    "symbol")
  runTopologyGSAMulti(ps, "var", y1, y2, 0.05)
}
}
\seealso{
  \code{\link[topologyGSA:pathway.var.test]{pathway.var.test}}
  \code{\link[topologyGSA:pathway.mean.test]{pathway.mean.test}}
}
\keyword{topology}
\keyword{analysis}
\keyword{topologyGSEA}
