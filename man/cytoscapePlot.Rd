\name{cytoscapePlot}
\alias{cytoscapePlot}
\title{Plot a pathway graph in Cytoscape}
\description{
  Renders the topology of a pathway as a Cytoscape graph.
}
\usage{
cytoscapePlot(pathway, ...)
}
\arguments{
  \item{pathway}{a \code{pathway} object.}
  \item{...}{optional arguments to pass to \link{pathwayGraph}.}
}
\details{
  Requires the \code{RCytoscape} package.
}
\seealso{
  \code{\link{pathway-class}}

  \code{\link{pathwayGraph}}
}
\examples{
\dontrun{cytoscapePlot(convertIdentifiers(reactome$`Unwinding of DNA`, "symbol"))}
}
