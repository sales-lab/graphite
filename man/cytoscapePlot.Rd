\name{cytoscapePlot}
\alias{cytoscapePlot}
\title{Plot a pathway graph in Cytoscape}
\description{
  Renders the topology of a pathway as a Cytoscape graph.
}
\usage{
cytoscapePlot(pathway, ..., cy.ver=3)
}
\arguments{
  \item{pathway}{a \code{Pathway} object.}
  \item{...}{optional arguments forwarded to \code{\link{pathwayGraph}}.}
  \item{cy.ver}{select a Cytoscape version. It can be 3 (the default) or 2.}
}
\details{
  Requires the \code{RCytoscape} package.
}
\seealso{
  \code{\linkS4class{Pathway}}

  \code{\link{pathwayGraph}}
}
\examples{
\dontrun{
  r <- pathways()
  cytoscapePlot(convertIdentifiers(reactome$`Unwinding of DNA`, "symbol"))
}
}
