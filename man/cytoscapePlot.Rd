\name{cytoscapePlot}
\alias{cytoscapePlot}
\title{Plot a pathway graph in Cytoscape}
\description{
  Renders the topology of a pathway as a Cytoscape graph.
}
\usage{
cytoscapePlot(pathway, ..., cy.ver = 3)
}
\arguments{
  \item{pathway}{a \code{Pathway} object.}
  \item{...}{optional arguments forwarded to \code{\link{pathwayGraph}}.}
  \item{cy.ver}{select a Cytoscape version. Only version 3 is supported in this
    release.}
}
\details{
  Requires the \code{RCy3} package.
}
\value{
  An invisible list with two items:
  \item{graph}{the \code{\linkS4class{graphNEL}} object sent to Cytoscape.}
  \item{window}{the RCy3 window handle.}
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
