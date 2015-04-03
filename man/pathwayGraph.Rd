\name{pathwayGraph}
\alias{pathwayGraph}
\title{Graph representing the topology of a pathway}
\description{
  Builds a \code{graphNEL} object representing the topology of a pathway.
}
\usage{
pathwayGraph(pathway, edge.types=NULL)
}
\arguments{
  \item{pathway}{a \code{\linkS4class{Pathway}} object.}
  \item{edge.types}{keep only the edges maching the type names in this vector.}
}
\value{
  A \code{graphNEL} object.
}
\seealso{
  \code{\linkS4class{Pathway}}

  \code{\link[graph:graphNEL-class]{graphNEL}}
}
\examples{
  r <- pathways("hsapiens", "reactome")
  pathwayGraph(r$`mTOR signalling`, edge.types="binding")
}
