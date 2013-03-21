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
  \item{pathway}{a \code{pathway} object.}
  \item{edge.types}{keep only the edges maching the type names in this vector.}
}
\value{
  A \code{graphNEL} object.
}
\seealso{
  \code{\link{pathway-class}}

  \code{\link[graph:graphNEL-class]{graphNEL-class}}
}
\examples{
pathwayGraph(reactome$`mTOR signalling`, edge.types=c("binding"))
}
