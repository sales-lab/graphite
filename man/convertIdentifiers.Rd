\name{convertIdentifiers}
\alias{convertIdentifiers}
\title{Convert the node identifiers of a pathway.}
\description{
  Converts the node identifiers of a pathway to either Entrez Genes of Gene Symbols.
}
\usage{
convertIdentifiers(pathway, type)
}
\arguments{
  \item{pathway}{a pathway from \link{biocarta}, \link{kegg},
    \link{nci} or \link{reactome}.}
  \item{type}{the type of identifier: either "entrez" or "symbol".}
}
\value{
  A \code{pathway} object.
}
\seealso{
  \code{\link{biocarta}}

  \code{\link{kegg}}

  \code{\link{nci}}

  \code{\link{reactome}}

  \code{\link{pathway-class}}
}
\examples{
p <- convertIdentifiers(reactome$`mTOR signalling`, type="symbol")
}
