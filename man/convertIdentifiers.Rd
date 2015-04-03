\name{convertIdentifiers}
\docType{methods}
\alias{convertIdentifiers}
\title{Convert the node identifiers of a pathway.}
\description{
  Converts the node identifiers of pathways.
}
\usage{
convertIdentifiers(x, to)
}
\arguments{
  \item{x}{can be a list of pathways or a single pathway}
  \item{to}{
    a string describing the type of the identifier.
    Can assume the values \code{"entrez"}, \code{"symbol"} or the name
    of one of the columns provided by an Annotation package (for example,
    \code{"UNIPROT"}).
  }
}
\value{
  A \code{Pathway} object.
}
\seealso{
  \code{\linkS4class{Pathway}}
}
\examples{
  r <- pathways("hsapiens", "reactome")
  convertIdentifiers(r$`mTOR signalling`, "symbol")
}
