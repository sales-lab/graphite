\name{pathways}
\alias{pathways}
\title{Retrieve a list of pathways.}
\description{
  This function retrieves a list of pathways from a database for a given
  species.
}
\usage{
  pathways(species, database)
}
\arguments{
  \item{species}{one of the supported species}
  \item{database}{the name of the pathway database}
}
\value{
  A \code{PathwayList} object.
}
\seealso{
  \code{\linkS4class{PathwayList}}
}
\examples{
  pathways("hsapiens", "reactome")
}
