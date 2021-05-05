\name{pathways}
\alias{pathways}
\title{Retrieve a list of pathways.}
\description{
  Retrieve a list of pathways from a database for a given species.

  graphite currently supports the following databases:
  \itemize{
    \item \href{http://www.kegg.jp/kegg/pathway.html}{KEGG}
    \item \href{http://www.pantherdb.org/pathway/}{PANTHER}
    \item \href{https://pathbank.org/}{PathBank}
    \item \href{https://www.pharmgkb.org/pathways/}{PharmGKB}
    \item \href{http://www.reactome.org/}{Reactome}
    \item \href{http://smpdb.ca/}{SMPDB}
    \item \href{https://www.wikipathways.org/}{WikiPathways}
  }

  Call the \code{\link{pathwayDatabase}} function for more details.
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
  \code{\linkS4class{PathwayList}},
  \code{\link{pathwayDatabases}}
}
\examples{
  pathways("hsapiens", "reactome")
}
