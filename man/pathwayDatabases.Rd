\name{pathwayDatabases}
\alias{pathwayDatabases}
\title{
  List the available pathway databases.
}
\description{
  Obtains the list of pathway databases available through \code{graphite}.
}
\usage{
  pathwayDatabases()
}
\value{
  Returns a \code{data.frame} with two columns: \code{species} and
  \code{database}.
}
\seealso{
  \code{\link{pathways}}
}
\author{
  Gabriele Sales
}
\examples{
  pathwayDatabases()
}
