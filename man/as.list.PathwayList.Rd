\name{as.list.PathwayList}
\alias{as.list.PathwayList}
\title{
  Convertion of PathwayLists into lists.
}
\description{
  Converts a \code{\linkS4class{PathwayList}}
  into a list of \code{\linkS4class{Pathway}}s.
}
\usage{
  \method{as.list}{PathwayList}(x, ...)
}
\arguments{
  \item{x}{a \code{\linkS4class{PathwayList}} object}
  \item{\dots}{extra arguments to as.list}
}
\value{
  A list of pathways.
}
\author{
  Gabriele Sales
}
\seealso{
  \code{\linkS4class{PathwayList}}
}
\examples{
  as.list(kegg)
}
