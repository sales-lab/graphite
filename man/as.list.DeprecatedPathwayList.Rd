\name{as.list.DeprecatedPathwayList}
\alias{as.list.DeprecatedPathwayList}
\title{
  Convertion of DeprecatedPathwayLists into lists.
}
\description{
  Converts a \code{\linkS4class{DeprecatedPathwayList}}
  into a list of \code{\linkS4class{Pathway}}s.
}
\usage{
  \method{as.list}{DeprecatedPathwayList}(x, ...)
}
\arguments{
  \item{x}{a \code{\linkS4class{DeprecatedPathwayList}} object}
  \item{\dots}{extra arguments to as.list}
}
\value{
  A list of pathways.
}
\author{
  Gabriele Sales
}
\seealso{
  \code{\linkS4class{DeprecatedPathwayList}}
}
\examples{
  as.list(reactome)
}
