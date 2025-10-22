\name{purgeCacheV1}
\alias{purgeCacheV1}
\title{Purge Pathway Data Cache (Format v1)}
\description{
  Delete the pathway data cache using the deprecated format 1.  

  Prior to version 1.55.1, graphite stored pathway data locally in format v1.
  This format is now deprecated and graphite will automatically migrate to and use format v2 going forward.
  This function allows you to reclaim disk space by deleting the v1 cache if you no longer require compatibility with older graphite versions.
}
\usage{
  purgeCacheV1()
}
\value{
  None. This function is called for its side effect of deleting files.
}
\seealso{
  \code{\link{pathways}}
}
\examples{
  \dontrun{purgeCacheV1()}
}
