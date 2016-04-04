\name{buildPathway}
\alias{buildPathway}
\title{
Build a Pathway object.
}
\description{
  This function creates a new object of type \code{Pathway} given a data
  frame describing its edges.
}
\usage{
buildPathway(id, title, edges, species, database, identifier, timestamp=NULL)
}
\arguments{
  \item{id}{the pathway identifier.}
  \item{title}{the title of the pathway.}
  \item{edges}{a data.frame of pathway edges.

    Must have the following columns: src, dest, direction and type.

    Direction must be one of the two strings: "directed" or "undirected".
  }
  \item{species}{the species the pathway belongs to.}
  \item{database}{the name of the database the pathway derives from.}
  \item{identifier}{the type of identifier used to label the pathway nodes.}
  \item{timestamp}{
    when the pathway was annotated, by default the time
    \code{buildPathway} is called.
  }
}

\seealso{
  \code{\link{Pathway-class}}
}

\examples{
edges <- data.frame(src="672", dest="7157", direction="undirected", type="binding")
pathway <- buildPathway("#1", "example", edges, "hsapiens", "database", "ENTREZID")
}
