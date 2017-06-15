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
buildPathway(id, title, species, database, proteinEdges,
             metaboliteEdges = NULL, mixedEdges = NULL,
             timestamp = NULL)
}
\arguments{
  \item{id}{the pathway identifier.}
  \item{title}{the title of the pathway.}
  \item{species}{the species the pathway belongs to.}
  \item{database}{the name of the database the pathway derives from.}
  \item{proteinEdges}{a data.frame of edges between proteins (or genes).

    Must have the following columns: src_type, src, dest_type, dest, direction and type.

    Direction must be one of the two strings: "directed" or "undirected".
  }
  \item{metaboliteEdges}{interactions between metabolites.

    Can be \code{NULL}. Otherwise, it must have the same structure as \code{proteinEdges}.
  }
  \item{mixedEdges}{interactions between metabolites and proteins.

    Can be \code{NULL}. Otherwise, it must have the same structure as \code{proteinEdges}.
  }
  \item{timestamp}{
    when the pathway was annotated, by default the time
    \code{buildPathway} is called.
  }
}

\seealso{
  \code{\link{Pathway-class}}
}

\examples{
edges <- data.frame(src_type = "ENTREZID", src="672",
                    dest_type = "ENTREZID", dest="7157",
                    direction="undirected", type="binding")
pathway <- buildPathway("#1", "example", "hsapiens", "database", edges)

# Example with metabolites:
edges <- data.frame(src_type = "ENTREZID", src="672",
                    dest_type = "ENTREZID", dest="7157",
                    direction="undirected", type="binding")
mixed <- data.frame(src_type = "CHEBI", src="77750",
                    dest_type = "ENTREZID", dest="7157",
                    direction="undirected", type="binding")
pathway <- buildPathway("#1", "example", "hsapiens", "database",
                        edges, mixedEdges = mixed)
}
