\name{pathway-class}
\Rdversion{1.1}
\docType{class}
\alias{pathway-class}
\alias{edges,pathway,ANY-method}
\alias{nodes,pathway-method}
\alias{show,pathway-method}

\title{Class \code{"pathway"}}
\description{
  This class represents a biological pathway.
}
\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"vector"}.}
    \item{\code{nodes}:}{Object of class \code{"vector"}.}
    \item{\code{edges}:}{Object of class \code{"data.frame"}.
      The \code{data.frame} has one row for each edge and four columns:
      source node, destination node, edge direction and type.
    }
    \item{\code{ident}:}{Object of class \code{"vector"}.
      The type of identifiers used to label the nodes. The value of
      "native" means that the identifiers are those present in the
      original pathway definition.

      See \code{\link{convertIdentifiers}} to convert them.
    }
    \item{\code{database}:}{Object of class \code{"vector"}.
      The name of the database from which the pathway was retrieved.
    }
    \item{\code{timestamp}:}{Object of class \code{"Date"}.
      The date when the pathway was retrieved.
    }
  }
}
\section{Methods}{
  \describe{
    \item{edges}{\code{signature(object = "pathway")}:
      retrieves the \code{data.frame} describing the pathway edges.
    }

    \item{nodes}{\code{signature(object = "pathway")}: retrieves the
      \code{vector} enumerating the identifiers of the pathway nodes.
    }
  }
}
\keyword{classes}
