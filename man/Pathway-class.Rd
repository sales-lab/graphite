\name{Pathway-class}
\Rdversion{1.1}
\docType{class}
\alias{Pathway-class}
\alias{convertIdentifiers,Pathway-method}
\alias{edges,Pathway,ANY-method}
\alias{nodes,Pathway-method}
\alias{plot,Pathway,ANY-method}
\alias{runClipper,Pathway-method}
\alias{runDEGraph,Pathway-method}
\alias{runTopologyGSA,Pathway-method}

\title{Class \code{"Pathway"}}
\description{
    A biological pathway.
}
\section{Slots}{
  \describe{
    \item{\code{id}:}{
      the native ID of the pathway.
    }
    \item{\code{title}:}{
      the title of the pathway.
    }
    \item{\code{edges}:}{
      a data.frame describing the edges of this pathway.
    }
    \item{\code{database}:}{
      the name of the database the pathway was derived from.
    }
    \item{\code{species}:}{
      the name of the species in which the pathway was annotated.
    }
    \item{\code{identifier}:}{
      the type of node identifier used by this pathway.
    }
    \item{\code{timestamp}:}{
      the date in which the pathway data was retrieved.
    }
  }
}
\section{Methods}{
  \describe{
    \item{}{
      \code{convertIdentifiers(p, to)}
      Returns a new pathway using a different type of node identifiers.
    }
    \item{}{
      \code{edges(p)}
      Returns a data.frame describing the edges of this pathway.
    }
    \item{}{
      \code{nodes(p)}
      Return the names of the nodes belonging to this pathway.
    }
    \item{}{
      \code{plot(p)}
      Shows the pathway topology in Cytoscape.
    }
    \item{}{
      \code{runClipper(p, expr, classes, method, ...)}
      Runs a \code{clipper} analysis over the pathway.
    }
    \item{}{
      \code{runDEGraph(p, expr, classes, ...)}
      Runs a \code{DEGraph} analysis over the pathway.
    }
    \item{}{
      \code{runTopologyGSA(p, test, exp1, exp2, alpha, ...)}
      Runs a \code{topologyGSA} analysis over the pathway.
    }
  }
}
\author{
  Gabriele Sales
}

\seealso{
  \code{\link{pathways}}
}
\keyword{classes}
