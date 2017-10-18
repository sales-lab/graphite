\name{PathwayList-class}
\Rdversion{1.1}
\docType{class}
\alias{PathwayList-class}
\alias{[,PathwayList-method}
\alias{[[,PathwayList-method}
\alias{$,PathwayList-method}
\alias{convertIdentifiers,PathwayList-method}
\alias{length,PathwayList-method}
\alias{names,PathwayList-method}
\alias{prepareSPIA,PathwayList-method}
\alias{runClipper,PathwayList-method}
\alias{runTopologyGSA,PathwayList-method}

\title{Class \code{"PathwayList"}}
\description{
  A collection of pathways from a single database.
}
\section{Extends}{
  Class \code{"\linkS4class{Pathways}"}, directly.
}
\section{Methods}{
  \describe{
    \item{}{
      \code{l[i]}:
      Returns a selection of the pathways contained in the pathway list.
    }
    \item{}{
      \code{l[[i]]}
      Access one of the pathways contained in the pathway list.
    }
    \item{}{
      \code{l$`title`}
      Access one of the pathways by its title.
    }
    \item{}{
      \code{convertIdentifiers(l, to)}
      Returns a new list of pathways using a different type of node
      identifiers.
    }
    \item{}{
      \code{length(l)}
      Returns the number of pathways contained in the list.
    }
    \item{}{
      \code{names(l)}
      Returns the titles of the pathways contained in the list.
    }
    \item{}{
      \code{prepareSPIA(l, pathwaySetName, print.names=FALSE)}
      Prepares the pathways for a SPIA analysis.
    }
    \item{}{
      \code{runClipper(l, expr, classes, method, maxNodes=150, ...)}
      Runs a \code{clipper} analysis over all the pathways in the list.
    }
    \item{}{
      \code{runTopologyGSA(l, test, exp1, exp2, alpha, maxNodes=150, ...)}
      Runs a \code{topologyGSA} analysis over all the pathways in the list.
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
