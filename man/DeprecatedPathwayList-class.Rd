\name{DeprecatedPathwayList-class}
\Rdversion{1.1}
\docType{class}
\alias{DeprecatedPathwayList-class}
\alias{[,DeprecatedPathwayList-method}
\alias{[[,DeprecatedPathwayList-method}
\alias{$,DeprecatedPathwayList-method}
\alias{convertIdentifiers,DeprecatedPathwayList-method}
\alias{length,DeprecatedPathwayList-method}
\alias{names,DeprecatedPathwayList-method}
\alias{prepareSPIA,DeprecatedPathwayList-method}
\alias{runClipper,DeprecatedPathwayList-method}
\alias{runDEGraph,DeprecatedPathwayList-method}
\alias{runTopologyGSA,DeprecatedPathwayList-method}

\title{Class \code{"DeprecatedPathwayList"}}
\description{
  Represents deprecated objects for accessing pathway databases.
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
      \code{runDEGraph(l, expr, classes, maxNodes=150, ...)}
      Runs a \code{DEGraph} analysis over all the pathways in the list.
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
  \code{\linkS4class{PathwayList}}
}
\keyword{classes}
