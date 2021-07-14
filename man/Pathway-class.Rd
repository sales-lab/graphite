\name{Pathway-class}
\Rdversion{1.1}
\docType{class}
\alias{Pathway-class}
\alias{convertIdentifiers,Pathway-method}
\alias{edges,Pathway-method}
\alias{nodes,Pathway-method}
\alias{plot,Pathway,ANY-method}
\alias{runClipper,Pathway-method}
\alias{runTopologyGSA,Pathway-method}
\alias{pathwayId}
\alias{pathwayTitle}
\alias{pathwayDatabase}
\alias{pathwaySpecies}
\alias{pathwayTimestamp}
\alias{pathwayURL}

\title{Class \code{"Pathway"}}
\description{
    A biological pathway.
}
\section{Variants}{
  A \code{Pathway} instance actually stores multiple variants of the same biological data.

  This is the list of included variants:
  \itemize{
    \item{\code{proteins}: includes only interactions among proteins;}
    \item{\code{metabolites}: includes only interactions among metabolites;}
    \item{\code{mixed}: includes all available interactions.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{pathwayId(p)}:}{
      Returns the native ID of the pathway.
    }
    \item{\code{pathwayTitle(p)}:}{
      Returns the title of the pathway.
    }
    \item{\code{pathwayDatabase(p)}:}{
      Returns the name of the database the pathway was derived from.
    }
    \item{\code{pathwaySpecies(p)}:}{
      Returns the name of the species in which the pathway was annotated.
    }
    \item{\code{pathwayTimestamp(p)}:}{
      Returns the date of pathway data retrieval.
    }
    \item{\code{pathwayURL(p)}:}{
      Returns the URL of the pathway in its original database, if available.
    }
    \item{\code{convertIdentifiers(p, to)}:}{
      Returns a new pathway using a different type of node identifiers.
    }
    \item{\code{edges(p, which = c("proteins", "metabolites", "mixed"),
                stringsAsFactors = TRUE)}:}{
      Returns a data.frame describing the edges of this pathway.

      The option \code{which} selects the desired pathway variant (see section "Variants" above).

      If \code{stringsAsFactors} is \code{TRUE}, strings are converted to factors.
    }
    \item{\code{nodes(p, which = c("proteins", "metabolites", "mixed"))}:}{
      Returns the names of the nodes belonging to this pathway.

      The option \code{which} selects the desired pathway variant (see section "Variants" above).
    }
    \item{\code{plot(p)}:}{
      Shows the pathway topology in Cytoscape.
    }
    \item{\code{runClipper(p, expr, classes, method, ...)}:}{
      Runs a \code{clipper} analysis over the pathway.
    }
    \item{\code{runTopologyGSA(p, test, exp1, exp2, alpha, ...)}:}{
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
\examples{
  reactome <- pathways("hsapiens", "reactome")
  pathway <- reactome[[1]]

  pathwayTitle(pathway)
  pathwaySpecies(pathway)
  nodes(pathway)
  edges(pathway)
}
\keyword{classes}
