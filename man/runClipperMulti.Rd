\name{runClipperMulti}
\alias{runClipperMulti}
\docType{data}
\title{
  Run topological analyses on an expression dataset, calling clipper
  over all the pathways in a list.
}
\description{
  clipper is a package for topological gene set analysis. It implements a
  two-step empirical approach based on the exploitation of graph
  decomposition into a junction tree to reconstruct the most relevant
  signal path. In the first step clipper selects significant pathways
  according to statistical tests on the means and the concentration
  matrices of the graphs derived from pathway topologies. Then, it "clips"
  the whole pathway identifying the signal paths having the greatest
  association with a specific phenotype. This function easily provide
  pathway analysis using clipper on an entire pathway database.
}
\usage{
runClipperMulti(pathways, expr, classes, method, maxNodes=150, ...)
}
\arguments{
  \item{pathways}{
    A list of pathways, such as \link{biocarta}, \link{kegg},\link{nci},
    \link{reactome} or \link{spike}.
  }
  \item{expr}{
    A \code{matrix} (size: number \code{p} of genes x number \code{n} of
    samples) of gene expression.
  }
  \item{classes}{
    A \code{vector} (length: \code{n}) of class assignments.
  }
  \item{method}{
    The kind of test to perform on the cliques. It could be either mean or variance.
  }
  \item{maxNodes}{
    Ignore pathways with more than \code{"maxNodes"} nodes. Set to
    \code{"NULL"} to disable the filter.
  }
  \item{\dots}{
    Additional options for details see \code{\link[clipper:easyClip]{clipper}}.
  }
}
\value{
  A list with two elements:
  \itemize{
    \item{results}: a list with one entry for each successfully analyzed pathway;
    \item{errors}: a vector containing the error messages of failed analyses.
  }
}
\details{
  The expression data and the pathway have to be annotated in the same set of identifiers.
}
\references{
Martini P, Sales G, Massa MS, Chiogna M, Romualdi C. Along signal paths: an
empirical gene set approach exploiting pathway topology. Nucleic Acids Res. 2013 
Jan 7;41(1):e19. doi: 10.1093/nar/gks866. Epub 2012 Sep 21. PubMed PMID:
23002139; PubMed Central PMCID: PMC3592432.
}

\examples{
if (require(clipper) & require(ALL)){
  paths <- lapply(kegg[1:5], function(x) convertIdentifiers(x, "entrez"))
  genes <- unlist(lapply(paths, nodes))
  data(ALL)
  all <- as.matrix(exprs(ALL[1:length(genes),1:20]))
  classes <- c(rep(1,10), rep(2,10))
  rownames(all) <- genes
  clipped <- runClipperMulti(paths, all, classes, "mean", pathThr=0.1)
}
}
\seealso{
  \code{\link[clipper:easyClip]{clipper}}
}
\keyword{topology}
\keyword{analysis}
\keyword{clipper}
