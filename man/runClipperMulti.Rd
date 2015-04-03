\name{runClipperMulti}
\alias{runClipperMulti}
\title{
  Run a topological analysis on an expression dataset using clipper.
}
\description{
  This function is deprecated and will be removed in a future release.
  You can use \code{\link{runClipper}} instead.
}
\usage{
  runClipperMulti(pathways, expr, classes, method, maxNodes=150, ...)
}
\arguments{
  \item{pathways}{a \code{\linkS4class{PathwayList}} object.}
  \item{expr}{
    a \code{matrix} (size: number \code{p} of genes x number \code{n} of
    samples) of gene expression.
  }
  \item{classes}{
    a \code{vector} (length: \code{n}) of class assignments.
  }
  \item{method}{
    the kind of test to perform on the cliques. It could be
    either \code{"mean"} or \code{"variance"}.
  }
  \item{maxNodes}{
    ignore a pathway when it has more than this number of nodes.
  }
  \item{\dots}{
    Additional options; see for details
    \code{\link[clipper:easyClip]{easyClip}}.
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
  k <- pathways("hsapiens", "kegg")
  paths <- convertIdentifiers(k[1:5], "entrez")
  genes <- unlist(lapply(paths, nodes))
  data(ALL)
  all <- as.matrix(exprs(ALL[1:length(genes),1:20]))
  classes <- c(rep(1,10), rep(2,10))
  rownames(all) <- genes
  runClipperMulti(paths, all, classes, "mean", pathThr=0.1)
}
}
\seealso{
  \code{\link[clipper:easyClip]{clipper}}
}
\keyword{topology}
\keyword{analysis}
\keyword{clipper}
