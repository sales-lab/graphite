\name{runClipper}
\docType{methods}
\alias{runClipper}
\alias{runClipper,list-method}
\title{
  Run a topological analysis on an expression dataset using clipper.
}
\description{
  clipper is a package for topological gene set analysis. It implements a
  two-step empirical approach based on the exploitation of graph
  decomposition into a junction tree to reconstruct the most relevant
  signal path. In the first step clipper selects significant pathways
  according to statistical tests on the means and the concentration
  matrices of the graphs derived from pathway topologies. Then, it "clips"
  the whole pathway identifying the signal paths having the greatest
  association with a specific phenotype.
}
\usage{
runClipper(x, expr, classes, method, ...)
}
\arguments{
  \item{x}{
    a \code{\linkS4class{PathwayList}}, a list of \code{\linkS4class{Pathway}}s
    or a single \code{\linkS4class{Pathway}} object.
  }
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
  \item{\dots}{
    Additional options; see for details
    \code{\link[clipper:easyClip]{easyClip}}.

    When invoked on a \code{\linkS4class{PathwayList}}, can use the named
    option "maxNodes" to limit the analysis to those pathways having up to
    this given number of nodes.
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
if (require(clipper) & require(ALL) & require(a4Preproc)) {
  data(ALL)
  pheno <- as(phenoData(ALL), "data.frame")
  samples <- unlist(lapply(c("NEG", "BCR/ABL"), function(t) {
    which(grepl("^B\\\\d*", pheno$BT) & (pheno$mol.biol == t))[1:10]
  }))
  classes <- c(rep(1,10), rep(2,10))

  expr <- exprs(ALL)[,samples]
  rownames(expr) <- featureData(addGeneInfo(ALL))$ENTREZID

  k <- as.list(pathways("hsapiens", "kegg"))
  selected <- k[c("Bladder cancer", "Cytosolic DNA-sensing pathway")]

  runClipper(selected, expr, classes, "mean", pathThr = 0.1)
}
}
\seealso{
  \code{\link[clipper:easyClip]{clipper}}
}
\keyword{topology}
\keyword{analysis}
\keyword{clipper}
