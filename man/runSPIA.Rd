\name{runSPIA}
\alias{runSPIA}
\title{Run SPIA analysis}
\description{
  Run a topological analysis on an expression dataset using SPIA.
}
\usage{
  runSPIA(de, all, pathwaySetName, ...)
}
\arguments{
  \item{de}{
    A named vector containing log2 fold-changes of the differentially
    expressed genes. The names of this numeric vector are Entrez gene IDs.
  }
  \item{all}{
    A vector with the Entrez IDs in the reference set. If the data was
    obtained from a microarray experiment, this set will contain all genes
    present on the specific array used for the experiment. This vector
    should contain all names of the 'de' argument.
  }
  \item{pathwaySetName}{
    The name of a pathway set created with \code{\link{prepareSPIA}}.
  }
  \item{\dots}{
    Additional options to pass to \code{\link[SPIA:spia]{spia}}.
  }
}
\details{
  The spia option "organism" is internally used. It is an error use it in
  the additional options.
}
\value{
  The same of spia, without KEGG links. A data frame containing the ranked pathways and various statistics: pSize is the number of genes
  on the pathway; NDE is the number of DE genes per pathway; tA is the observed total preturbation accumulation in the pathway; pNDE is the
  probability to observe at least NDE genes on the pathway using a hypergeometric model; pPERT is the probability to observe a total
  accumulation more extreme than tA only by chance; pG is the p-value obtained by combining pNDE and pPERT; pGFdr and pGFWER are the False
  Discovery Rate and respectively Bonferroni adjusted global p-values; and the Status gives the direction in which the pathway is perturbed (activated or inhibited).
}
\references{
  Tarca AL, Draghici S, Khatri P, Hassan SS, Mittal P, Kim JS, Kim CJ,
  Kusanovic JP, Romero R. A novel signaling pathway impact
  analysis. Bioinformatics. 2009 Jan 1;25(1):75-82.

  Adi L. Tarca, Sorin Draghici, Purvesh Khatri, et. al, A Signaling Pathway Impact Analysis for
  Microarray Experiments, 2008, Bioinformatics, 2009, 25(1):75-82.

  Draghici, S., Khatri, P., Tarca, A.L., Amin, K., Done, A., Voichita, C., Georgescu, C., Romero, R.:
  A systems biology approach for pathway level analysis. Genome Research, 17, 2007.
}
\examples{
if (require(SPIA) && require(hgu133plus2.db)) {
  data(colorectalcancer)

  top$ENTREZ <- mapIds(hgu133plus2.db, top$ID, "ENTREZID", "PROBEID", multiVals = "first")
  top <- top[!is.na(top$ENTREZ) & !duplicated(top$ENTREZ), ]
  top$ENTREZ <- paste("ENTREZID", top$ENTREZ, sep = ":")
  tg1 <- top[top$adj.P.Val < 0.05, ]

  DE_Colorectal = tg1$logFC
  names(DE_Colorectal) <- tg1$ENTREZ
  ALL_Colorectal <- top$ENTREZ

  biocarta <- pathways("hsapiens", "biocarta")[1:20]
  biocarta <- convertIdentifiers(biocarta, "ENTREZID")
  prepareSPIA(biocarta, "biocartaEx")
  runSPIA(de = DE_Colorectal, all = ALL_Colorectal, "biocartaEx")
}
}
\seealso{
  \code{\link[SPIA:spia]{spia}}
}
\keyword{spia}
\keyword{analysis}
\keyword{topology}
