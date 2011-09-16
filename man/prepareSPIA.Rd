\name{prepareSPIA}
\alias{prepareSPIA}
\title{
Prepare pathway dataset needed by runSPIA.
}
\description{
Prepare pathway dataset needed by runSPIA. See \code{\link{runSPIA}} and
\code{\link[SPIA:spia]{spia}} for more details.
}
\usage{
prepareSPIA(db, pathwaySetName, print.names = FALSE)
}
\arguments{
  \item{db}{
    A list of pathways like \link{biocarta}, \link{kegg},\link{nci} or \link{reactome}.
  }
  \item{pathwaySetName}{
Name of the pathway set in output.
}
  \item{print.names}{
The verbose option.
}
}
\details{
It is required to run this function before \code{\link{runSPIA}} to prepare the pathway set as required by \code{runSPIA}.
}

\seealso{
  \code{\link{runSPIA}}
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
