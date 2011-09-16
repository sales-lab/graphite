\name{nci}
\alias{nci}
\title{NCI pathways}
\description{
  A list with a \code{\link[=pathway-class]{pathway}} object for each NCI pathway.

  Schaefer CF, Anthony K, Krupa S, Buchoff J, Day M, Hannay T, Buetow KH. PID:
the Pathway Interaction Database. Nucleic Acids Res. 2009 Jan;37(Database
issue):D674-9. Epub 2008 Oct 2.

  \url{http://pid.nci.nih.gov/}
}

\section{Pathway topology conversion}{
  NCI pathways were retrieved in BioPax format from the PDI database web
  page.

  We define a pathway for each BioPax tag ``pathway''. Pathway nodes
  often correspond to multiple gene products. These can be divided into
  protein complexes (proteins linked by protein-protein interactions)
  and groups made of alternative members (genes with similar biochemical
  functions). Thus, when considering signal propagation these groups are
  considered differently. The first kind (hereafter group AND) should be
  expanded into a clique (all proteins connected to the others), while
  the second (hereafter group OR) should be expanded without connection
  among them. In the BioPax format only one type of group is allowed:
  protein complexes (group AND) with the tag 'complex'. However, it
  often happens that the 'protein' tag contains multiple 'xref' pointing
  to alternative elements of the process (group OR).

  Compound mediated interactions are interactions for which a compound
  acts as a bridge between two elements. Since chemical compounds are
  not usually measured with high-throughput technology, they should be
  removed from the network to analyse gene signals. However, the trivial
  elimination of the compounds, without signal propagation, will
  strongly bias the topology interrupting the signals that pass through
  them. If element 'A' is linked to compound 'c' and compound 'c' is
  linked to element 'B', element 'A' should be linked to element
  'B'. Not all compounds are considered for the propagation because some
  of them (for example: H2O, ATP, ADP) are highly frequent in map
  descriptions and the signal propagation through them would lead to
  chains too long. Compounds not considered for propagation are not
  characteristic of a specific reaction, but act as secondary
  substrates/products widely shared among different processes.

  graphite allows the user to see the single/multiple relation types
  that characterized an edge. The type of edges have been kept as much
  as possible similar to those annotated in the original data
  format. Some new types have been introduced due to topological
  conversion needs.
}

\seealso{
  \code{\link{pathway-class}}.
}
