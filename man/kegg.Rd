\name{kegg}
\alias{kegg}
\title{KEGG pathways}
\description{
  A list with a \code{\link[=pathway-class]{pathway}} object for each KEGG pathway.

  Ogata H, Goto S, Sato K, Fujibuchi W, Bono H, Kanehisa M. KEGG: Kyoto Encyclopedia of Genes and Genomes. Nucleic Acids Res. 1999 Jan 1;27(1):29-34.

  \url{http://www.genome.jp/kegg/}
}

\section{Pathway topology conversion}{ KEGG pathway were retrieved in
  KGML format from the KEGG ftp site.
  
  KEGG database provides separate xml files, one for each pathway. A
  pathway is therefore define by all the reactions described within each
  file.

  Pathway nodes often correspond to multiple gene products. These can be
  divided into protein complexes (proteins linked by protein-protein
  interactions) and groups made of alternative members (genes with
  similar biochemical functions). Thus, when considering signal
  propagation these groups are considered differently. The first kind
  (hereafter group AND) should be expanded into a clique (all proteins
  connected to the others), while the second (hereafter group OR) should
  be expanded without connection among them. In the KGML format there
  are two ways of defining nodes with multiple elements: protein
  complexes (group AND defined by entry type=``group'') and groups with
  alternative members (group OR defined by entry type=``gene'').

  Compound mediated interactions are interactions for which a compound
  acts as a bridge between two elements. Since chemical compounds are
  not usually measured with high-throughput technology, they should be
  removed from the network to analyse gene signals. However, the trivial
  elimination of the compounds, without signal propagation, will
  strongly bias the topology interrupting the signals that pass through
  them. If element 'A' is linked to compound 'c' and compound 'c' is
  linked to element 'B', element 'A' should be linked to element
  'B'. Within the KGML format there are two different ways of describing
  a compound mediated interaction: i) direct interaction type=``PPrel''
  ('A' interacts whit 'B' through compound 'c' ) and ii) indirect one
  type=``PCrel'' ('A' interacts to compound 'c' and 'c' interacts with
  'B').

  Not all compounds are considered for the propagation because some of
  them (for example: H2O, ATP, ADP) are highly frequent in map
  descriptions and the signal propagation through them would lead to
  chains too long.  Compounds not considered for propagation are not
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
