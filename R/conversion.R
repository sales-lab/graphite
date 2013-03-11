# Copyright 2011 Gabriele Sales <gabriele.sales@unipd.it>
#
#
# This file is part of graphite.
#
# graphite is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License
# version 3 as published by the Free Software Foundation.
#
# graphite is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with graphite. If not, see <http://www.gnu.org/licenses/>.


convertIdentifiers <- function(pathway, type) {
  if (pathway@ident != "native")
    stop("can only work on a pathway with native identifiers")

  ns     <- nodes(pathway)
  es     <- edges(pathway)

  if (type == "entrez") {
    ns <- nodesToEntrez(ns)
    es <- edgesToEntrez(es)
    id <- "Entrez Gene"
  } else if (type == "symbol") {
    ns <- nodesToSymbol(ns)
    es <- edgesToSymbol(es)
    id <- "Gene Symbol"
  } else 
    stop("unknown identifier type: ", type)

  pathway@nodes <- ns
  pathway@edges <- es
  pathway@ident <- id
  return(pathway)
}

nodesToEntrez <- function(v) {
  unique(safe.unlist(any2entrez(v, length(v))))
}

edgesToEntrez <- function(m) {
  nr <- NROW(m)
  if (nr == 0)
    return(m)

  leftIds  <- any2entrez(m[,1], nr)
  rightIds <- any2entrez(m[,2], nr)

  convL <- lapply(seq_along(leftIds), function(i) {
    l <- leftIds[[i]]
    r <- rightIds[[i]]

    if (!is.null(l) && !is.null(r)) {
      suf <- as.matrix(m[i, -c(1,2)])
      pairMatrix(l, r, suf)
    }
  })

  es <- unique(do.call(rbind, Filter(Negate(is.null), convL)))
  data.frame(src=safe.unlist(es[,1]),
             dest=safe.unlist(es[,2]),
             direction=factor(es[,3]),
             type=factor(es[,4]),
             stringsAsFactors=FALSE)
}

nodesToSymbol <- function(v) {
  conv <- nodesToEntrez(v)
  na.omit(unique(safe.unlist(mget(conv, org.Hs.egSYMBOL, ifnotfound=NA))))
}

edgesToSymbol <- function(m) {
  if (NROW(m) == 0)
    return(m)

  conv <- edgesToEntrez(m)
  if (NROW(conv) == 0)
    return(conv)

  conv$src  <- unlist(mget(conv$src,  org.Hs.egSYMBOL, ifnotfound=NA))
  conv$dest <- unlist(mget(conv$dest, org.Hs.egSYMBOL, ifnotfound=NA))
  na.omit(conv)
}


any2entrez <- function(v, n) {
  l <- splitIds(v)

  res <- vector("list", n)
  res <- copy(l$`EntrezGene`, res)
  res <- convert(l$`ENSEMBL`, org.Hs.egENSEMBLTRANS2EG, res)
  res <- convert(l$`EnzymeConsortium`, revmap(org.Hs.egENZYME), res)
  res <- convert(dropIsoformLabel(l$`UniProt`), revmap(org.Hs.egUNIPROT), res)

  return(res)
}

splitIds <- function(v) {
  splitted  <- strsplit(v, ":", fixed=TRUE)
  filtered  <- lapply(splitted, function(x) if (length(x) == 2) x else rep(NA,2))

  paired    <- data.frame(seq_along(filtered), do.call(rbind, filtered), stringsAsFactors=FALSE)
  colnames(paired) <- c("idx", "type", "id")

  partition(paired, 2)
}

dropIsoformLabel <- function(df) {
  df$id <- sub("-.*", "", df$id)
  return(df)
}

copy <- function(info, res) {
  for (i in seq_len(NROW(info)))
    res[[ info$idx[i] ]] <- info$id[i]
  return(res)
}

convert <- function(info, map, res) {
  if (NROW(info) == 0)
    return(res)
  
  converted <- mget(info$id, map, ifnotfound=NA)
  for (i in seq_along(converted)) {
    conv <- converted[[i]]
    if (length(conv) > 1 || !is.na(conv[1]))
      res[[ info$idx[i] ]] <- conv
  }
  
  return(res)
}


pairMatrix <- function(l, r, suf) {
  stopifnot(length(l) >= 1 && length(r) >= 1)
  
  lRep <- rep(l, length(r))
  rRep <- rep(r, each=length(l))

  t(sapply(seq_along(lRep), function(j) {
    buildEdge(lRep[j], rRep[j], suf)
  }))
}

buildEdge <- function(l, r, suf) {
  if (suf[1] == "undirected" && l > r)
    c(r, l, suf)
  else
    c(l, r, suf)
}


partition <- function(m, col) {
  tapply(seq_len(NROW(m)), m[,col], function(idxs) m[idxs,-col], simplify=FALSE)
}

safe.unlist <- function(l) {
  v <- unlist(l)
  if (is.null(v))
    character(0)
  else
    v
}
