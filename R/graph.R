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


pathwayGraph <- function(pathway, edge.types=NULL) {
  buildGraphNEL(nodes(pathway), edges(pathway), TRUE, edge.types)
}

buildGraphNEL <- function(nodes, edges, sym, edge.types=NULL) {
  if (!is.null(edge.types))
    edges <- selectEdges(edges, edge.types)

  if (NROW(edges) == 0)
    g <- new("graphNEL", nodes, list(), "directed")
  else {
    edges <- prepareEdges(as.matrix(edges), sym)
    g <- new("graphNEL", nodes, edgeList(nodes, edges), "directed")
    edgeDataDefaults(g, "edgeType") <- "undefined"
    edgeData(g, edges[,1], edges[,2], "edgeType") <- edges[,3]
  }

  return(g)
}

selectEdges <- function(m, types) {
  unknownTypes <- setdiff(types, edgeTypes)
  if (length(unknownTypes))
    stop("the following edge types are invalid: ", paste(unknownTypes, collapse=", "))

  m[m[,4] %in% types,]
}

prepareEdges <- function(m, sym) {
  ns         <- canonicalEdgeNames(m)
  simplified <- matrix(unlist(tapply(1:NROW(m), ns, function(is) mergeEdges(m, is))),
                       ncol=4, byrow=T)

  if (sym)
    symmetricEdges(simplified)
  else
    simplified[, -3, drop=FALSE]
}

canonicalEdgeNames <- function(m) {
  apply(m, 1, function(e) {
    if (e[1] <= e[2])
      paste(e[1], e[2], sep="|")
    else
      paste(e[2], e[1], sep="|")
  })
}

mergeEdges <- function(m, is) {
  h <- m[is[1],]
  if (length(is) == 1)
    h
  else {
    if ("undirected" %in% m[is,3] || any(h[1]!=m[is,1]))
      dir <- "undirected"
    else
      dir <- "directed"

    c(h[1], h[2], dir, paste(unique(m[is,4]), collapse=";"))
  }
}

symmetricEdges <- function(m) {
  undirected <- m[m[,3]=="undirected" & m[,1]!=m[,2], c(2,1,4), drop=FALSE]

  if (NROW(undirected) > 0) {
    full <- m[, -3, drop=FALSE]
    stopifnot(is.null(dimnames(full)))
    rbind(full, undirected)
  } else
    return(m[, -3, drop=FALSE])
}

edgeList <- function(nodes, edges) {
  sapply(nodes,
         function(n) list(edges=edges[edges[,1]==n, 2]),
         simplify=FALSE,
         USE.NAMES=TRUE)
}
