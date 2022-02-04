# Copyright 2011-2022 Gabriele Sales <gabriele.sales@unipd.it>
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


pathwayGraph <- function(pathway, which = "proteins", edge.types = NULL) {
  if (!is(pathway, "Pathway"))
    rlang::abort(c(
      "The argument `pathway` must be an object belonging to class `Pathway`.",
      "i" = "Load some pathways invoking the function `pathways()`."
    ))

  buildGraphNEL(edges(pathway, which), TRUE, edge.types)
}

buildGraphNEL <- function(edges, sym, edge.types) {
  if (!is.null(edge.types))
    edges <- selectEdges(edges, edge.types)

  if (nrow(edges) == 0)
    g <- new("graphNEL", character(), list(), "directed")
  else {
    prep <- prepareEdges(edges, sym)
    nodes <- union(unique(prep$src), unique(prep$dest))

    g <- new("graphNEL", nodes, edgeList(nodes, prep), "directed")
    edgeDataDefaults(g, "edgeType") <- "undefined"
    edgeData(g, prep$src, prep$dest, "edgeType") <- prep$type
  }

  return(g)
}

selectEdges <- function(m, types) {
  missing <- setdiff(types, edgeInfo$type)
  if (length(missing) > 0) {
    stop("the following edge types are missing: ",
         paste(sort(missing), collapse=", "))
  }

  m[m$type %in% types,, drop = FALSE]
}

prepareEdges <- function(edges, sym) {
  edges[] <- lapply(edges, as.character)
  if (sym) {
    edges <- symmetric(edges)
  }

  ends <- endpoints(edges)
  types <- tapply(edges$type, ends, function(group) {
    paste(sort(unique(group)), collapse = ";")
  })

  binder <- function(...) rbind.data.frame(..., stringsAsFactors = FALSE)
  merged <- do.call(binder, strsplit(names(types), "|", fixed = TRUE))
  colnames(merged) <- c("src", "dest")

  cbind(merged,
        data.frame(type = as.character(types), stringsAsFactors = FALSE))
}

symmetric <- function(edges) {
  mask <- edges$direction == "undirected" &
          (edges$src_type != edges$dest_type | edges$src != edges$dest)

  dird <- edges[!mask,]
  undir <- edges[mask,]
  revdir <- edges[mask,]

  revdir$src_type <- undir$dest_type
  revdir$src <- undir$dest
  revdir$dest_type <- undir$src_type
  revdir$dest <- undir$src

  rbind(dird, undir, revdir)
}

endpoints <- function(edges) {
  paste(paste(edges$src_type, edges$src, sep = ":"),
        paste(edges$dest_type, edges$dest, sep = ":"),
        sep = "|")
}

edgeList <- function(nodes, edges) {
  sapply(nodes,
         function(n) list(edges=edges[edges$src == n, "dest"]),
         simplify=FALSE,
         USE.NAMES=TRUE)
}
