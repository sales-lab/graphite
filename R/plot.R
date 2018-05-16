# Copyright 2011-2018 Gabriele Sales <gabriele.sales@unipd.it>
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


if (!isGeneric("plot"))
  setGeneric("plot", function(x, ...)
    standardGeneric("plot"))

setMethod("plot", signature("Pathway"),
  function(x, ...) cytoscapePlot(x, ...))


cytoscapePlot <- function(pathway, ..., cy.ver=3) {
  if (cy.ver == 3) {
    cytoscapePlot3(pathway, ...)
  } else {
    stop("unsupported Cytoscape version")
  }
}

cytoscapePlot3 <- function(pathway, ...) {
  requirePkg("RCy3")

  try(RCy3::deleteNetwork(pathway@title), silent = TRUE)

  g <- buildGraphNEL(edges(pathway, ...), FALSE, NULL)
  suid <- RCy3::createNetworkFromGraph(g, pathway@title)
  setNodeAttributes(g)
  setEdgeAttributes(g)

  RCy3::setNodeLabelMapping("label")
  RCy3::setNodeShapeMapping("type", nodeShape$type, nodeShape$shape)
  RCy3::setEdgeTargetArrowMapping("edgeType",
                                  c(edgeInfo$type, "multiple"),
                                  c(edgeInfo$arrow, "NONE"),
                                  "NONE")

  invisible(list(graph = g, suid = suid))
}

setNodeAttributes <- function(g) {
  ns <- nodes(g)

  attrs <- data.frame(
    id = ns,
    label =  sub("^[^:]*:", "", ns),
    type = sub(":.*", "", ns),
    stringsAsFactors = FALSE)

  RCy3::loadTableData(attrs, "id", "node")
}

setEdgeAttributes <- function(g) {
  attrs <- markMultipleEdges(g)
  RCy3::loadTableData(attrs, "id", "edge")
}

markMultipleEdges <- function(g) {
  isMultiple <- sapply(edgeData(g), function(e) {
    type <- e$edgeType
    length(grep(";", type, fixed = TRUE)) > 0
  })
  
  edgeName <- function(nodes) {
    ns <- unlist(strsplit(nodes, "|", fixed = TRUE))
    paste(ns[1], "(interacts with)", ns[2])
  }

  ns <- names(isMultiple[isMultiple])
  data.frame(id = vapply(ns, edgeName, ""),
             edgeType = rep.int("multiple", length(ns)),
             stringsAsFactors = FALSE)
}
