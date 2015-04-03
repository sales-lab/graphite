# Copyright 2011-2012,2015 Gabriele Sales <gabriele.sales@unipd.it>
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

setMethod("plot", "Pathway",
  function(x, ...) cytoscapePlot(x, ...))


cytoscapePlot <- function(pathway, ...) {

  requirePkg("RCytoscape")

  g <- buildGraphNEL(nodes(pathway), edges(pathway), FALSE, ...)
  g <- markMultiple(g)
  g <- RCytoscape::initEdgeAttribute(g, "edgeType", "char", "undefined")
  g <- RCytoscape::initEdgeAttribute(g, "weight", "numeric", 1)

  cy <- RCytoscape::CytoscapeConnection()
  if (pathway@title %in% as.character(RCytoscape::getWindowList(cy)))
    RCytoscape::deleteWindow(cy, pathway@title)

  w <- RCytoscape::new.CytoscapeWindow(pathway@title, g)
  RCytoscape::displayGraph(w)

  RCytoscape::setEdgeTargetArrowRule(w, "edgeType",
                                     c(edgeTypes, "multiple"),
                                     c(edgeArrows, "No Arrow"))
  RCytoscape::layoutNetwork(w)
  RCytoscape::redraw(w)
}

markMultiple <- function(g) {
  d <- edgeData(g)
  if (length(d) == 0)
    return(g)

  ns <- names(d)
  for (i in 1:length(d)) {
    tp <- d[[i]]$edgeType
    if (length(grep(";", tp, fixed=T)) > 0) {
      nodes <- unlist(strsplit(ns[[i]], "|", fixed=T))
      edgeData(g, nodes[1], nodes[2], "edgeType") <- "multiple"
    }
  }

  return(g)
}
