# Copyright 2011-2012 Gabriele Sales <gabriele.sales@unipd.it>
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


cytoscapePlot <- function(pathway, ...) {
  if (!require(RCytoscape))
    stop("the RCytoscape package is missing")

  g <- buildGraphNEL(nodes(pathway), edges(pathway), FALSE, ...)
  g <- markMultiple(g)
  g <- initEdgeAttribute(g, "edgeType", "char", "undefined")
  g <- initEdgeAttribute(g, "weight", "numeric", 1)

  cy <- CytoscapeConnection()
  if (pathway@title %in% as.character(getWindowList(cy)))
    deleteWindow(cy, pathway@title)

  w <- new.CytoscapeWindow(pathway@title, g)
  displayGraph(w)

  setEdgeTargetArrowRule(w, "edgeType", c(edgeTypes, "multiple"), c(edgeArrows, "No Arrow"))
  layoutNetwork(w)
  redraw(w)
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
