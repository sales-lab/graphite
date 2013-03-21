# Copyright 2013 Gabriele Sales <gabriele.sales@unipd.it>
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


runClipper <- function(pathway, expr, classes, method, ...) {
  initClipper()
  runClipperSingle(pathway, expr, classes, method, ...)
}

runClipperMulti <- function(pathways, expr, classes, method, maxNodes=150, ...) {
  initClipper()
  pathways <- filterPathwaysByNodeNum(pathways, maxNodes)
  lapplyCapturingErrors(pathways, function(p) runClipperSingle(p, expr, classes, method, ...))
}

initClipper <- function() {
  if (!require(clipper))
    stop("library clipper is missing")
}

runClipperSingle <- function(pathway, expr, classes, method, ...) {
  genes <- rownames(expr)
  if (insufficientCommonGenes(pathway, genes))
    return(NULL)
  
  g <- buildGraphNEL(nodes(pathway), edges(pathway), FALSE)
  easyClip(expr, classes, g, method=method, ...)
}
