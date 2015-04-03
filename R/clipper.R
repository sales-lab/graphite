# Copyright 2013,2015 Gabriele Sales <gabriele.sales@unipd.it>
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


initClipper <- function() requirePkg("clipper")

.clipper <- function(pathway, expr, classes, method, ...) {
  genes <- rownames(expr)
  if (insufficientCommonGenes(pathway, genes))
    return(NULL)

  g <- buildGraphNEL(nodes(pathway), edges(pathway), FALSE)
  clipper::easyClip(expr, classes, g, method=method, ...)
}

.clipperList <- function(l, expr, classes, method, maxNodes=150, ...) {
  initClipper()
  lapplyCapturingErrors(filterPathwaysByNodeNum(l, maxNodes),
    function(p) .clipper(p, expr, classes, method, ...))
}


setGeneric("runClipper",
  function(x, expr, classes, method, ...)
    standardGeneric("runClipper"))

setMethod("runClipper", "PathwayList",
  function(x, expr, classes, method, maxNodes=150, ...) {
    .clipperList(x@entries, expr, classes, method, maxNodes, ...)
  })

setMethod("runClipper", "DeprecatedPathwayList",
  function(x, expr, classes, method, maxNodes=150, ...) {
    deprecatedObj(x@name)
    runClipper(x@content, expr, classes, method, maxNodes, ...)
  })

setMethod("runClipper", "list",
  function(x, expr, classes, method, maxNodes=150, ...) {
    checkPathwayList(x)
    .clipperList(x, expr, classes, method, maxNodes, ...)
  })

setMethod("runClipper", "Pathway",
  function(x, expr, classes, method, ...) {
    initClipper()
    .clipper(x, expr, classes, method, ...)
  })


runClipperMulti <- function(pathways, expr, classes, method, maxNodes=150, ...) {
  deprecatedFn("runClipperMulti", "runClipper")
  runClipper(pathways, expr, classes, method, maxNodes, ...)
}
