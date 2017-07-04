# Copyright 2011-2017 Gabriele Sales <gabriele.sales@unipd.it>
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


initDEGraph <- function() {
  requirePkg("DEGraph")
  checkPkgVersion("DEGraph", "1.4.0")
}

.degraph <- function(pathway, expr, classes) {
  if (insufficientCommonGenes(pathway, rownames(expr)))
    return(NULL)

  g <- buildGraphNEL(edges(pathway), FALSE, NULL)
  DEGraph::testOneGraph(g, expr, classes, useInteractionSigns=FALSE)
}

.degraphList <- function(l, expr, classes, maxNodes=150) {
  initDEGraph()
  lapplyCapturingErrors(filterPathwaysByNodeNum(l, maxNodes),
    function(p) .degraph(p, expr, classes))
}


setGeneric("runDEGraph",
  function(x, expr, classes, ...) standardGeneric("runDEGraph"))


setMethod("runDEGraph", "PathwayList",
  function(x, expr, classes, maxNodes=150) {
    .degraphList(x@entries, expr, classes, maxNodes)
  })

setMethod("runDEGraph", "DeprecatedPathwayList",
  function(x, expr, classes, maxNodes=150) {
    deprecatedObj(x@name)
    runDEGraph(x@content, expr, classes)
  })

setMethod("runDEGraph", "list",
  function(x, expr, classes, maxNodes=150) {
    checkPathwayList(x)
    .degraphList(x@entries, expr, classes, maxNodes)
  })

setMethod("runDEGraph", "Pathway",
  function(x, expr, classes) {
    initDEGraph()
    .degraph(x, expr, classes)
  })


runDEGraphMulti <- function(pathways, expr, classes, maxNodes=150) {
  deprecatedFn("runDEGraphMulti", "runDEGraph")
  runDEGraph(pathways, expr, classes, maxNodes)
}
