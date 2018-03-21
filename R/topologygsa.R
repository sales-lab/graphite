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


initTopologyGSA <- function() {
  requirePkg("topologyGSA")
  checkPkgVersion("topologyGSA", "1.0")
}

switchTest <- function(name) {
  switch(name,
         var  = topologyGSA::pathway.var.test,
         mean = topologyGSA::pathway.mean.test,
         stop("invalid test type: ", name))
}

.topologyGSA <- function(pathway, test, exp1, exp2, alpha, ...) {
  if (insufficientCommonNodes(pathway, colnames(exp1), "proteins"))
    return(NULL)

  g <- buildGraphNEL(edges(pathway), FALSE, NULL)
  test(exp1, exp2, g, alpha, ...)
}

.topologyGSAList <- function(l, test, exp1, exp2, alpha, maxNodes=150, ...) {
  initTopologyGSA()
  test <- switchTest(test)
  lapplyCapturingErrors(filterPathwaysByNodeNum(l, maxNodes),
    function(p) .topologyGSA(p, test, exp1, exp2, alpha, ...))
}


setGeneric("runTopologyGSA",
  function(x, test, exp1, exp2, alpha, ...)
    standardGeneric("runTopologyGSA"))


setMethod("runTopologyGSA", "PathwayList",
  function(x, test, exp1, exp2, alpha, maxNodes=150, ...) {
    .topologyGSAList(x@entries, test, exp1, exp2, alpha, maxNodes, ...)
  })

setMethod("runTopologyGSA", "list",
  function(x, test, exp1, exp2, alpha, maxNodes=150, ...) {
    checkPathwayList(x)
    .topologyGSAList(x, test, exp1, exp2, alpha, maxNodes)
  })


setMethod("runTopologyGSA", "Pathway",
  function(x, test, exp1, exp2, alpha, ...) {
    initTopologyGSA()
    .topologyGSA(x, switchTest(test), exp1, exp2, alpha, ...)
  })
