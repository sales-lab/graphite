# Copyright 2013-2017 Gabriele Sales <gabriele.sales@unipd.it>
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

.clipper <- function(pathway, expr, classes, method, which, ...) {
  ns <- rownames(expr)
  if (insufficientCommonNodes(pathway, ns, which))
    return(NULL)

  g <- buildGraphNEL(edges(pathway, which), FALSE, NULL)
  clipper::easyClip(expr, classes, g, method=method, ...)
}

.clipperList <- function(l, expr, classes, method, which, maxNodes = 150, seed = NULL, ...) {
  initClipper()
  if (!is.null(seed)) set.seed(seed)
  lapplyCapturingErrors(filterPathwaysByNodeNum(l, maxNodes),
    function(p) .clipper(p, expr, classes, method, which, ...))
}


setGeneric("runClipper",
  function(x, expr, classes, method, which = "proteins", seed = NULL, ...)
    standardGeneric("runClipper"))

setMethod("runClipper", signature("PathwayList"),
  function(x, expr, classes, method, which = "proteins", seed = NULL, ...) {
    .clipperList(x@entries, expr, classes, method, which, seed = seed, ...)
  })

setMethod("runClipper", signature("DeprecatedPathwayList"),
  function(x, expr, classes, method, which = "proteins", seed = NULL, ...) {
    deprecatedObj(x@name)
    runClipper(x@content, expr, classes, method, which, seed, ...)
  })

setMethod("runClipper", signature("list"),
  function(x, expr, classes, method, which = "proteins", seed = NULL, ...) {
    checkPathwayList(x)
    .clipperList(x, expr, classes, method, which, seed = seed, ...)
  })

setMethod("runClipper", signature("Pathway"),
  function(x, expr, classes, method, which = "proteins", seed = NULL, ...) {
    initClipper()
    if (!is.null(seed)) set.seed(seed)
    .clipper(x, expr, classes, method, which, ...)
  })
