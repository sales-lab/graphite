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


runTopologyGSA <- function(pathway, test, exp1, exp2, ...) {
  if (!require(topologyGSA))
    stop("library topologyGSA is missing")

  checkPkgVersion("topologyGSA", "1.0")

  if (test == "var") {
    test <- pathway.var.test
  } else if (test == "mean") {
    test <- pathway.mean.test
  } else {
    stop("invalid test type: ", test)
  }

  if (insufficientCommonGenes(pathway, colnames(exp1)))
    return(NULL)

  g <- buildGraphNEL(nodes(pathway), edges(pathway), FALSE)
  test(exp1, exp2, g, ...)
}
