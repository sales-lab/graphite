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

checkPkgVersion <- function(name, min_version) {
  version <- package_version(installed.packages()[name, "Version"])
  if (version < package_version(min_version))
    stop("the installed ", name, " version is too old (need at least ", min_version, ")")
}

insufficientCommonGenes <- function(pathway, exprGenes) {
  commonNames <- intersect(nodes(pathway), exprGenes)

  if (length(commonNames) < 2) {
    warning("not enough genes in common between pathway \"",
            pathway@title,
            "\" and expression data (mismatched identifiers?)")
    return(TRUE)
  } else
    return(FALSE)
}
