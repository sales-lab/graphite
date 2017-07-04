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


deprecatedObj <- function(name) {
  warning("Object \"", name, "\" is deprecated and will be removed from ",
          "the next release.\n",
          "Use the pathways() function instead.",
          call.=FALSE)
}

deprecatedFn <- function(name, repl) {
  warning("Function \"", name, "\" is deprecated and will be removed from ",
          "the next release.\n",
          "Use the \"", repl, "\" function instead.",
          call.=FALSE)
}


filterPathwaysByNodeNum <- function(pathways, maxNodes) {
  if (!is.null(maxNodes))
    pathways <- Filter(function(p) length(nodes(p)) <= maxNodes, pathways)

  return(pathways)
}


nameLapply <- function(l, f) {
  ns <- names(l)
  for (i in seq_along(ns)) {
    l[[i]] <- f(ns[i], l[[i]])
  }
  return(l)
}

lapplyCapturingErrors <- function(l, f) {
  log <- lapply(l, function(x) {
    tryCatch(list("ok", f(x)),
             error = function(e) list("err", e))
  })

  list(results = Filter(Negate(is.null), filterByTag("ok", log)),
       errors  = sapply(filterByTag("err", log), gettext))
}

filterByTag <- function(tag, l) {
  isTagged <- sapply(l, function(x) x[[1]] == tag)
  lapply(l[isTagged], function(x) x[[2]])
}


checkPathwayList <- function(l) {
  if (!all(sapply(l, function(e) is(e, "Pathway"))))
    stop("can only process a list of Pathways")
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


requirePkg <- function(name) {
  if (!requireNamespace(name, quietly=TRUE))
    stop("library ", name, " is missing", call.=FALSE)
}

checkPkgVersion <- function(name, min_version) {
  version <- package_version(installed.packages()[name, "Version"])
  if (version < package_version(min_version))
    stop("the installed ", name, " version is too old (need at least ",
         min_version, ")",
         call.=FALSE)
}
