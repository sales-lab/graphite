# Copyright 2011-2018 Gabriele Sales <gabriele.sales@unipd.it>
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


# Packages

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


# Checks & filters

filterPathwaysByNodeNum <- function(pathways, maxNodes) {
  if (!is.null(maxNodes))
    pathways <- Filter(function(p) length(nodes(p)) <= maxNodes, pathways)

  return(pathways)
}

insufficientCommonNodes <- function(pathway, exprNodes, which) {
  commonNames <- intersect(nodes(pathway, which), exprNodes)

  if (length(commonNames) < 2) {
    warning("not enough genes in common between pathway \"",
            pathway@title,
            "\" and expression data (mismatched identifiers?)")
    return(TRUE)
  } else
    return(FALSE)
}

checkPathwayList <- function(l) {
  if (!all(sapply(l, function(e) is(e, "Pathway"))))
    stop("can only process a list of Pathways")
}


# Parallelism

parallelCluster <- function(tasks, type = c("auto", "psock")) {
  type <- match.arg(type)

  ncpus <- getOption("Ncpus")
  parallel <- is.numeric(ncpus) && ncpus > 1 &&
    length(tasks) >= ncpus

  if (parallel) {
    if (requireNamespace("parallel", quietly = TRUE)) {
      if (type == "psock" || .Platform$OS.type != "unix") {
        return(parallel::makePSOCKcluster(ncpus))
      } else {
        return(parallel::makeForkCluster(ncpus))
      }
    } else {
      message("This task could run in parallel. To use multiple cores in ",
              "parallel, please install the \"parallel\" package.")
    }
  }

  NULL
}


adaptiveLapply <- function(tasks, f, ...) {
  cl <- parallelCluster(tasks)
  if (is.null(cl)) {
    log <- lapply(tasks, wrapFun(f), ...)
  } else {
    on.exit(parallel::stopCluster(cl), add = TRUE)
    log <- parallel::parLapply(cl, tasks, wrapFun(f), ...)
  }

  succeeded <- sapply(log, function(x) x$success)
  list(results  = viewNonNull(log[succeeded], function(x) x$value),
       warnings = viewNonNull(log[succeeded], function(x) x$warnings),
       errors   = sapply(log[!succeeded], function(x) gettext(x$error)))
}

wrapFun <- function(f) {
  function(...) {
    tryCatch({
      warns <- NULL
      value <- withCallingHandlers(
        f(...),
        warning = function(w) {
          warns <<- c(warns, w)
          invokeRestart("muffleWarning")
        })

      list(success = TRUE,
           value = value,
           warnings = warns)
    },
    error = function(e) {
      list(success = FALSE,
           error = e)
    })
  }
}

viewNonNull <- function(items, f) {
  Filter(Negate(is.null), lapply(items, f))
}


# Others

literalDataFrame <- function(cnames, data) {
  m <- matrix(data, ncol = length(cnames), byrow = TRUE)
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  colnames(df) <- cnames
  df
}

nameLapply <- function(l, f) {
  ns <- names(l)
  for (i in seq_along(ns)) {
    l[[i]] <- f(ns[i], l[[i]])
  }
  return(l)
}
