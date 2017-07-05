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


setGeneric("prepareSPIA",
  function(db, pathwaySetName, print.names = FALSE)
    standardGeneric("prepareSPIA"))


setMethod("prepareSPIA", "PathwayList",
  function(db, pathwaySetName, print.names)
    .prepareSPIA(db@entries, pathwaySetName, print.names))

setMethod("prepareSPIA", "DeprecatedPathwayList",
  function(db, pathwaySetName, print.names) {
    deprecatedObj(db@name)
    .prepareSPIA(db@content, pathwaySetName, print.names)
  })

setMethod("prepareSPIA", "list",
  function(db, pathwaySetName, print.names) {
    checkPathwayList(db)
    .prepareSPIA(db, pathwaySetName, print.names)
  })

.prepareSPIA <- function(db, pathwaySetName, print.names) {
  path.info <- lapply(db, function(p) {
    if (print.names)
      cat(p@title, "\n")

    translated <- translateEdges(p)
    if (is.null(translated) || nrow(translated$edges) < 5) {
      return(NULL)
    }

    l <- sapply(spiaAttributes, edgeMatrix(translated$nodes, translated$edges),
                simplify=FALSE, USE.NAMES=TRUE)
    l$title <- p@title
    l$nodes <- translated$nodes
    l$NumberOfReactions <- 0
    return(l)
  })

  names(path.info) <- sapply(db, pathwayTitle)
  path.info <- Filter(Negate(is.null), path.info)
  save(path.info, file = datasetName(pathwaySetName))
}

translateEdges <- function(pathway) {
  es <- edges(pathway, which = "proteins")
  if (nrow(es) == 0)
    return(NULL)

  # Prefix all nodes with their type and drop the "_type" columns.
  es$src <- paste(es$src_type, es$src, sep = ":")
  es$dest <- paste(es$dest_type, es$dest, sep = ":")
  es <- es[, !(colnames(es) %in% c("src_type", "dest_type"))]

  # Convert edge types to the SPIA vocabulary.
  converted <- merge(es, spiaConv, all.x = TRUE)
  checkEdgeTypes(pathway@title, converted)

  # Drop the original "type" column.
  converted <- converted[, colnames(converted) != "type"]

  # Rename the column "spiaType" to "type".
  colnames(converted)[colnames(converted) == "spiaType"] <- "type"

  # Convert node labels to factors.
  nodes <- union(unique(converted$src), unique(converted$dest))
  converted$src <- factor(converted$src, levels = nodes)
  converted$dest <- factor(converted$dest, levels = nodes)

  list(nodes = nodes, edges = converted)
}

checkEdgeTypes <- function(title, edges) {
  nas <- is.na(edges$spiaType)
  if (any(nas)) {
    types <- sort(unique(edges$type[nas]))

    stop(paste0("Pathway \"", title, "\" contains edges with types ",
                "not supported by SPIA: ", paste(types, collapse = ", ")))
  }
}

edgeMatrix <- function(nodes, edges) {
  function(type) {
    selected <- edges[edges$type == type, , drop=FALSE]
    srcs <- as.integer(selected$src)
    dests <- as.integer(selected$dest)

    m <- matrix(data = 0, nrow = length(nodes), ncol = length(nodes),
                dimnames = list(nodes, nodes))
    for (i in seq_along(srcs)) {
      m[srcs[i], dests[i]] <- 1
    }

    m
  }
}


runSPIA <- function(de, all, pathwaySetName, ...) {
  requirePkg("SPIA")
  checkPkgVersion("SPIA", "2.18")

  if (!(datasetName(pathwaySetName) %in% dir()))
    stop("There is no dataset corresponding to the pathway set name: ",
         pathwaySetName, "\n",
         "Did you forget to run prepareSPIA?")

  optArgs <- list(...)
  if (!is.null(optArgs$organism))
    warning("Ignoring the \"organism\" parameter.")
  if (!is.null(optArgs$data.dir))
    warning("Ignoring the \"data.dir\" parameter.")

  optArgs$organism <- pathwaySetName
  optArgs$data.dir <- paste0(dirname(pathwaySetName), "/")

  do.call(SPIA::spia, c(list(de, all), optArgs))[,c(-2,-12)]
}

datasetName <- function(pathwaySetName)
  paste(pathwaySetName, "SPIA.RData", sep="")
