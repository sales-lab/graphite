# Copyright 2011-2022 Gabriele Sales <gabriele.sales@unipd.it>
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

setMethod("prepareSPIA", "list",
  function(db, pathwaySetName, print.names) {
    errors <- checkPathwayList(db)
    if (length(errors))
      stop(paste("There was an error with the list of pathways you provided:",
                 errors[[1]],
                 sep = " "))

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

    out <- lapply(spiaAttributes,
                  edgeMatrix(translated$nodes, translated$edges))
    names(out) <- spiaAttributes
    out$title <- p@title
    out$nodes <- translated$nodes
    out$NumberOfReactions <- 0
    return(out)
  })

  path.info <- Filter(Negate(is.null), path.info)
  if (length(path.info) == 0)
    stop("Your pathway list does not include at least one pathway with 5 edges or more.")

  # Add a fake pathway containing at least one activation.
  path.info <- c(path.info, list(spiaFakePathway(path.info[[1]]$nodes[1:2])))

  names(path.info) <- vapply(path.info, function(i) i[["title"]], "")
  save(path.info, file = datasetName(pathwaySetName))
}

translateEdges <- function(pathway) {
  es <- edges(pathway, which = "proteins")
  if (nrow(es) == 0)
    return(NULL)

  # Prefix all nodes with their type.
  es$src <- paste(es$src_type, es$src, sep = ":")
  es$dest <- paste(es$dest_type, es$dest, sep = ":")

  # Convert edge types to the SPIA vocabulary.
  converted <- merge(es, edgeInfo, all.x = TRUE)
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

spiaFakePathway <- function(nodes) {
  edges <- data.frame(src = 1, dest = 2, type = "activation")

  out <- lapply(spiaAttributes, edgeMatrix(nodes, edges))
  names(out) <- spiaAttributes
  out$title <- "<graphite_placeholder>"
  out$nodes <- nodes
  out$NumberOfReactions <- 0
  return(out)
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

  out <- do.call(SPIA::spia, c(list(de, all), optArgs))
  out[out$Name != "<graphite_placeholder>", c(-2,-12)]
}

datasetName <- function(pathwaySetName)
  paste(pathwaySetName, "SPIA.RData", sep="")
