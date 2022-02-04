# Copyright 2016-2022 Gabriele Sales <gabriele.sales@unipd.it>
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


buildPathway <- function(id, title, species, database, proteinEdges,
                         metaboliteEdges = NULL, mixedEdges = NULL,
                         timestamp = NULL)
{
  check_nonempty_string(id)
  check_nonempty_string(title)
  check_nonempty_string(species)
  check_nonempty_string(database)
  check_edges(proteinEdges, TRUE)
  check_edges(metaboliteEdges, FALSE)
  check_edges(mixedEdges, FALSE)

  if (is.null(timestamp)) {
    timestamp <- Sys.Date()
  } else {
    check_timestamp(timestamp)
  }

  fixedEdges <- fixEdges(proteinEdges, metaboliteEdges, mixedEdges)

  new("Pathway",
      id = id,
      title = title,
      species = species,
      database = database,
      protEdges = fixedEdges$prot,
      protPropEdges = fixedEdges$empty,
      metabolEdges = fixedEdges$metabol,
      metabolPropEdges = fixedEdges$empty,
      mixedEdges = fixedEdges$mixed,
      timestamp = timestamp)
}

check_nonempty_string <- function(value) {
  argument <- deparse(substitute(value))
  if (!is.character(value) || length(value) != 1 || nchar(value) == 0)
    rlang::abort(
      paste0("Argument `", argument, "` must be a non-empty string."),
      call = parent.frame())
}

check_edges <- function(edges, required) {
  argument <- deparse(substitute(edges))

  if (required || !is.null(edges)) {
    if (!is.data.frame(edges))
      rlang::abort(
        paste0("Argument `", argument, "` must be a `data.frame`."),
        call = parent.frame())

    expected <- c("src_type", "src", "dest_type", "dest", "direction", "type")
    found <- colnames(edges)
    if (length(found) != length(expected))
      rlang::abort(
        paste0("data.frame `", argument, "` must have ", length(expected),
               " columns."),
        call = parent.frame())

    for (cname in expected) {
      if (!(cname %in% found))
        rlang::abort(
          paste0("data.frame `", argument, "` lacks a column named `",
                 cname, "`."),
          call = parent.frame())

      if (!is.character(edges[[cname]]))
        rlang::abort(
          paste0("Column `", cname, "` of data.frame `", argument,
                 "` must be a character vector."),
          call = parent.frame())
    }

    if (!all(grepl("^(un)?directed$", edges$direction)))
      rlang::abort(
        c(paste0("Values in column `direction` of data.frame `", argument,
                 "` must be one of:"),
          "*" = "\"directed\"",
          "*" = "\"undirected\""),
        call = parent.frame())
  }
}

check_timestamp <- function(value) {
  argument <- deparse(substitute(value))
  if (!is(value, "Date") || length(value) != 1)
    rlang::abort(
      paste0("Argument `", argument, "` must be a Date instance."),
      call = parent.frame())
}

fixEdges <- function(prot, metabol, mixed) {
  nodeTypes <- union(unique(as.character(prot$src_type)),
                     unique(as.character(prot$dest_type)))
  edgeTypes <- unique(as.character(prot$type))

  if (!is.null(metabol)) {
    metabolTypes <- union(unique(as.character(metabol$src_type)),
                          unique(as.character(metabol$dest_type)))
    nodeTypes <- union(nodeTypes, metabolTypes)
    edgeTypes <- union(edgeTypes, unique(as.character(metabol$type)))
  }

  if (!is.null(mixed)) {
    mixedTypes <- union(unique(as.character(mixed$src_type)),
                        unique(as.character(mixed$dest_type)))
    nodeTypes <- union(nodeTypes, mixedTypes)
    edgeTypes <- union(edgeTypes, unique(as.character(mixed$type)))
  }

  prot$src_type <- factor(prot$src_type, levels = nodeTypes)
  prot$src <- as.character(prot$src)
  prot$dest_type <- factor(prot$dest_type, levels = nodeTypes)
  prot$dest <- as.character(prot$dest)
  prot$direction <- factor(prot$direction,
                           levels = c("directed", "undirected"))
  prot$type  <- factor(prot$type, levels = edgeTypes)

  empty <- prot[0,]

  if (is.null(metabol)) {
    metabol <- empty
  } else {
    metabol$src_type <- factor(metabol$src_type, levels = nodeTypes)
    metabol$src <- as.character(metabol$src)
    metabol$dest_type <- factor(metabol$dest_type, levels = nodeTypes)
    metabol$dest <- as.character(metabol$dest)
    metabol$direction <- factor(metabol$direction,
                                levels = c("directed", "undirected"))
    metabol$type <- factor(metabol$type, levels = edgeTypes)
  }

  if (is.null(mixed)) {
    mixed <- empty
  } else {
    mixed$src_type <- factor(mixed$src_type, levels = nodeTypes)
    mixed$src <- as.character(mixed$src)
    mixed$dest_type <- factor(mixed$dest_type, levels = nodeTypes)
    mixed$dest <- as.character(mixed$dest)
    mixed$direction <- factor(mixed$direction,
                              levels = c("directed", "undirected"))
    mixed$type <- factor(mixed$type, levels = edgeTypes)
  }

  list(prot = prot, metabol = metabol, mixed = mixed, empty = empty)
}
