# Copyright 2016-2017 Gabriele Sales <gabriele.sales@unipd.it>
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
                         timestamp = NULL) {

  assertString(title, min.chars = 1)
  assertString(species, min.chars = 1)
  assertString(database, min.chars = 1)
  assertEdges(proteinEdges, substitute(proteinEdges), TRUE)
  assertEdges(metaboliteEdges, substitute(metaboliteEdges), FALSE)
  assertEdges(mixedEdges, substitute(mixedEdges), FALSE)

  if (is.null(timestamp)) {
    timestamp <- Sys.Date()
  } else {
    assertDate(timestamp)
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

assertEdges <- function(edges, varName, required) {
  varName <- deparse(varName)

  if (required || !is.null(edges)) {
    assertDataFrame(edges, any.missing = FALSE,
                    types = c("character", "factor"), ncols = 6,
                    .var.name = varName)

    cnames <- c("src_type", "src", "dest_type", "dest", "direction", "type")
    assertNames(colnames(edges), permutation.of = cnames, .var.name = varName)

    assertCharacter(as.character(edges$direction), pattern = '^(un)?directed$',
                    .var.name = paste0(varName, "$direction"))
  }
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
