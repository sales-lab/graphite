# Copyright 2017 Gabriele Sales <gabriele.sales@unipd.it>
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

suppressPackageStartupMessages(library(graph))

context("Graph construction")

mkEdges <- function(src, dest, types) {
  stopifnot(length(src) == length(dest))
  stopifnot(length(src) == length(types))

  nodes <- union(unique(src), unique(dest))
  nodeTypes <- factor(rep("ENTREZID", length(src)), levels = "ENTREZID")

  data.frame(src_type = nodeTypes, src = factor(src, levels = nodes),
             dest_type = nodeTypes, dest = factor(dest, levels = nodes),
             direction = factor(ifelse(types == "binding", "undirected", "directed"),
                                levels = c("directed", "undirected")),
             type = factor(types))
}

mkPrep <- function(src, dest, types) {
  stopifnot(length(src) == length(dest))
  stopifnot(length(src) == length(types))

  data.frame(src = paste("ENTREZID", src, sep = ":"),
             dest = paste("ENTREZID", dest, sep = ":"),
             type = types,
             stringsAsFactors = FALSE)
}

expectSameEdges <- function(object, expected) {
  expect_equal(colnames(object), colnames(expected))

  object <- object[order(endpoints(object)),]
  expected <- expected[order(endpoints(expected)),]
  expect_equivalent(object, expected)
}

test_that("Unique edges are preserved", {
  spec <- list(c("1", "2", "3"), c("2", "1", "1"), rep("activation", 3))
  prep <- prepareEdges(do.call(mkEdges, spec), TRUE)
  expectSameEdges(prep, do.call(mkPrep, spec))
})

test_that("Redundant edges disappear", {
  edges <- mkEdges(c("1", "2", "1"), c("2", "1", "2"), rep("activation", 3))
  uniq <- mkPrep(c("1", "2"), c("2", "1"), rep("activation", 2))

  prep <- prepareEdges(edges, TRUE)
  expectSameEdges(prep, uniq)
})

test_that("Directed edges are merged", {
  edges <- mkEdges(c("1", "2", "1"), c("2", "1", "2"),
                   c("activation", "activation", "inhibition"))
  merged <- mkPrep(c("1", "2"), c("2", "1"),
                   c("activation;inhibition", "activation"))

  prep <- prepareEdges(edges, TRUE)
  expectSameEdges(prep, merged)
})

test_that("Mixed edges are merged", {
  edges <- mkEdges(c("1", "2", "1"), c("2", "1", "3"),
                   c("activation", "binding", "inhibition"))
  merged <- mkPrep(c("1", "2", "1"), c("2", "1", "3"),
                   c("activation;binding", "binding", "inhibition"))

  prep <- prepareEdges(edges, TRUE)
  expectSameEdges(prep, merged)
})

test_that("graphNEL is built", {
  ns <- c("ENTREZID:1", "ENTREZID:2", "ENTREZID:3")
  es <- mkEdges(c("1", "1", "2", "3"), c("2", "3", "1", "1"), rep("activation", 4))

  graph <- buildGraphNEL(es, TRUE, NULL)
  expect_is(graph, "graphNEL")
  expect_equal(sort(nodes(graph)), ns)

  graphEdges <- edges(graph)
  expect_list(graphEdges)
  expect_equal(sum(sapply(graphEdges, length)), 4)
})

test_that("graphNEL includes metabolites from pathway", {
  path <- pathways("hsapiens", "kegg")[["Pentose phosphate pathway"]]
  expect_count(nrow(edges(path, "mixed")), positive = TRUE)

  graph <- pathwayGraph(path, "mixed")
  expect_equal(sort(nodes(graph)), sort(nodes(path, "mixed")))
})

test_that("Selection of an invalid type of edge is rejected", {
  path <- pathways("hsapiens", "kegg")[[1]]
  expect_error(pathwayGraph(path, edge.types = "no_such_type"),
               "the following edge types are missing: no_such_type")
})
