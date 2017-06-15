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

suppressPackageStartupMessages(library(checkmate))

context("Pathway construction")

single_edge <- data.frame(src_type = "UNIPROT", src="Q9Y243",
                          dest_type = "CHEBI", dest="25212",
                          direction = "undirected", type = "binding")

test_that("invalid edges are rejected", {
  invalid <- single_edge
  invalid$extra <- "extra"

  expect_error(buildPathway("id", "example", "hsapiens", "database", invalid))
  expect_error(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, metaboliteEdges = invalid))
  expect_error(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, mixedEdges = invalid))

  invalid <- single_edge
  invalid$src <- 2
  expect_error(buildPathway("id", "example", "hsapiens", "database", invalid))
})

test_that("invalid directions are rejected", {
  invalid <- single_edge
  invalid$direction <- "invalid"

  expect_error(buildPathway("id", "example", "hsapiens", "database", invalid))
  expect_error(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, metaboliteEdges = invalid))
  expect_error(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, mixedEdges = invalid))
})

test_that("invalid timestamp is rejected", {
  expect_class(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, timestamp = Sys.Date()),
               "Pathway")

  expect_error(buildPathway("id", "example", "hsapiens", "database",
                            single_edge, timestamp = "timestamp"))
})

test_that("single-edge pathway is built as expected", {
  p <- buildPathway("#1", "example", "hsapiens", "database", single_edge)

  ns <- nodes(p)
  expect_equal(sort(ns), c("CHEBI:25212", "UNIPROT:Q9Y243"))

  es <- edges(p)
  expect_equal(nrow(es), 1)
  expect_factor(es$src_type, c("CHEBI", "UNIPROT"))
  expect_character(es$src)
  expect_factor(es$dest_type, c("CHEBI", "UNIPROT"))
  expect_character(es$dest)
  expect_factor(es$direction, c("directed", "undirected"))
  expect_factor(es$type, c("binding"))
})

test_that("edge information is converted to character when stringsAsFactors is FALSE", {
  p <- buildPathway("#1", "example", "hsapiens", "database", single_edge)

  esc <- edges(p, stringsAsFactors = FALSE)
  expect_character(esc$src_type)
  expect_character(esc$dest_type)
  expect_character(esc$direction)
  expect_character(esc$type)

  esAsChar <- edges(p, stringsAsFactors = TRUE)
  esAsChar[] <- lapply(esAsChar, as.character)
  expect_identical(esAsChar, esc)
})

test_that("mixed edges are selectively returned", {
  protein_edge <- single_edge
  protein_edge$dest_type <- "UNIPROT"
  protein_edge$dest <- "Q9H3D4"

  p <- buildPathway("#1", "example", "hsapiens", "database", protein_edge,
                    mixedEdges = single_edge)

  expect_length(nodes(p), 2)
  expect_equal(sort(nodes(p, "mixed")),
               c("CHEBI:25212", "UNIPROT:Q9H3D4", "UNIPROT:Q9Y243"))

  expect_equal(nrow(edges(p)), 1)
  expect_equal(nrow(edges(p, "metabolites")), 0)
  expect_equal(nrow(edges(p, "mixed")), 2)
})
