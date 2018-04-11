# Copyright 2017-2018 Gabriele Sales <gabriele.sales@unipd.it>
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

suppressPackageStartupMessages({
  library(topologyGSA)
})

context("topologyGSA analysis")


# Utility functions

load_data <- function() {
  data(examples)
  colnames(y1) <- paste("SYMBOL", colnames(y1), sep = ":")
  colnames(y2) <- paste("SYMBOL", colnames(y2), sep = ":")
  
  list(y1 = y1,
       y2 = y2)
}

load_pathways <- function() {
  k <- pathways("hsapiens", "kegg")
  convertIdentifiers(k[c("Glycolysis / Gluconeogenesis",
                         "Fc epsilon RI signaling pathway")],
                     "SYMBOL")
}


# Tests

dat <- load_data()
paths <- load_pathways()

test_that("pathway \"Fc epsilon RI signaling pathway\" is significantly altered", {
  x <- runTopologyGSA(paths, "var", dat$y1, dat$y2, 0.05)

  expect_named(x, c("results", "warnings", "errors"))
  expect_length(x$errors, 0)
  expect_gte(length(x$results), 1)
  expect_equal("Fc epsilon RI signaling pathway" %in% names(x$results), TRUE)
})

test_that("parallel and serial analyses of a PathwayList produce the same results", {
  expect_equal(length(paths), 2)

  ncpus <- getOption("Ncpus")
  on.exit(options(Ncpus = ncpus), add = TRUE)

  run <- function() runTopologyGSA(paths, "var", dat$y1, dat$y2, 0.05)

  options(Ncpus = 1)
  serial <- run()

  options(Ncpus = 2)
  parallel <- run()

  expect_identical(serial, parallel)
})
