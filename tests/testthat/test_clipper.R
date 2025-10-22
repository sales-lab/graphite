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
  library(clipper)
  library(ALL)
  library(a4Preproc)
})

context("Clipper analysis")


# Utility functions

load_data <- function() {
  data(ALL)
  pheno <- as(phenoData(ALL), "data.frame")
  samples <- unlist(lapply(c("NEG", "BCR/ABL"), function(t) {
    which(grepl("^B\\d*", pheno$BT) & (pheno$mol.biol == t))[1:10]
  }))
  classes <- c(rep(1,10), rep(2,10))

  expr <- exprs(ALL)[,samples]
  rownames(expr) <- paste("ENTREZID", featureData(addGeneInfo(ALL))$ENTREZID,
                          sep = ":")

  list(classes = classes,
       expr = expr)
}

load_pathways <- function() {
  k <- as.list(pathways("hsapiens", "kegg"))
  k[c("Bladder cancer", "Hippo signaling pathway - multiple species")]
}


# Tests

dat <- load_data()
paths <- load_pathways()

test_that("pathway \"Hippo signaling pathway - multiple species\" is altered in the ALL dataset", {
  x <- runClipper(paths, dat$expr, dat$classes, "mean", pathThr = 0.1, seed = 42)

  expect_named(x, c("results", "warnings", "errors"))
  expect_length(x$errors, 0)
  expect_gte(length(x$results), 1)
  expect_equal("Hippo signaling pathway - multiple species" %in% names(x$results), TRUE)
})

test_that("parallel and serial analyses of a PathwayList produce the same results", {
  expect_equal(length(paths), 2)

  ncpus <- getOption("Ncpus")
  on.exit(options(Ncpus = ncpus), add = TRUE)

  run <- function() runClipper(paths, dat$expr, dat$classes, "mean",
                               pathThr = 0.1, seed = 42)

  options(Ncpus = 1)
  serial <- run()

  options(Ncpus = 2)
  parallel <- run()

  expect_identical(serial, parallel)
})
