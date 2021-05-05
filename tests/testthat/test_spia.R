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

suppressPackageStartupMessages({
  require(SPIA)
  require(hgu133plus2.db)
  library(checkmate)
})

context("SPIA analysis")

test_that("analysis result contains one row for each pathway", {
  data(colorectalcancer)

  top$ENTREZ <- mapIds(hgu133plus2.db, top$ID, "ENTREZID", "PROBEID", multiVals = "first")
  top <- top[!is.na(top$ENTREZ) & !duplicated(top$ENTREZ), ]
  top$ENTREZ <- paste("ENTREZID", top$ENTREZ, sep = ":")
  tg1 <- top[top$adj.P.Val < 0.05, ]

  DE_Colorectal = tg1$logFC
  names(DE_Colorectal) <- tg1$ENTREZ
  ALL_Colorectal <- top$ENTREZ

  kegg <- pathways("hsapiens", "kegg")
  selected <- kegg[c("Bladder cancer", "Cytosolic DNA-sensing pathway")]

  on.exit(file.remove("spiaTestExSPIA.RData"), add = TRUE)
  prepareSPIA(selected, "spiaTestEx")

  capture.output({x <- runSPIA(de = DE_Colorectal, all = ALL_Colorectal, "spiaTestEx")})

  expect_data_frame(x)
  expect_equal(nrow(x), 2)
  expect_true('Name' %in% names(x))
  expect_set_equal(names(selected), x$Name)
})
