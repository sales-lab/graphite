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

suppressPackageStartupMessages(library(httr))

context("Pathway URLs")

test_that("each pathway database links to valid pathway URLs", {
  skip_on_bioc()

  dbs <- pathwayDatabases()
  species <- "hsapiens"
  dbs <- as.character(dbs[dbs$species == species, "database"])

  for (db in dbs) {
    p <- sample(pathways(species, db), 1)[[1]]
    r <- HEAD(pathwayURL(p), timeout(30))
    expect_equal(http_status(r)$category, "Success",
                 info = paste0(db, "(", pathwayId(p), ")"))
  }
})
