# Copyright 2017-2019 Gabriele Sales <gabriele.sales@unipd.it>
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

context("Fetching of pathway data")

test_that("Mus musculus pathways are available", {
  url <- remoteUrl("mmusculus-reactome")
  r <- HEAD(url)
  expect_equal(http_status(r)$category, "Success")
})
