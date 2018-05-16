# Copyright 2018 Gabriele Sales <gabriele.sales@unipd.it>
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

context("Plotting utilities")

test_that("markMultipleEdges works when there are no multiple edges", {
  g <- new("graphNEL",
           nodes = c("A", "B"),
           edgeL = list("A" = "B", "B" = character()),
           edgemode = 'directed')
  edgeDataDefaults(g, "edgeType") <- "undefined"
  
  attrs <- markMultipleEdges(g)
  
  expect_s3_class(attrs, "data.frame")
  expect_named(attrs, c("id", "edgeType"))
  expect_equal(nrow(attrs), 0L)
})
