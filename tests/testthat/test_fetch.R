library(httr)
context("Fetching of pathway data")

test_that("Mus musculus pathways are available", {
  url <- remoteUrl("mmusculus", "kegg")
  r <- HEAD(url)
  expect_equal(http_status(r)$category, "Success")
})
