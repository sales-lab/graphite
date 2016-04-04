context("Pathway construction")

single_edge <- data.frame(src="672", dest="7157", direction="undirected", type="binding")

test_that("single edge pathway", {
  p <- buildPathway("#1", "example", single_edge, "database", "hsapiens", "ENTREZID")

  expect_equal(p@identifier, "ENTREZID")
  expect_equal(NROW(edges(p)), 1)
})

test_that("nodes are converted into characters", {
  e <- single_edge
  e$src <- as.numeric(e$src)
  e$direction <- as.factor(e$direction)

  expect_true(is.numeric(e$src))
  expect_true(is.factor(e$direction))

  p <- buildPathway("#1", "example", e, "database", "hsapiens", "ENTREZID")

  pe <- edges(p)
  expect_true(is.character(pe$src))
  expect_true(is.character(pe$dest))
  expect_true(is.character(pe$direction))
  expect_true(is.character(pe$type))
})
