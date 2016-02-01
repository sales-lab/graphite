context("Conversion of identifiers")

test_that("trivial conversion", {
    p <- pathways("hsapiens", "kegg")[[1]]
    expect_equal(p@identifier, "ENTREZID")

    p2 <- convertIdentifiers(p, "ENTREZID")
    expect_identical(edges(p), edges(p2))
})

test_that("convertion to symbol does not increase the number of edges", {
    p <- pathways("hsapiens", "kegg")[[1]]
    expect_equal(p@identifier, "ENTREZID")

    p2 <- convertIdentifiers(p, "symbol")
    expect_true(nrow(edges(p)) >= nrow(edges(p2)))
})

test_that("invalid type of identifiers", {
    p <- pathways("hsapiens", "kegg")[[1]]
    expect_error(convertIdentifiers(p, "INVALID"))
})

test_that("conversion of empty pathway", {
    p <- pathways("hsapiens", "kegg")[[1]]
    p@edges <- data.frame()
    
    es <- edges(convertIdentifiers(p, "ENTREZID"))
    expect_true(is.data.frame(es))
    expect_true(nrow(es) == 0)
})

test_that("conversion loosing all edges", {
    p <- pathways("hsapiens", "kegg")[[1]]
    expect_equal(p@identifier, "ENTREZID")

    es <- edges(p)[1,]
    es$src <- -1
    es$dest <- -2
    p@edges <- es

    expect_warning(p2 <- convertIdentifiers(p, "symbol"))
    es <- edges(p2)
    expect_true(is.data.frame(es))
    expect_true(nrow(es) == 0)
})

test_that("conversion table with extra columns", {
    p <- pathways("hsapiens", "kegg")[[1]]

    es <- edges(p)
    expect_true(nrow(es) > 0)
    p@edges <- es[1:3,]

    p2 <- convertIdentifiers(p, "GO")
    expect_true(is.data.frame(edges(p2)))
})


sameAsListConversion <- function(pathways, n) {
    expect_true(length(pathways) == 3)

    c1 <- convertIdentifiers(pathways, "symbol")
    c2 <- lapply(as.list(pathways), function(p) convertIdentifiers(p, "symbol"))

    for (i in seq_along(c2)) {
        p1 <- c1[[i]]
        p2 <- c2[[i]]
        expect_identical(edges(p1), edges(p2))
    }
}

test_that("conversion of a PathwayList", {
    ps <- pathways("hsapiens", "kegg")[1:3]
    sameAsListConversion(ps, 3)
})

test_that("conversion of a DeprecatedPathwayList", {
    ps <- kegg
    ps@content <- ps@content[1:3]
    suppressWarnings(sameAsListConversion(ps, 3))
})
