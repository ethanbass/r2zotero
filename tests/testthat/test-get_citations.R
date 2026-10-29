test_that("base packages share one citation and missing packages are skipped", {
  expect_warning(
    cits <- get_citations(c("stats", "utils", "base", "jsonlite", "notapkg123")),
    "notapkg123"
  )
  expect_named(cits, c("base", "jsonlite"))
})
