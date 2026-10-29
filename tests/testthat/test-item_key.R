test_that("item_key ignores case, punctuation and the date beyond its year", {
  expect_equal(
    item_key("journalArticle", "Fitting Linear Mixed-Effects Models Using lme4", "2015"),
    item_key("journalArticle", "Fitting linear mixed-effects models using lme4.", "2015-10-07")
  )
  expect_false(
    item_key("computerProgram", "jsonlite", "2024") ==
      item_key("computerProgram", "jsonlite", "2023")
  )
  expect_equal(
    item_key("journalArticle", "Models using <i>lme4</i>", "2015"),
    item_key("journalArticle", "Models using lme4", "2015")
  )
  expect_true(is.na(item_key("document", NULL, NULL)))
})
