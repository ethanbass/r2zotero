test_that("skip_duplicates controls skipping of repeated citations in a batch", {
  saved <- 0
  local_mocked_bindings(
    zotero_running = function() TRUE,
    get_zotero_schema = function(...) NULL,
    get_library_keys = function(...) character(),
    bibentry_to_zotero = function(...) list(itemType = "document", title = "Same", date = "2020"),
    save_item = function(...) {
      saved <<- saved + 1
      structure(list(status_code = 201L), class = "response")
    }
  )
  r2zotero(c("jsonlite", "httr"), verbose = FALSE)
  expect_equal(saved, 2)
  saved <- 0
  r2zotero(c("jsonlite", "httr"), skip_duplicates = TRUE, verbose = FALSE)
  expect_equal(saved, 1)
})

test_that("citations without a package name are labelled by title", {
  local_mocked_bindings(
    zotero_running = function() TRUE,
    get_zotero_schema = function(...) NULL,
    bibentry_to_zotero = function(...) list(itemType = "document", title = "X", date = "2020"),
    save_item = function(...) structure(list(status_code = 201L), class = "response")
  )
  b <- bibentry("Manual", title = strrep("A long title ", 5), author = "A", year = 2020)
  expect_message(
    r2zotero(b, skip_duplicates = FALSE),
    "^A long title A long title A long title A\\.\\.\\. successfully imported"
  )
})

test_that("an entry that can't be converted is skipped without stopping the batch", {
  saved <- 0
  local_mocked_bindings(
    zotero_running = function() TRUE,
    get_zotero_schema = function(...) NULL,
    bibentry_to_zotero = function(ct, ...) {
      if (grepl("httr", ct$title)) stop("Unsupported BibTeX entry type: INBOOK")
      list(itemType = "document", title = ct$title, date = "2020")
    },
    save_item = function(...) {
      saved <<- saved + 1
      structure(list(status_code = 201L), class = "response")
    }
  )
  expect_warning(
    res <- r2zotero(c("jsonlite", "httr"), skip_duplicates = FALSE, verbose = FALSE),
    "Skipping httr: Unsupported"
  )
  expect_equal(saved, 1)
  expect_length(res, 1)
})

test_that("an unreadable library warns on every call", {
  local_mocked_bindings(
    zotero_running = function() TRUE,
    get_zotero_schema = function(...) NULL,
    get_library_keys = function(...) stop("HTTP 403"),
    bibentry_to_zotero = function(...) list(itemType = "document", title = "X", date = "2020"),
    save_item = function(...) structure(list(status_code = 201L), class = "response")
  )
  for (i in 1:2) {
    expect_warning(r2zotero("jsonlite", skip_duplicates = TRUE, verbose = FALSE), "HTTP 403")
  }
})
