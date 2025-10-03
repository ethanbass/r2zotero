
.r2zotero_cache <- new.env(parent = emptyenv())

# Function that uses the cache
get_zotero_schema <- function(force = FALSE, verbose = getOption("verbose")) {
  # Check if schema exists in the cache environment
  if (is.null(.r2zotero_cache$schema) || force) {
    if (verbose) message("Downloading schema...")
    # Download and store in the cache environment
    .r2zotero_cache$schema <- download_schema_data(verbose = verbose)
  }

  # Return cached data
  return(.r2zotero_cache$schema)
}

#' Download Zotero schema data
#' @author Ethan Bass
#' @noRd
download_schema_data <- function(verbose = getOption("verbose")) {

  tryCatch({
    response <- httr::GET("https://api.zotero.org/schema",
                          httr::add_headers("Accept-Encoding" = "gzip"))

    if (httr::status_code(response) == 200) {
      .zotero_schema <<- httr::content(response, "parsed")
      names(.zotero_schema$itemTypes) <- get_valid_item_types(.zotero_schema)
      if (verbose){
        message("Schema downloaded successfully (version: ", .zotero_schema$version, ")")
      }
    } else {
      stop("Failed to download schema: HTTP ", httr::status_code(response))
    }
  }, error = function(e) {
    stop("Error downloading Zotero schema: ", e$message)
  })
  return(.zotero_schema)
}
