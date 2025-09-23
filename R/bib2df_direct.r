#' Adapted from bib2df function in bib2df
#' This version takes bib as a character vector instead of a filepath.
#' @noRd

bib2df_direct <- function (bib, separate_names = FALSE, merge_lines = FALSE){
  bib <- stringr::str_replace_all(bib, "[^[:graph:]]", " ")
  bib <- stringr::str_replace_all(bib, "=", " = ")
  bib <- stringr::str_replace_all(bib, "  ", " ")
  if (merge_lines == TRUE) {
    bib <- bib2df:::bib2df_merge_lines(bib)
  }
  bib <- bib2df:::bib2df_gather(bib)
  bib <- bib2df:::bib2df_tidy(bib, separate_names)
  return(bib)
}
