
#' Get valid item types from Zotero Web API JSON schema
#'
#' @return Character vector of valid Zotero item types
#' @noRd
get_valid_item_types <- function(schema) {
  sapply(schema$itemTypes, function(x) x$itemType)
}

#' Convert a bibentry to a list in Zotero Web API JSON format
#' @author Ethan Bass
#' @noRd
bibentry_to_zotero <- function(cit, manual_as_document = FALSE){
  entry <- unclass(cit)[[1]]
  bibtype <- toupper(attr(entry, "bibtype"))
  manual_as <- ifelse(manual_as_document, "document", "computerProgram")
  what <- switch(bibtype, "MANUAL" = manual_as,
                "BOOK" = "book",
                "ARTICLE" = "journalArticle",
                "INCOLLECTION" = "bookSection",
                "INPROCEEDINGS" = ,
                "CONFERENCE" = "conferencePaper",
                "TECHREPORT" = "report",
                "PHDTHESIS" = ,
                "MASTERSTHESIS" = "thesis",
                "UNPUBLISHED" = "manuscript",
                "MISC" = "document",
                stop("Unsupported BibTeX entry type: ", bibtype, call. = FALSE))
  map <- c(
    title = "title",
    abstractNote = "annote",
    publicationTitle = switch(what, journalArticle = "journal",
                              book = "booktitle",
                              bookSection = "booktitle",
                              conferencePaper = "booktitle",
                              # webpage ,
                              preprint = "journal"
    ),
    volume = "volume",
    issue = "number",
    pages = "pages",
    date = "year",
    series = "series",
    # language = LANGUAGE,
    DOI = "doi",
    ISSN = "issn",
    PMID = "pmid",
    # shortTitle = ,
    url = "url",
    extra = "note",
    seriesNumber = "number",
    edition = "edition",
    place = "address",
    publisher = switch(what, report = "institution", thesis = "school",
                       "publisher"),
    ISBN = "isbn",
    # websiteTitle = ,
    repository = "institution"
    # archiveID
  )
  map <- map[map %in% names(entry)]
  item <- lapply(entry[map], function(x) gsub("\\s+", " ", trimws(paste(x, collapse = " "))))
  names(item) <- names(map)
  item$citationKey <- attr(entry, "key")
  schema <- get_zotero_schema()
  base_fields <- get_base_fields(schema, what)
  base_fields <- base_fields[base_fields %in% names(item)]
  names(item)[match(base_fields, names(item))] <- names(base_fields)
  item$itemType <- what
  item$creators <- create_creators_list(authors = entry$author,
                                        editors = entry$editor,
                                        creator_types = get_valid_creator_types(schema, what))
  fields <- get_valid_fields(schema, item_type = what)
  item <- item[names(item) %in% c("itemType", fields, "creators")]
  if (!is.null(item$title)) item$title <- gsub("[{}]", "", latex_to_unicode(item$title))
  item
}

#' Get valid fields for a specific item type
#'
#' @param schema Zotero Web API JSON schema
#' @param item_type Character, Zotero item type (e.g., "journalArticle")
#' @return Character vector of valid field names for this item type
#' @noRd

get_valid_fields <- function(schema, item_type){
  types <- get_valid_item_types(schema)
  idx <- which(types == item_type)
  fields <- sapply(schema$itemTypes[[idx]]$fields, function(x) x$field)
  fields
}

#' Get type-specific fields that map to a base field
#'
#' @param schema Zotero Web API JSON schema
#' @param item_type Character, Zotero item type (e.g., "bookSection")
#' @return Named character vector of base fields, named by the type-specific
#' field (e.g., `c(bookTitle = "publicationTitle")`)
#' @noRd

get_base_fields <- function(schema, item_type){
  fields <- Filter(function(x) !is.null(x$baseField),
                   schema$itemTypes[[item_type]]$fields)
  base_fields <- vapply(fields, function(x) x$baseField, character(1))
  names(base_fields) <- vapply(fields, function(x) x$field, character(1))
  base_fields
}

#' Get valid creator types for a specific item type
#'
#' @param item_type Character, Zotero item type
#' @return Character vector of valid creator types
#' @noRd

get_valid_creator_types <- function(schema, item_type) {
  # schema <- get_zotero_schema()
  types <- get_valid_item_types(schema)
  if (!item_type %in% types) {
    stop("Invalid item type: ", item_type)
  }

  # Creator types are stored in the creatorTypes field
  creator_info <- schema$itemTypes[[item_type]]$creatorTypes
  if (is.null(creator_info)) {
    return(character(0))
  } else{
    return(sapply(creator_info, function(x)x$creatorType))
  }

  return(creator_info)
}

# creator_types <- get_valid_creator_types(schema, "book")
# creator_types

#' Create Zotero creators list from author and editor persons
#'
#' @param authors [person] vector of authors (can be NULL)
#' @param editors [person] vector of editors (can be NULL)
#' @param creator_types Character vector of creator types valid for the item
#' type, primary type first
#' @return List of creator objects in Zotero format
#' @noRd

create_creators_list <- function(authors = NULL, editors = NULL,
                                 creator_types = c("author", "editor")) {
  author_type <- if ("author" %in% creator_types) "author" else creator_types[1]
  editor_type <- if ("editor" %in% creator_types) "editor" else "contributor"
  c(lapply(authors, person_to_creator, creator_type = author_type),
    lapply(editors, person_to_creator, creator_type = editor_type))
}

#' Convert a single person to a Zotero creator
#' @noRd
#' @author Ethan Bass
person_to_creator <- function(person, creator_type = "author") {
  clean <- function(x) trimws(gsub("[{}]", "", latex_to_unicode(paste(x, collapse = " "))))
  given <- clean(person$given)
  family <- clean(person$family)
  if (!nzchar(given) || !nzchar(family) ||
      paste(given, family) == "R Core Team") {
    return(list(
      creatorType = creator_type,
      name = trimws(paste(given, family))
    ))
  }
  list(
    creatorType = creator_type,
    lastName = family,
    firstName = given
  )
}

latex_to_unicode <- function(x) {
  accents <- list(
    "\\\\'"  = "\u0301",  # acute
    "\\\\`"  = "\u0300",  # grave
    "\\\\\"" = "\u0308",  # diaeresis/umlaut
    "\\\\\\^"  = "\u0302",  # circumflex
    "\\\\~"  = "\u0303",  # tilde
    "\\\\c"  = "\u0327",  # cedilla
    "\\\\r"  = "\u030A",  # ring above
    "\\\\v"  = "\u030C",  # caron/háček
    "\\\\\\."  = "\u0307"   # dot above
  )

  for (cmd in names(accents)) {
    pattern <- paste0("(", cmd, ")\\{?([A-Za-z])\\}?")
    replacement <- paste0("\\2", accents[[cmd]])
    x <- gsub(pattern, replacement, x, perl = TRUE)
  }

  # normalize to precomposed characters where possible
  x <- stringi::stri_trans_nfc(x)
  x
}


