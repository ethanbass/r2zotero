
#' Get valid item types from Zotero Web API JSON schema
#'
#' @return Character vector of valid Zotero item types
#' @noRd
get_valid_item_types <- function(schema) {
  sapply(schema$itemTypes, function(x) x$itemType)
}

#' Covert bibtex to data.frame with fields in Zotero Web API JSON format
#' @author Ethan Bass
#' @noRd
bib2zot_df <- function(bib, manual_as_document = FALSE){
  df <- bib2df_direct(bib)
  what <- df$CATEGORY
  manual_as <- ifelse(manual_as_document, "manuscript", "computerProgram")
  what <- switch(what, "MANUAL" = manual_as,
                "BOOK" = "book",
                "ARTICLE" = "journalArticle",
                "INCOLLECTION" = "book")
  df_renamed <- df |>
    dplyr::rename(
      dplyr::any_of(
        c(
        title = "TITLE",
        abstractNote = "ANNOTE",
        publicationTitle = switch(what, journalArticle = "JOURNAL",
                                  book = "BOOKTITLE",
                                  # webpage ,
                                  preprint = "JOURNAL"
        ),
        volume = "VOLUME",
        issue = "NUMBER",
        pages = "PAGES",
        date = "YEAR",
        series = "SERIES",
        # language = LANGUAGE,
        DOI = "DOI",
        ISSN = "ISSN",
        # shortTitle = ,
        url = "URL",
        extra = "NOTE",
        seriesNumber = "NUMBER",
        edition = "EDITION",
        place = "ADDRESS",
        publisher = "PUBLISHER",
        ISBN = "ISBN",
        # websiteTitle = ,
        repository = "INSTITUTION",
        # archiveID
        citationKey = "BIBTEXKEY"
        )
      )
    )
  df_renamed$itemType <- what
  df_renamed$creators <- list(create_creators_list(authors = df$AUTHOR[[1]],
                                              editors = df$EDITOR[[1]]))
  schema <- get_zotero_schema()
  fields <- get_valid_fields(schema, item_type = what)
  df_clean <- df_renamed[colnames(df_renamed) %in% c("itemType", fields, "creators")]
  df_clean[which(is.na(df_clean))] <- ""
  df_clean$title <- gsub("[{}]", "", df_clean$title)
  # df_clean$publicationTitle <- gsub("[{}]", "", df_clean$publicationTitle)
  df_clean
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

#' Validate a Zotero item against the schema
#'
#' @param item List representing a Zotero item
#' @param strict Logical, whether to be strict about extra fields
#' @return List with validation results
#' @noRd

validate_zotero_item <- function(item, strict = FALSE) {
  schema <- get_zotero_schema()

  results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )

  # Check if itemType exists
  if (is.null(item$itemType)) {
    results$valid <- FALSE
    results$errors <- c(results$errors, "Missing required field: itemType")
    return(results)
  }

  # Check if itemType is valid
  if (!item$itemType %in% names(schema$itemTypes)) {
    results$valid <- FALSE
    results$errors <- c(results$errors,
                        paste("Invalid itemType:", item$itemType))
    return(results)
  }

  # Get valid fields for this item type
  valid_fields <- get_valid_fields(item$itemType)
  valid_fields <- c(valid_fields, "itemType")  # Always valid

  # Check for invalid fields
  item_fields <- names(item)
  invalid_fields <- setdiff(item_fields, valid_fields)

  if (length(invalid_fields) > 0) {
    if (strict) {
      results$valid <- FALSE
      results$errors <- c(results$errors,
                          paste("Invalid fields:", paste(invalid_fields, collapse = ", ")))
    } else {
      results$warnings <- c(results$warnings,
                            paste("Unexpected fields:", paste(invalid_fields, collapse = ", ")))
    }
  }

  return(results)
}

#' Smart field mapping using schema information
#'
#' @param bibtex_field Character, BibTeX field name
#' @param item_type Character, target Zotero item type
#' @return Character, corresponding Zotero field name or NULL
#' @noRd

smart_field_mapping <- function(bibtex_field, item_type) {
  schema <- get_zotero_schema()
  valid_fields <- get_valid_fields(item_type)

  # Static mapping with schema validation
  mapping <- list(
    "TITLE" = "title",
    "YEAR" = "date",
    "URL" = "url",
    "JOURNAL" = "publicationTitle",
    "BOOKTITLE" = "publicationTitle",
    "PUBLISHER" = "publisher",
    "PAGES" = "pages",
    "VOLUME" = "volume",
    "NUMBER" = "issue"
  )

  zotero_field <- mapping[[bibtex_field]]

  # Validate against schema
  if (!is.null(zotero_field) && zotero_field %in% valid_fields) {
    return(zotero_field)
  }

  return(NULL)
}

#' Create Zotero creators list from AUTHOR and EDITOR fields
#'
#' @param author_field Character string of authors (can be NA)
#' @param editor_field Character string of editors (can be NA)
#' @return List of creator objects in Zotero format
#' @noRd

create_creators_list <- function(authors = NULL, editors = NULL) {
  if (length(authors) == 1 && is.na(authors)){
    authors <- NULL
  } else{
    authors <- lapply(authors, parse_single_name, creator_type = "author")
  }
  if (length(editors) == 1 && is.na(editors)){
    editors <- NULL
  } else{
    editors <- lapply(editors, parse_single_name, creator_type = "editor")
  }
  creators <- c(authors,editors)
  return(creators)
}

#' Parse single name
#' @noRd
#' @author Ethan Bass
parse_single_name <- function(name_string, creator_type = "author") {
  name_string <- latex_to_unicode(name_string)
  name_string <- gsub("[{}]", "", name_string)

  name_string <- trimws(name_string)

  if (name_string == "R Core Team"){
    return(list(
      creatorType = creator_type,
      name = name_string
    ))
  }
  # Try to detect if name is in "Last, First" format
  if (grepl(",", name_string)) {
    parts <- unlist(strsplit(name_string, ",", fixed = TRUE))
    if (length(parts) >= 2) {
      lastName <- trimws(parts[1])
      firstName <- trimws(paste(parts[-1], collapse = " "))

      return(list(
        creatorType = creator_type,
        lastName = lastName,
        firstName = firstName
      ))
    }
  }

  # Try to parse "First Last" or "First Middle Last" format
  words <- unlist(strsplit(name_string, "\\s+"))
  if (length(words) >= 2) {
    # Assume last word is last name, everything else is first name
    lastName <- words[length(words)]
    firstName <- paste(words[-length(words)], collapse = " ")

    return(list(
      creatorType = creator_type,
      lastName = lastName,
      firstName = firstName
    ))
  }

  # If parsing fails, use the whole string as name
  return(list(
    creatorType = creator_type,
    name = name_string
  ))
}

latex_to_unicode <- function(x) {
  accents <- list(
    "\\\\'"  = "\u0301",  # acute
    "\\\\`"  = "\u0300",  # grave
    "\\\\\"" = "\u0308",  # diaeresis/umlaut
    "\\\\^"  = "\u0302",  # circumflex
    "\\\\~"  = "\u0303",  # tilde
    "\\\\c"  = "\u0327",  # cedilla
    "\\\\r"  = "\u030A",  # ring above
    "\\\\v"  = "\u030C",  # caron/háček
    "\\\\."  = "\u0307"   # dot above
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


