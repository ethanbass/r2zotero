# r2zotero 0.2.0

## Breaking changes

* `r2zotero(manual_as_document = TRUE)` now imports `Manual` entries as a Zotero "Document", as documented.

## New features

* `r2zotero()` now imports `Misc`, `InProceedings`, `TechReport`, `PhdThesis`, `MastersThesis` and `Unpublished` citations, which previously failed with an obscure error. Other entry types give a clear error.

## Minor improvements

* r2zotero no longer depends on bib2df, dplyr or stringr.

## Bug fixes

* `InCollection` citations are now imported as book sections with their book title; previously the chapter was saved as a book under the chapter's title.

* URLs containing `=`, such as query strings, are no longer corrupted with inserted spaces (`?a = b`).

* Organisation names containing a comma or "and", such as "Posit Software, PBC", and braced family names such as `{van der Berg}` are no longer split incorrectly.

* Titles that wrap across lines in a CITATION file no longer keep the extra spaces from the line break.

* LaTeX circumflex and dot accents in author names are now converted correctly; previously `\^{o}` became "ȯ".

* Creators now use roles Zotero accepts for the item type: software authors become "Programmer", and editors become "Contributor" where no editor role exists.

* The `publisher` of software citations now fills Zotero's "Company" field; previously it was dropped.

* Failed imports now warn with the HTTP status. Previously an HTTP 500 raised an empty warning and other failures were silent.

* `r2zotero()` no longer creates a `.zotero_schema` object in the global environment.
