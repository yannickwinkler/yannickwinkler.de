library(httr)       # Though not directly used by user's function, good for general API work
library(jsonlite)   # Used by the OpenAlex function
library(dplyr)
library(purrr)
library(lubridate)  # For potential date parsing, though OpenAlex provides year
library(knitr)
library(stringr)    # For DOI string manipulation

# --- Function to read DOIs from a text file ---
read_dois_from_file <- function(filepath) {
  if (!file.exists(filepath)) {
    warning(paste("DOI file not found:", filepath))
    return(character(0)) # Return empty character vector
  }
  dois <- readLines(filepath, warn = FALSE)
  dois <- trimws(dois) # Remove leading/trailing whitespace
  dois <- dois[dois != ""] # Remove empty lines
  dois <- dois[!str_starts(dois, "#")] # Remove comment lines
  return(dois)
}

# --- Function to fetch data from OpenAlex API ---
fetch_openalex_data <- function(doi) {
  # Normalize DOI: remove trailing slash and ensure it's a full URL for OpenAlex
  clean_doi <- stringr::str_remove(doi, "/$")
  if (!stringr::str_detect(clean_doi, "^(http|https)://doi.org/")) {
    if (stringr::str_detect(clean_doi, "^doi.org/")) {
        clean_doi <- paste0("https://", clean_doi)
    } else {
        clean_doi <- paste0("https://doi.org/", clean_doi)
    }
  }

  print(clean_doi)

  # OpenAlex API expects the DOI *path* (e.g., 10.xxxx/xxxx) or the full DOI URL
  api_url <- paste0("https://api.openalex.org/works/", URLencode(clean_doi, reserved = TRUE))

  Sys.sleep(0.2) # Be polite to the API (OpenAlex is generally fast & allows high rates)
  tryCatch({
    response_json <- jsonlite::fromJSON(api_url)
    if (!is.null(response_json$error)) {
        warning(paste("OpenAlex API error for DOI:", doi, "-", response_json$message))
        return(NULL)
    }
    return(response_json)
  }, error = function(e) {
    warning(paste("Failed to fetch or parse from OpenAlex for DOI:", doi, "-", e$message))
    return(NULL)
  })
}

# --- Function to format publication data from OpenAlex response ---
format_publication_openalex <- function(item) {
  if (is.null(item)) return(list(year = NA, md = ""))

  title <- item$display_name
  year <- item$publication_year
  year_str <- ifelse(!is.na(year), as.character(year), "N/A")

  authors <- "N/A"
  if (!is.null(item$authorships) && length(item$authorships) > 0) {
    # Check if authorships is a list of data frames or a single data frame
    if (is.data.frame(item$authorships)) { # Can happen if only one author and fromJSON simplifies
        authors <- paste(item$authorships$author$display_name, collapse = ", ")
    } else if (is.list(item$authorships)) { # More common: list of authors
        authors <- paste(sapply(item$authorships, function(a) a$author$display_name), collapse = ", ")
    }
  }

  journal_name <- "N/A"
  if (!is.null(item$primary_location) && !is.null(item$primary_location$source) && !is.null(item$primary_location$source$display_name)) {
    journal_name <- item$primary_location$source$display_name
  } else if (!is.null(item$host_venue) && !is.null(item$host_venue$display_name)) {
    journal_name <- item$host_venue$display_name # Fallback
  }
  if (is.list(journal_name)) journal_name <- journal_name[[1]] # Ensure it's not a list

  volume <- item$biblio$volume
  issue <- item$biblio$issue
  pages <- NA
  if (!is.null(item$biblio$first_page)) {
    pages <- item$biblio$first_page
    if (!is.null(item$biblio$last_page) && item$biblio$last_page != item$biblio$first_page) {
      pages <- paste0(pages, "–", item$biblio$last_page)
    }
  }

  doi_url <- item$doi # OpenAlex provides the full DOI URL

  # Construct publication details string
  details_parts <- c()
  if (!is.na(journal_name) && nzchar(journal_name) && journal_name != "N/A") details_parts <- c(details_parts, paste0("*", journal_name, "*"))
  if (!is.null(volume) && !is.na(volume) && nzchar(volume)) details_parts <- c(details_parts, paste("Vol.", volume))
  if (!is.null(issue) && !is.na(issue) && nzchar(issue)) details_parts <- c(details_parts, paste("Issue", issue))
  if (!is.na(pages) && nzchar(pages)) details_parts <- c(details_parts, paste("pp.", pages))

  details_str <- paste(details_parts, collapse = ", ")
  if (nzchar(details_str)) details_str <- paste(details_str, ".", sep = "")
  else details_str <- "Publication details not available."


  md_output <- sprintf(
    "<div class='publication-item'>\n<span class='publication-title'>%s</span> (%s)\n<br>\n<span class='publication-authors'>%s</span>\n<br>\n<span class='publication-details'>%s</span>\n<br>\n<span class='publication-doi'><a href='%s' target='_blank'>%s</a></span>\n</div>\n",
    ifelse(is.na(title), "Title N/A", title),
    year_str,
    authors,
    details_str,
    ifelse(is.na(doi_url), "#", doi_url),
    ifelse(is.na(doi_url), "DOI N/A", doi_url)
  )

  return(list(year = year, md = md_output))
}

# --- Main Function Called from Quarto Document ---
generate_publication_list_from_file <- function(doi_file_path, group_by_year = TRUE) {
  dois <- read_dois_from_file(doi_file_path)
  if (length(dois) == 0) {
    return("<p>No DOIs found in the specified file or the file does not exist.</p>")
  }

  # Fetch and format data for each DOI
  # Using purrr::map to iterate and collect results
  results <- purrr::map(dois, function(d) {
    raw_data <- fetch_openalex_data(d)
    if (is.null(raw_data)) {
      # Attempt to add a placeholder for failed DOIs if you want to see them in the list
      # return(list(year = NA, md = paste0("<div class='publication-item'><span class='publication-title'>Could not retrieve data for DOI: ", d,"</span></div>")))
      return(NULL) # Or simply skip failed ones
    }
    format_publication_openalex(raw_data)
  })

  # Remove NULL entries (failed fetches) and entries with empty md
  results <- results[!sapply(results, is.null)]
  results <- results[sapply(results, function(x) !is.null(x$md) && nzchar(x$md))]


  if (length(results) == 0) {
    return("<p>No publication data could be retrieved or formatted from the provided DOIs.</p>")
  }

  # Sort by year (descending) - handle potential NA years by putting them last
  years_for_sorting <- sapply(results, function(x) ifelse(is.na(x$year), -Inf, x$year))
  results <- results[order(years_for_sorting, decreasing = TRUE)]


  # Generate final Markdown string
  final_md <- ""
  if (group_by_year) {
    # Extract unique, non-NA years and sort them
    valid_years <- unique(sapply(results, `[[`, "year"))
    valid_years <- valid_years[!is.na(valid_years)]
    valid_years <- sort(valid_years, decreasing = TRUE)

    for (yr in valid_years) {
      final_md <- paste0(final_md, "\n### ", yr, "\n\n<div class='publication-list'>\n")
      # Get items for this year
      year_items <- Filter(function(x) !is.na(x$year) && x$year == yr, results)
      final_md <- paste0(final_md, paste(sapply(year_items, `[[`, "md"), collapse = "\n"))
      final_md <- paste0(final_md, "\n</div>\n")
    }
    # Handle items with NA year if any (e.g. preprints without a publication year yet or failed year parsing)
    na_year_items <- Filter(function(x) is.na(x$year), results)
    if (length(na_year_items) > 0) {
      final_md <- paste0(final_md, "\n### Year Not Available\n\n<div class='publication-list'>\n")
      final_md <- paste0(final_md, paste(sapply(na_year_items, `[[`, "md"), collapse = "\n"))
      final_md <- paste0(final_md, "\n</div>\n")
    }

  } else {
    # Ungrouped list
    final_md <- "<div class='publication-list'>\n"
    final_md <- paste0(final_md, paste(sapply(results, `[[`, "md"), collapse = "\n"))
    final_md <- paste0(final_md, "\n</div>\n")
  }

  return(final_md)
}