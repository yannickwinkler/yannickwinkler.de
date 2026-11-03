library(tidyverse)
library(httr)

# Section headings, in display order
SECTION_ORDER <- c(
  "Peer-Reviewed Journal Articles",
  "Book Chapters",
  "Preprints",
  "Other Publications"
)

# Map Crossref work types to section headings
crossref_type_to_section <- function(type) {
  switch(type %||% "",
    "journal-article" = "Peer-Reviewed Journal Articles",
    "book-chapter"    = "Book Chapters",
    "posted-content"  = "Preprints",
    "Other Publications"
  )
}

# Read DOIs from a text file.
# Each line: a DOI, optionally followed by "| Section name" to override the
# automatic section, e.g. "https://doi.org/10.1234/abc | Book Chapters".
read_dois_from_file <- function(filepath) {
  if (!file.exists(filepath)) {
    warning(str_glue("DOI file not found: {filepath}"))
    return(tibble(doi = character(0), section_override = character(0)))
  }

  lines <- read_lines(filepath) %>%
    str_trim() %>%
    discard(~ .x == "" | str_starts(.x, "#"))

  parts <- str_split_fixed(lines, "\\|", 2)
  tibble(
    doi = parts[, 1] %>% str_trim() %>% str_remove("^https?://(dx\\.)?doi\\.org/"),
    section_override = na_if(str_trim(parts[, 2]), "")
  )
}

# GET with retries: the citation services occasionally return errors
# (e.g. a spurious 404), which used to drop publications from the list.
get_with_retry <- function(url, ...) {
  response <- safely(httr::RETRY)(
    "GET", url, ...,
    times = 5, pause_base = 1, pause_cap = 8, quiet = TRUE,
    terminate_on = c(400, 401, 403)
  )
  if (is.null(response$error) && status_code(response$result) == 200) response$result else NULL
}

# Fetch metadata (type, titles, volume, authors, year) from the Crossref API
fetch_crossref_metadata <- function(doi) {
  response <- get_with_retry(str_glue("https://api.crossref.org/works/{doi}"))

  if (is.null(response)) {
    warning(str_glue("Failed to retrieve Crossref metadata for DOI: {doi}"))
    return(list())
  }

  msg <- jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"))$message
  authors <- msg$author
  list(
    type      = msg$type,
    title     = msg$title[1],
    container = msg$`container-title`[1],
    volume    = msg$volume,
    year      = msg$issued$`date-parts`[[1]][1],
    authors   = if (is.data.frame(authors)) {
      str_c(authors$family, ", ", str_replace_all(authors$given, "(\\p{L})\\p{L}*", "\\1."))
    } else character(0)
  )
}

# Minimal APA-like citation from Crossref metadata (last-resort fallback)
citation_from_metadata <- function(doi, meta) {
  if (is.null(meta$title)) return(NULL)
  authors <- meta$authors
  author_str <- case_when(
    length(authors) == 0 ~ "",
    length(authors) == 1 ~ authors[1],
    TRUE ~ str_c(str_c(head(authors, -1), collapse = ", "), ", & ", tail(authors, 1))
  )
  parts <- c(
    str_glue("{author_str} ({meta$year %||% 'n.d.'})."),
    str_c(meta$title, "."),
    if (!is.null(meta$container) && !is.na(meta$container)) str_c(meta$container, ".") else NULL,
    str_glue("https://doi.org/{doi}")
  )
  str_c(parts, collapse = " ")
}

# Fetch an APA 7 citation (plain text). Tries the DOI citation formatter,
# then doi.org content negotiation (both with retries).
fetch_apa_citation <- function(doi) {
  response <- get_with_retry(
    "https://citation.doi.org/format",
    query = list(doi = doi, style = "apa", lang = "en-US")
  )
  if (is.null(response)) {
    response <- get_with_retry(
      str_glue("https://doi.org/{doi}"),
      add_headers(Accept = "text/x-bibliography; style=apa; locale=en-US")
    )
  }
  if (is.null(response)) {
    warning(str_glue("Failed to retrieve citation for DOI: {doi}"))
    return(NULL)
  }

  content(response, as = "text", encoding = "UTF-8") %>% str_trim()
}

# Italicize an exact substring once (no-op if it isn't found)
italicize <- function(text, target) {
  if (is.null(target) || is.na(target) || !str_detect(text, fixed(target))) {
    return(text)
  }
  str_replace(text, fixed(target), str_c("*", target, "*"))
}

# Apply APA italics, bold the highlighted author and link the DOI
format_citation <- function(citation, meta, author_name) {
  citation <- switch(meta$type %||% "",
    "journal-article" = {
      target <- meta$container
      if (!is.null(meta$volume) && str_detect(citation, fixed(str_c(target, ", ", meta$volume)))) {
        target <- str_c(target, ", ", meta$volume)
      }
      italicize(citation, target)
    },
    "book-chapter"   = italicize(citation, meta$container),
    "posted-content" = italicize(citation, meta$title),
    citation
  )

  citation %>%
    str_replace_all(fixed(author_name), str_c("**", author_name, "**")) %>%
    str_replace_all("(https?://doi\\.org/\\S+)", "<a href='\\1'>\\1</a>")
}

citations_to_md <- function(citations) {
  str_c("- ", citations, "\n", collapse = "")
}

# Main function: publication list grouped into sections by type
generate_publication_list_from_file <- function(doi_file_path, highlight_author = "Winkler, Y.", group_by_year = FALSE) {
  dois <- read_dois_from_file(doi_file_path)

  if (nrow(dois) == 0) {
    return("<p>No DOIs found in the specified file or the file does not exist.</p>")
  }

  pubs <- pmap_dfr(dois, function(doi, section_override) {
    meta <- fetch_crossref_metadata(doi)
    citation_text <- fetch_apa_citation(doi) %||% citation_from_metadata(doi, meta)

    # Never drop a publication silently: fall back to a plain DOI link
    if (is.null(citation_text)) {
      warning(str_glue("Using bare DOI link for: {doi}"))
      citation_text <- str_glue("https://doi.org/{doi}")
    }
    year <- as.integer(str_match(citation_text, "\\((\\d{4})")[, 2])

    tibble(
      section  = coalesce(section_override, crossref_type_to_section(meta$type)),
      year     = year,
      citation = format_citation(citation_text, meta, highlight_author)
    )
  })

  if (nrow(pubs) == 0) {
    return("<p>No valid citations could be retrieved.</p>")
  }

  sections <- c(SECTION_ORDER, setdiff(unique(pubs$section), SECTION_ORDER))

  sections %>%
    keep(~ .x %in% pubs$section) %>%
    map_chr(function(sec) {
      sec_pubs <- pubs %>% filter(section == sec) %>% arrange(desc(year))

      body <- if (group_by_year) {
        sec_pubs %>%
          group_by(year) %>%
          group_map(~ str_c("\n**", .y$year, "**\n\n", citations_to_md(.x$citation))) %>%
          str_c(collapse = "")
      } else {
        citations_to_md(sec_pubs$citation)
      }

      str_c("\n\n### ", sec, "\n\n", body)
    }) %>%
    str_c(collapse = "")
}
