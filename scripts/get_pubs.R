library(tidyverse)
library(httr)

# Function to read DOIs from a text file
read_dois_from_file <- function(filepath) {
  if(!file.exists(filepath)) {
    warning(str_glue("DOI file not found: {filepath}"))
    return(character(0))
  }
  
  read_lines(filepath) %>%
    str_trim() %>%
    discard(~ .x == "" | str_starts(.x, "#"))
}

# Function to fetch APA-formatted citation from Crossref
fetch_apa_citation <- function(doi) {
  url <- str_glue("https://doi.org/{doi}")
  
  response <- safely(httr::GET)(
    url, 
    add_headers(Accept = "text/x-bibliography; style=apa-6th-edition; lang=en-US")
  )
  
  if(!is.null(response$error) || status_code(response$result) != 200) {
    warning(str_glue("Failed to retrieve citation for DOI: {doi}"))
    return(NULL)
  }
  
  content(response$result, as = "text", encoding = "UTF-8")
}

# Function to apply formatting to the citation
format_citation <- function(citation, doi, author_name) {
  # Bold the specified author's name
  escaped_author <- str_replace_all(author_name, "([\\W])", "\\\\\\1")
  
  citation %>%
    str_replace_all(
      regex(escaped_author, ignore_case = TRUE),
      function(match) str_glue("**{match}**")
    ) %>%
    # Italicize journal name
    str_replace(
      "(?<=\\.\\s)([^\\.]+?),\\s\\d",
      function(match) {
        journal <- str_match(match, "([^,]+),")[,2]
        rest <- str_replace(match, "([^,]+),", "")
        str_glue("*{journal}*,{rest}")
      }
    ) %>%
    # Replace plain DOI with hyperlink
    str_replace_all(
      "(https?://doi\\.org/\\S+)",
      "<a href='\\1'>\\1</a>"
    )
}

# Main function to generate publication list
generate_publication_list_from_file <- function(doi_file_path, highlight_author = "Winkler, Y.", group_by_year = TRUE) {
  dois <- read_dois_from_file(doi_file_path)
  
  if(length(dois) == 0) {
    return("<p>No DOIs found in the specified file or the file does not exist.</p>")
  }
  
  citations <- dois %>%
    map_dfr(function(doi) {
      citation_text <- fetch_apa_citation(doi)
      
      if(is.null(citation_text)) {
        return(NULL)
      }
      
      formatted_citation <- format_citation(citation_text, doi, highlight_author)
      year_match <- str_match(citation_text, "\\((\\d{4})\\)")
      year <- if(!is.na(year_match[2])) as.integer(year_match[2]) else NA
      
      tibble(year = year, citation = formatted_citation)
    }) %>%
    arrange(desc(year))
  
  if(nrow(citations) == 0) {
    return("<p>No valid citations could be retrieved.</p>")
  }
  
  if(group_by_year) {
    citations %>%
      group_by(year) %>%
      group_map(~ {
        year_val <- .y$year
        citations_text <- .x$citation %>%
          map_chr(~ str_c("- ", .x, "\n")) %>%
          str_c(collapse = "")
        
        str_c("\n\n**", year_val, "**\n\n", citations_text)
      }) %>%
      str_c(collapse = "") %>%
      return()
  } else {
    citations$citation %>%
      map_chr(~ str_c("- ", .x, "\n")) %>%
      str_c(collapse = "") %>%
      return()
  }
}