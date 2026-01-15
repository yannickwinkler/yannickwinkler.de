library(tidyverse)

generate_conference_list <- function(json_path, highlight_author = "Winkler, Y.", entry_type = "conference") {
  conferences <- jsonlite::read_json(json_path, simplifyVector = FALSE) %>%
    keep(~ .x$type == entry_type)
  
  if (length(conferences) == 0) {
    return(str_c("No entries of type '", entry_type, "' found."))
  }
  
  # Convert to tibble and format entries
  conf_tbl <- tibble(
    year = map_int(conferences, "year", .default = NA_integer_),
    conf = conferences
  ) %>%
    arrange(desc(year), na.last = TRUE) %>%
    mutate(
      formatted_entry = map_chr(conf, function(conf) {
        # Auto-bold highlighted author
        escaped_author <- str_replace_all(highlight_author, "([\\W])", "\\\\\\1")
        formatted_authors <- conf$authors %>%
          str_replace_all(
            regex(escaped_author, ignore_case = TRUE),
            ~ str_c("**", .x, "**")
          )
        
        title_suffix <- if(str_detect(conf$title, "[.!?]$")) " " else ". "
        entry_details <- str_c(
          formatted_authors, " (", conf$year, "). ",
          "<i>", conf$title, "</i>", title_suffix,
          conf$conference, ", ",
          conf$location
        )
        
        if(!is.null(conf$slides_url) && str_length(conf$slides_url) > 0) {
          entry_details <- str_c(
            entry_details, " [<i class='fas fa-project-diagram'></i> Slides](",
            conf$slides_url, ")"
          )
        }
        
        str_c("- ", entry_details)
      })
    )
  
  # Group by year and create markdown sections
  output_markdown <- conf_tbl %>%
    arrange(desc(year)) %>%
    group_by(year) %>%
    summarise(
      entries = str_c(formatted_entry, collapse = "\n"),
      .groups = "drop"
    ) %>%
    # ADD THIS LINE to re-sort the summarized data by year in descending order
    arrange(desc(year)) %>% 
    mutate(
      year_section = str_c("\n\n**", year, "**\n\n", entries, "\n")
    ) %>%
    pull(year_section) %>%
    str_c(collapse = "")
  
  return(output_markdown)
}