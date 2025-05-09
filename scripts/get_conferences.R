library(tidyverse)

generate_conference_list <- function(json_path, highlight_author = "Winkler, Y.", entry_type = "conference") {
  # Daten einlesen und filtern
  conferences <- jsonlite::read_json(json_path, simplifyVector = FALSE) %>%
    keep(~ .x$type == entry_type)
  
  if (length(conferences) == 0) {
    return(str_c("No entries of type '", entry_type, "' found."))
  }
  
  # Nach Jahr sortieren (absteigend)
  conferences <- conferences %>%
    enframe(name = NULL) %>%
    mutate(year = map_int(value, "year", .default = NA_integer_)) %>%
    arrange(desc(year), na.last = TRUE) %>%
    pull(value)
  
  # Markdown generieren
  output_markdown <- conferences %>%
    map_chr(function(conf) {
      # Auto-bold highlighted author
      escaped_author <- str_replace_all(highlight_author, "([\\W])", "\\\\\\1")
      formatted_authors <- conf$authors %>%
        str_replace_all(
          regex(escaped_author, ignore_case = TRUE),
          ~ str_c("**", .x, "**")
        )

      # Titel-Formatierung
      title_suffix <- if(str_detect(conf$title, "[.!?]$")) " " else ". "
      
      # Eintrag zusammenbauen
      entry_details <- str_c(
        formatted_authors, " (", conf$year, "). ",
        "<i>", conf$title, "</i>", title_suffix,
        conf$conference, ", ",
        conf$location
      )
      
      # Slides hinzufügen, wenn verfügbar
      if(!is.null(conf$slides_url) && str_length(conf$slides_url) > 0) {
        entry_details <- str_c(
          entry_details, " [<i class='fas fa-project-diagram'></i> Slides](", 
          conf$slides_url, ")"
        )
      }
      
      # Markdown-String erstellen
      str_c("\n\n**", conf$year, "**\n\n- ", entry_details, "\n")
    })
  
  # Alle Einträge zusammenfügen
  str_c(output_markdown, collapse = "")
}