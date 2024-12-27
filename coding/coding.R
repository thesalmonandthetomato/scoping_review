# Load necessary library
library(tidyverse)
library(synthesisr)

# Read the RIS file
articles <- read_refs("visualisations and analysis/provisional includes 21.12.24.ris")

# Extract relevant fields
article_data <- articles %>%
  select(accession_number = `accession_zr`,
         title = `title`,
         abstract = `abstract`) %>%
  mutate(across(c(title, abstract), ~replace_na(., "")))

# Read the dictionary
dictionary <- readLines("dictionary.yml")

# Parse dictionary into a named list
parse_dictionary <- function(dict_lines) {
  dict_list <- list()
  for (line in dict_lines) {
    term <- sub("\\[.*", "", line) %>% str_trim()
    synonyms <- sub(".*\\[(.*)\\]", "\\1", line) %>% 
      str_split(",\\s*") %>% unlist() %>% str_trim()
    dict_list[[term]] <- synonyms
  }
  return(dict_list)
}

dictionary_list <- parse_dictionary(dictionary)

# Create a pattern list for matching
patterns <- map(dictionary_list, ~ paste(.x, collapse = "|"))

# Function to find matches for a given term
find_matches <- function(text, pattern) {
  matches <- str_extract_all(text, pattern) %>% unlist()
  matches <- matches[!is.na(matches)]
  if (length(matches) == 0) return(NA_character_)
  paste(unique(matches), collapse = ", ")
}

# Add a column for each dictionary term
for (term in names(dictionary_list)) {
  article_data[[term]] <- apply(article_data[, c("title", "abstract")], 1, function(row) {
    find_matches(paste(row, collapse = " "), patterns[[term]])
  })
}

# Save the resulting dataframe to a CSV file
write_csv(article_data, "output_dataframe.csv")
