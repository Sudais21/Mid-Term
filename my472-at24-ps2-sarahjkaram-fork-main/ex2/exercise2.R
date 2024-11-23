# Load required libraries
library(rvest)
library(dplyr)
library(tidyr)

# Define the election years
years <- seq(1952, 2020, by = 4)

# Generate Wikipedia URLs for each election year
urls <- paste0("https://en.wikipedia.org/wiki/", years, "_United_States_presidential_election")

# Function to scrape data from the infobox of each election page
scrape_presidential_candidates <- function(url, year) {
  # Read the page
  page <- read_html(url)
  
  # Find the first infobox table
  table <- page %>% html_node(".infobox") %>% html_table(fill = TRUE)
  
  if (is.null(table)) {
    message(paste("No infobox table found for URL:", url))
    return(NULL)
  }
  
  # Convert to data frame
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  
  # Dynamically locate rows containing relevant information
  candidate_rows <- grepl("Candidate", table[, 1], ignore.case = TRUE)
  party_rows <- grepl("Party", table[, 1], ignore.case = TRUE)
  vote_rows <- grepl("Votes", table[, 1], ignore.case = TRUE)
  ec_vote_rows <- grepl("Electoral", table[, 1], ignore.case = TRUE)
  
  # Extract data
  candidate_name <- if (any(candidate_rows)) table[candidate_rows, 2] else NA
  candidate_party <- if (any(party_rows)) table[party_rows, 2] else NA
  pop_vote <- if (any(vote_rows)) as.numeric(gsub("[^0-9]", "", table[vote_rows, 2])) else NA
  ec_vote <- if (any(ec_vote_rows)) as.numeric(gsub("[^0-9]", "", table[ec_vote_rows, 2])) else NA
  
  # Ensure all columns have the same length
  max_length <- max(
    length(candidate_name),
    length(candidate_party),
    length(pop_vote),
    length(ec_vote),
    na.rm = TRUE
  )
  
  candidate_name <- rep_len(candidate_name, max_length)
  candidate_party <- rep_len(candidate_party, max_length)
  pop_vote <- rep_len(pop_vote, max_length)
  ec_vote <- rep_len(ec_vote, max_length)
  
  # Create a tibble
  candidate_data <- tibble(
    election_year = rep(year, max_length),
    candidate_name = candidate_name,
    candidate_party = candidate_party,
    pop_vote = pop_vote,
    ec_vote = ec_vote,
    winner = c(1, rep(0, max_length - 1)) # Assume first candidate is the winner
  )
  
  return(candidate_data)
}

# Apply the scraping function to all URLs
pres_cands <- lapply(seq_along(urls), function(i) {
  scrape_presidential_candidates(urls[i], years[i])
})

# Combine all results into a tibble
pres_cands_df <- bind_rows(pres_cands) %>%
  arrange(election_year, desc(pop_vote))

# Save the tibble to CSV
write.csv(pres_cands_df, "pres_cands_df.csv", row.names = FALSE)

# Display rows for specific elections
pres_cands_df %>%
  filter(election_year %in% c(1992, 2008, 2016))
