# Load necessary libraries
library(RSelenium)
library(tidyverse)

# Start the RSelenium driver
driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.85", verbose = F)
remote_driver <- driver[["client"]]

# Open the webpage
remote_driver$navigate("https://www.congress.gov/senate-hearing-transcripts/")

# Wait for the page to load
Sys.sleep(2)

# Find the dropdown menu for Congress
dropdown <- remote_driver$findElement(using = 'xpath', value = '//*[@id="congress"]')

# Wait for the dropdown to be loaded
Sys.sleep(2)

# Now, try to find the option for '117th Congress' using xpath
congress_option <- remote_driver$findElement(using = 'xpath', value = "//option[text()='117th Congress']")

# Click the dropdown option for the 117th Congress
congress_option$clickElement()

# Wait for the page to reload or update after selecting the 117th Congress
Sys.sleep(2)


# Load necessary libraries
library(RSelenium)
library(tidyverse)
library(rvest)

# Start the RSelenium driver
driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.85", verbose = F)
remote_driver <- driver[["client"]]

# Open the webpage
remote_driver$navigate("https://www.congress.gov/senate-hearing-transcripts/")

# Wait for the page to load
Sys.sleep(2)

# Extract the page source
page_source <- remote_driver$getPageSource()[[1]]

# Convert the page source to an HTML document
html <- read_html(page_source)

# Extract the table containing the Senate hearings
hearing_table <- html %>%
  html_node("table") %>% 
  html_table(fill = TRUE)

# View the first few rows of the extracted table
head(hearing_table)

# Check the column names to ensure they are correct
colnames(hearing_table)

# Now let's filter the hearings by 'Judiciary' committee and 'Confirmation' in the title
# Update the column names as per the actual table structure, if necessary
# Filter the hearings by 'Judiciary' committee and 'Confirmation' in the title
filtered_hearings <- hearing_table %>%
  filter(grepl("Judiciary", Committee, ignore.case = TRUE) & 
           grepl("Confirmation", `Hearing Title`, ignore.case = TRUE))

# View the filtered results
head(filtered_hearings)


# Create the Link column based on Hearing Number
filtered_hearings$Link <- paste0("https://www.congress.gov/committee-hearing/", filtered_hearings$`Hearing Number`)

# Create a folder to store the transcripts
dir.create("D:/Mid Term Solved/my472-at24-ps2-sarahjkaram-fork-main/ex4/transcripts", showWarnings = FALSE)

# Iterate over the filtered hearings and download the transcripts
for (i in 1:nrow(filtered_hearings)) {
  # Find the link to the hearing transcript
  hearing_link <- filtered_hearings[i, "Link"]  # Now the Link column exists
  
  # Navigate to the hearing link
  remote_driver$navigate(hearing_link)
  
  # Wait for the page to load
  Sys.sleep(2)
  
  # Extract the text of the hearing
  hearing_text <- remote_driver$getPageSource()[[1]]
  
  # Parse and extract the transcript
  transcript <- read_html(hearing_text) %>%
    html_node("div.transcript") %>%
    html_text()
  
  # Save the transcript as a text file
  file_name <- paste0("D:/Mid Term Solved/my472-at24-ps2-sarahjkaram-fork-main/ex4/transcripts/", filtered_hearings[i, "Hearing Number"], ".txt")
  writeLines(transcript, file_name)
  
  # Wait between downloads to mimic human behavior
  Sys.sleep(2)
}



# Load the required libraries
library(quanteda)
library(dplyr)
library(stringr)

# List all transcript files in the directory
file_paths <- list.files("D:/Mid Term Solved/my472-at24-ps2-sarahjkaram-fork-main/ex4/transcripts", full.names = TRUE)

# Read all files into a list
transcripts <- lapply(file_paths, function(file) {
  text <- readLines(file)
  
  # Remove non-verbatim sections (e.g., Table of Contents, Introductory Text)
  # Assuming sections like "TABLE OF CONTENTS" or "INTRODUCTION" need to be removed
  text_clean <- text[!grepl("^[A-Z ]+$", text)]  # Remove fully capitalized lines (assumed to be section headers)
  text_clean <- text_clean[!grepl("Table of Contents|Introduction", text_clean)]  # Remove specific unwanted sections
  
  return(text_clean)
})

# Flatten the list into a single vector of text, where each entry represents a paragraph
corpus_text <- unlist(transcripts)

# Create the corpus from the cleaned text
corpus <- corpus(corpus_text)

# Preprocess the corpus
corpus_cleaned <- corpus %>%
  tokens(remove_punct = TRUE) %>%             # Remove punctuation
  tokens_remove(stopwords("en")) %>%          # Remove stopwords (English)
  tokens_wordstem()                           # Perform stemming (word normalization)

# Optionally, preview the cleaned corpus
head(corpus_cleaned, 3)

# Create the document-feature matrix
dfm_clean <- dfm(corpus_cleaned)

# Remove words that appear in fewer than 5 documents
dfm_clean <- dfm_remove(dfm_clean, min_docfreq = 5)

# Preview the DFM
print(dfm_clean)

# Get the top 20 words in the document-feature matrix
top_words <- topfeatures(dfm_clean, 20)

# Print the top 20 words
print(top_words)



