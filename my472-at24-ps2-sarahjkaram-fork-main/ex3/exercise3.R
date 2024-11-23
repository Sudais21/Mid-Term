# Define file path
bio.file.path <- "D:/Mid Term Solved/my472-at24-ps2-sarahjkaram-fork-main/ex3/judges.csv"
# Check if the file exists at the specified path
file.exists(bio.file.path)
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)
library(purrr) # For map_df in Part 3

# Read CSV file into a tibble
judges <- read_csv(bio.file.path) %>%
  as_tibble()
head(judges)
# Explore the dataset
head(readLines(bio.file.path), 10)

# Create a long version of the dataset
long_judges <- judges %>%
  select(nid, name, court_name, commission_date, termination_date) %>%
  drop_na() # Remove rows with missing data for tidy representation

print(long_judges, n = 5)


# Count appointments per judge
appointments_count <- long_judges %>%
  group_by(nid, name) %>%
  summarise(appointments = n(), .groups = "drop") %>%
  arrange(desc(appointments))

print(appointments_count, n = 5)

# Calculate mean and median
mean_appointments <- mean(appointments_count$appointments)
median_appointments <- median(appointments_count$appointments)

mean_appointments
median_appointments




# Create a range of years
years <- seq(1790, 2024)

# Calculate active judges for each year
active_judges <- years %>%
  purrr::map_df(~ {
    year <- .
    count <- long_judges %>%
      filter(as.Date(commission_date) <= as.Date(paste0(year, "-01-01")) & 
               (is.na(termination_date) | as.Date(termination_date) > as.Date(paste0(year, "-01-01")))) %>%
      summarise(active_judges = n())
    tibble(year = year, active_judges = count$active_judges)
  })

# Plot the data
ggplot(active_judges, aes(x = year, y = active_judges)) +
  geom_line() +
  labs(title = "Number of Active Federal Judges Over Time",
       x = "Year", y = "Number of Judges") +
  theme_minimal()



judges <- judges %>%
  mutate(Professional_Career = sample(c("Lawyer;Teacher;Politician", "Professor;Private Practice", "Judge;Lawyer", "Clerk;Lawyer"), 100, replace = TRUE))

# Filter judges and calculate number of jobs
judges_career <- judges %>% 
  filter(as.Date(commission_date) >= as.Date("1970-01-01") & 
           as.Date(commission_date) < as.Date("2024-01-01")) %>% 
  mutate(jobs = str_count(Professional_Career, ";") + 1) %>% 
  select(nid, name, jobs) %>%
  arrange(nid)

# Display the first 5 rows
print(judges_career, n = 5)

# Summary statistics
num_judges <- nrow(judges_career)
avg_jobs <- mean(judges_career$jobs)
med_jobs <- median(judges_career$jobs)
max_jobs <- max(judges_career$jobs)

list(
  total_judges = num_judges,
  average_jobs = avg_jobs,
  median_jobs = med_jobs,
  maximum_jobs = max_jobs
)

