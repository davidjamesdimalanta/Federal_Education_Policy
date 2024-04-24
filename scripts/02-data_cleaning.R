#### Preamble ####
# Purpose: Clean Education Policy Spending data from the World Bank Data, and PISA scores data from the OECD
# Author: David James Dimalanta
# Date: 27 March 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(readxl)
library(tidyverse)
library(janitor)
library(learningtower)
library(knitr)
library(arrow)
#### Clean data ####

# read in data
raw_gov <- read_csv("./data/raw_data/37100212.csv")
raw_data <- load_student("all")
raw_spending <- read_csv("./data/raw_data/wbd.csv")

# clean and filter PISA data
pisa_data <- raw_data |>
  select(year, country, math, read, science) |>
  filter(country == "FIN" |
           country == "HKG" |
           country == "CAN" |
           country == "CHL") |>
  na.omit()

fin_data <- pisa_data |>
  filter(country == "FIN")

can_data <- pisa_data |>
  filter(country == "CAN")

hkg_data <- pisa_data |>
  filter(country == "HKG")

chl_data <- pisa_data |>
  filter(country == "CHL")

fin_read <- fin_data |>
  group_by(year) |>
  summarise(
    lowest = min(read, na.rm = TRUE),
    highest = max(read, na.rm = TRUE),
    average = mean(read, na.rm = TRUE)
  )

can_read <- can_data |>
  group_by(year) |>
  summarise(
    lowest = min(read, na.rm = TRUE),
    highest = max(read, na.rm = TRUE),
    average = mean(read, na.rm = TRUE)
  )

hkg_read <- hkg_data |>
  group_by(year) |>
  summarise(
    lowest = min(read, na.rm = TRUE),
    highest = max(read, na.rm = TRUE),
    average = mean(read, na.rm = TRUE)
  )

chl_read <- chl_data |>
  group_by(year) |>
  summarise(
    lowest = min(read, na.rm = TRUE),
    highest = max(read, na.rm = TRUE),
    average = mean(read, na.rm = TRUE)
  )

fin_math <- fin_data |>
  group_by(year) |>
  summarise(
    lowest = min(math, na.rm = TRUE),
    highest = max(math, na.rm = TRUE),
    average = mean(math, na.rm = TRUE)
  )

can_math <- can_data |>
  group_by(year) |>
  summarise(
    lowest = min(math, na.rm = TRUE),
    highest = max(math, na.rm = TRUE),
    average = mean(math, na.rm = TRUE)
  )

hkg_math <- hkg_data |>
  group_by(year) |>
  summarise(
    lowest = min(math, na.rm = TRUE),
    highest = max(math, na.rm = TRUE),
    average = mean(math, na.rm = TRUE)
  )

chl_math <- chl_data |>
  group_by(year) |>
  summarise(
    lowest = min(math, na.rm = TRUE),
    highest = max(math, na.rm = TRUE),
    average = mean(math, na.rm = TRUE)
  )

fin_science <- fin_data |>
  group_by(year) |>
  summarise(
    lowest = min(science, na.rm = TRUE),
    highest = max(science, na.rm = TRUE),
    average = mean(science, na.rm = TRUE)
  )

can_science <- can_data |>
  group_by(year) |>
  summarise(
    lowest = min(science, na.rm = TRUE),
    highest = max(science, na.rm = TRUE),
    average = mean(science, na.rm = TRUE)
  )

hkg_science <- hkg_data |>
  group_by(year) |>
  summarise(
    lowest = min(science, na.rm = TRUE),
    highest = max(science, na.rm = TRUE),
    average = mean(science, na.rm = TRUE)
  )

chl_science <- chl_data |>
  group_by(year) |>
  summarise(
    lowest = min(science, na.rm = TRUE),
    highest = max(science, na.rm = TRUE),
    average = mean(science, na.rm = TRUE)
  )

### world bank data
# Filter for relevant countries and years
cleaned_wbd_spending <- raw_spending |>
  clean_names() |>
  select(-indicator_name, -indicator_code, -x2023, -x69) |>
  filter(country_code == "CAN" |
           country_code == "CHL" |
           country_code == "FIN" |
           country_code == "HKG") |>
  select(where(~ !anyNA(.)))

# function to clean column names by removing the 'x' prefix 
clean_column_names <- function(df) {
  colnames(df) <- colnames(df) |>
    str_replace_all("^x", "")  # Replace 'x' at the beginning of column names
  return(df)
}

# Split the dataframe by country code and clean column names
can_spending <- cleaned_wbd_spending |>
  filter(country_code == "CAN") |>
  clean_column_names()

chl_spending <- cleaned_wbd_spending |>
  filter(country_code == "CHL") |>
  clean_column_names()

fin_spending <- cleaned_wbd_spending |>
  filter(country_code == "FIN") |>
  clean_column_names()

hkg_spending <- cleaned_wbd_spending |>
  filter(country_code == "HKG") |>
  clean_column_names()


# Transform data from wide to long format for plotting
can_long <- can_spending |>
  pivot_longer(
    cols = -c(country_name, country_code), 
    names_to = "year", 
    values_to = "value"
  ) |>
  mutate(year = as.numeric(year)) |>
  filter(year >= 2000)

# Create a line plot
ggplot(can_long, aes(x = year, y = value, group = country_code, color = country_code)) +
  geom_line() +
  labs(title = "Education Spending as a Percentage of GDP Over Time",
       subtitle = "Data for Canada",
       x = "Year",
       y = "Spending (% of GDP)",
       color = "Country Code") +
  theme_minimal()


# write data to csv file
write_csv(can_math, "./data/analysis_data/csv_data/canada_math_scores.csv")
write_csv(can_read, "./data/analysis_data/csv_data/canada_literacy_scores.csv")
write_csv(can_science, "./data/analysis_data/csv_data/canada_science_scores.csv")
write_csv(chl_math, "./data/analysis_data/csv_data/chile_math_score.csv")
write_csv(chl_read, "./data/analysis_data/csv_data/chile_literacy_scores.csv")
write_csv(chl_science, "./data/analysis_data/csv_data/chile_science_scores.csv")
write_csv(hkg_math, "./data/analysis_data/csv_data/hongkong_math_scores.csv")
write_csv(hkg_read, "./data/analysis_data/csv_data/hongkong_literacy_scores.csv")
write_csv(hkg_science, "./data/analysis_data/csv_data/hongkong_science_scores.csv")
write_csv(fin_math, "./data/analysis_data/csv_data/finland_math_scores.csv")
write_csv(fin_read, "./data/analysis_data/csv_data/finland_literacy_scores.csv")
write_csv(fin_science, "./data/analysis_data/csv_data/finland_science_scores.csv")
write_csv(cleaned_wbd_spending, "./data/analysis_data/csv_data/wbd_spending.csv")
write_csv(fin_data, "./data/analysis_data/csv_data/fin_data.csv")
write_csv(can_data, "./data/analysis_data/csv_data/can_data.csv")
write_csv(chl_data, "./data/analysis_data/csv_data/chl_data.csv")
write_csv(hkg_data, "./data/analysis_data/csv_data/hkg_data.csv")
write_csv(can_spending, "./data/analysis_data/csv_data/can_spending.csv")
write_csv(fin_spending, "./data/analysis_data/csv_data/fin_spending.csv")
write_csv(chl_spending, "./data/analysis_data/csv_data/chl_spending.csv")
write_csv(hkg_spending, "./data/analysis_data/csv_data/hkg_spending.csv")


# write data to parquet ***DOESNT WORK***
write_parquet(
  can_math,
  "./data/analysis_data/parquet_data/canada_math_scores.csv",
  chunk_size = NULL,
  version = "2.4",
  compression = default_parquet_compression(),
  compression_level = NULL,
  use_dictionary = NULL,
  write_statistics = NULL,
  data_page_size = NULL,
  use_deprecated_int96_timestamps = FALSE,
  coerce_timestamps = NULL,
  allow_truncated_timestamps = FALSE
)
