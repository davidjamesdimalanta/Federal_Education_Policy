#### Preamble ####
# Purpose: Clean OSSLT data from Data Ontario, Education Policy Spending data from the Government of Canada, and PISA scores data from the OECD
# Author: David James Dimalanta
# Date: 27 March 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(readxl)
library(tidyverse)
library(janitor)
library(arrow)
library(learningtower)
#### Clean data ####

# read in data
raw2017 <- read_xlsx("./data/raw_data/2017_2018.xlsx")
raw2018 <- read_xlsx("./data/raw_data/2018_2019.xlsx")
raw2019 <- read_xlsx("./data/raw_data/2019_2020.xlsx")
raw2020 <- read_xlsx("./data/raw_data/2020_2021.xlsx")
raw2021 <- read_xlsx("./data/raw_data/2021_2023.xlsx")
raw_gov <- read_csv("./data/raw_data/37100212.csv")
raw_data <- load_student("all")

### clean and filter PISA data
pisa_data <- raw_data |>
  select(year, country, math, read, science) |>
  filter(country == "FIN" | country == "HKG" | country == "CAN") |>
  na.omit()

fin_data <- pisa_data |>
  filter(country == "FIN")

can_data <- pisa_data |>
  filter(country == "CAN")

hkg_data <- pisa_data |>
  filter(country == "HKG")

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


#
# make OSSLT 2017 data readable
clean2017 <- raw2017 |>
  clean_names() |>
  select(-c(1:4, 7, 11:24), -extract_date) |>
  mutate(across(c(6:31), ~as.numeric(str_remove(., "%"))))

# make OSSLT 2018 data readable
clean2018 <- raw2018 |>
  clean_names() |>
  select(-c(1:4, 7, 11:24), -extract_date) |>
  mutate(across(c(6:31), ~as.numeric(str_remove(., "%"))))

# make OSSLT 2019 data readable
clean2019 <- raw2019 |>
  clean_names() |>
  select(-c(1:4, 7, 11:24), -extract_date) |>
  mutate(across(c(6:31), ~as.numeric(str_remove(., "%"))))

# make OSSLT 2020 data readable
clean2020 <- raw2020 |>
  clean_names() |>
  select(-c(1:4, 7, 11:24), -extract_date) |>
  mutate(across(c(6:31), ~as.numeric(str_remove(., "%"))))

# make OSSLT 2021 data readable
clean2021 <- raw2021 |>
  clean_names() |>
  select(-c(1:4, 7, 11:24), -extract_date) |>
  mutate(across(c(6:29), ~as.numeric(str_remove(., "%"))))


# filter cleaned 2017 set for OSSLT-related and relevant data
osslt_2017 <- clean2017 |>
  select(
    school_name, school_type,
    percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
    change_in_grade_10_osslt_literacy_achievement_over_three_years,
    percentage_of_school_aged_children_who_live_in_low_income_households,
    percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
    percentage_of_students_whose_first_language_is_not_english,
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  rename(osslt10 = percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
         ossltchange = change_in_grade_10_osslt_literacy_achievement_over_three_years,
         low_inc = percentage_of_school_aged_children_who_live_in_low_income_households,
         no_deg = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
         esl = percentage_of_students_whose_first_language_is_not_english,
         imm = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  filter(school_type == "Public" | school_type == "Catholic") |>
  na.omit()

# filter cleaned 2018 set for OSSLT-related and relevant data
osslt_2018 <- clean2018 |>
  select(
    school_name, school_type,
    percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
    change_in_grade_10_osslt_literacy_achievement_over_three_years,
    percentage_of_school_aged_children_who_live_in_low_income_households,
    percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
    percentage_of_students_whose_first_language_is_not_english,
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  rename(osslt10 = percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
         ossltchange = change_in_grade_10_osslt_literacy_achievement_over_three_years,
         low_inc = percentage_of_school_aged_children_who_live_in_low_income_households,
         no_deg = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
         esl = percentage_of_students_whose_first_language_is_not_english,
         imm = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  filter(school_type == "Public" | school_type == "Catholic") |>
  na.omit()


# filter cleaned 2019 set for OSSLT-related and relevant data
osslt_2019 <- clean2019 |>
  select(
    school_name, school_type,
    percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
    change_in_grade_10_osslt_literacy_achievement_over_three_years,
    percentage_of_school_aged_children_who_live_in_low_income_households,
    percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
    percentage_of_students_whose_first_language_is_not_english,
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  rename(osslt10 = percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
         ossltchange = change_in_grade_10_osslt_literacy_achievement_over_three_years,
         low_inc = percentage_of_school_aged_children_who_live_in_low_income_households,
         no_deg = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
         esl = percentage_of_students_whose_first_language_is_not_english,
         imm = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  filter(school_type == "Public" | school_type == "Catholic") |>
  na.omit()


# filter cleaned 2020 set for OSSLT-related and relevant data
osslt_2020 <- clean2020 |>
  select(
    school_name, school_type,
    percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
    change_in_grade_10_osslt_literacy_achievement_over_three_years,
    percentage_of_school_aged_children_who_live_in_low_income_households,
    percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
    percentage_of_students_whose_first_language_is_not_english,
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  rename(osslt10 = percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
         ossltchange = change_in_grade_10_osslt_literacy_achievement_over_three_years,
         low_inc = percentage_of_school_aged_children_who_live_in_low_income_households,
         no_deg = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
         esl = percentage_of_students_whose_first_language_is_not_english,
         imm = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  filter(school_type == "Public" | school_type == "Catholic") |>
  na.omit()


# filter cleaned 2021 set for OSSLT-related and relevant data
osslt_2021 <- clean2021 |>
  select(
    school_name, school_type,
    percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
    percentage_of_school_aged_children_who_live_in_low_income_households,
    percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
    percentage_of_students_whose_first_language_is_not_english,
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  rename(osslt10 = percentage_of_students_that_passed_the_grade_10_osslt_on_their_first_attempt,
         low_inc = percentage_of_school_aged_children_who_live_in_low_income_households,
         no_deg = percentage_of_students_whose_parents_have_no_degree_diploma_or_certificate,
         esl = percentage_of_students_whose_first_language_is_not_english,
         imm = percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country
  ) |>
  filter(school_type == "Public" | school_type == "Catholic") |>
  na.omit()


### clean canadian government spending data








# write data to parquet file
write_parquet(osslt_2017, "./data/analysis_data/2017.csv")
write_parquet(osslt_2018, "./data/analysis_data/2018.csv")
write_parquet(osslt_2019, "./data/analysis_data/2019.csv")
write_parquet(osslt_2020, "./data/analysis_data/2020.csv")
write_parquet(osslt_2021, "./data/analysis_data/2021.csv")
write_parquet(can_math, "./data/analysis_data/canada_math_scores.csv")
write_parquet(can_read, "./data/analysis_data/canada_literacy_scores.csv")
write_parquet(can_science, "./data/analysis_data/canada_science_scores.csv")
write_parquet(chl_math, "./data/analysis_data/chile_math_score.csv")
write_parquet(chl_read, "./data/analysis_data/chile_literacy_scores.csv")
write_parquet(chl_science, "./data/analysis_data/chile_science_scores.csv")
write_parquet(hkg_math, "./data/analysis_data/hongkong_math_scores.csv")
write_parquet(hkg_read, "./data/analysis_data/hongkong_literacy_scores.csv")
write_parquet(hkg_science, "./data/analysis_data/hongkong_science_scores.csv")




































### graph OSSLT data
# 2017 
  ggplot(osslt_2017, aes(x = osslt10)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of OSSLT Scores in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2017, aes(x = low_inc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of low income families by % in 2018",
    x = "coming from low_income (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2017, aes(x = no_deg)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2017, aes(x = imm)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2017, aes(x = esl)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

# 2018
  ggplot(osslt_2018, aes(x = osslt10)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of OSSLT Scores in 2018",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2018, aes(x = low_inc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of low income families by % in 2018",
    x = "coming from low_income (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2018, aes(x = no_deg)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2018, aes(x = imm)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2018, aes(x = esl)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

# 2019
  ggplot(osslt_2019, aes(x = osslt10)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of OSSLT Scores in 2019",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2019, aes(x = low_inc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of low income families by % in 2018",
    x = "coming from low_income (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2019, aes(x = no_deg)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2019, aes(x = imm)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2019, aes(x = esl)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

# 2020
  ggplot(osslt_2020, aes(x = osslt10)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of OSSLT Scores in 2019",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2020, aes(x = low_inc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of low income families by % in 2018",
    x = "coming from low_income (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2020, aes(x = no_deg)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2020, aes(x = imm)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2020, aes(x = esl)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

# 2021
  ggplot(osslt_2021, aes(x = osslt10)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of OSSLT Scores in 2019",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2021, aes(x = low_inc)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of low income families by % in 2018",
    x = "coming from low_income (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2021, aes(x = no_deg)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2021, aes(x = imm)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)

  ggplot(osslt_2021, aes(x = esl)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Parents with no degree in 2017",
    x = "OSSLT Scores (%)",
    y = "Count"
  ) +
  facet_grid(~ school_type)