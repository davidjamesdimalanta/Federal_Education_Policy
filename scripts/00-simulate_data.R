#### Preamble ####
# Purpose: Simulates PISA Score Data
# Author: David James Dimalanta
# Date: April 24 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA


#### Workspace setup ####
library(tidyverse)
#### Simulate data ####

years <- 1972:2022
low_scores <- 500
high_scores <- 800

# Simulate PISA score data
pisa_scores <-
  tibble(
    pisa_scores_count =
    runif(n = length(years), min = low_scores, max = high_scores),
    noise = rnorm(n = length(years), mean = 0, sd = 3)
  ) |>
  select(-noise)

print(pisa_scores, n = 200)