#### Preamble ####
# Purpose: Tests Cleaned Data
# Author: David James Dimalanta
# Date: 27 March 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# - 01-download_data.R
# - 02-data_cleaning.R



#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Read in PISA data ###
fin_data <- read_csv("./data/analysis_data/csv_data/fin_data.csv")
can_data <- read_csv("./data/analysis_data/csv_data/can_data.csv")
chl_data <- read_csv("./data/analysis_data/csv_data/chl_data.csv")
hkg_data <- read_csv("./data/analysis_data/csv_data/hkg_data.csv")

#### Test CAN data ####
test_that("Data is read correctly", {
  expect_true("year" %in% names(can_data))
  expect_true("country" %in% names(can_data))
  expect_true("math" %in% names(can_data))
  expect_true("read" %in% names(can_data))
  expect_true("science" %in% names(can_data))
})

#### Test FIN data ####
test_that("Data is read correctly", {
  expect_true("year" %in% names(fin_data))
  expect_true("country" %in% names(fin_data))
  expect_true("math" %in% names(fin_data))
  expect_true("read" %in% names(fin_data))
  expect_true("science" %in% names(fin_data))
})

#### Test CHL data ####
test_that("Data is read correctly", {
  expect_true("year" %in% names(chl_data))
  expect_true("country" %in% names(chl_data))
  expect_true("math" %in% names(chl_data))
  expect_true("read" %in% names(chl_data))
  expect_true("science" %in% names(chl_data))
})

#### Test hkg data ####
test_that("Data is read correctly", {
  expect_true("year" %in% names(hkg_data))
  expect_true("country" %in% names(hkg_data))
  expect_true("math" %in% names(hkg_data))
  expect_true("read" %in% names(hkg_data))
  expect_true("science" %in% names(hkg_data))
})

test_numeric_columns <- function(data, columns) {
  for (column in columns) {
    test_that(paste("Column", column, "in data is numeric and contains only real numbers"), {
      expect_type(data[[column]], "double")  # Expects columns to be of type double
      expect_true(all(is.finite(data[[column]])))  # Checks for NA, NaN, Inf, -Inf
    })
  }
}

#### Test CAN data ####
test_numeric_columns(can_data, c("math", "read", "science"))

#### Test FIN data ####
test_numeric_columns(fin_data, c("math", "read", "science"))

#### Test CHL data ####
test_numeric_columns(chl_data, c("math", "read", "science"))

#### Test HKG data ####
test_numeric_columns(hkg_data, c("math", "read", "science"))