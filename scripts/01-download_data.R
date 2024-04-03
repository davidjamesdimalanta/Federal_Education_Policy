#### Preamble ####
# Purpose: Downloads and saves OSSLT scores from Data Ontario and PISA scores from the Learning Tower package
# Author: David James Dimalanta
# Date: 27 March 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites: none


### Workspace Setup###
library(learningtower)

#### Download PISA data ####
raw_data <- load_student("all")

data(countrycode)

# download OSSLT 2017-2018 data through URL
"https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/3d8e7824-d033-4ca5-a5de-aa0dbec99002/download/sif_data_table_2017_2018_en.xlsx"

# download OSSLT 2018-2019 data through URL
"https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/bbbac037-b1c9-4b32-8f67-4fed1dfcac06/download/sif_data_table_2018_2019_en.xlsx"

# download OSSLT 2019-2020 data through URL
"https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/sif_data_table_2019_2020_en.xlsx"

# download OSSLT 2020-2021 data through URL
"https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/600ab128-f50f-43a4-b4d4-97c39bb270b6/download/sif_data_table_2020_2021_en.xlsx"

# download OSSLT 2021-2023 data through URL
"https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/e0e90bd5-d662-401a-a6d2-60d69ac89d14/download/new_sif_data_table_2021_22prelim_en_march2024.xlsx"

# download Distribution of total and current expenditure by educational institutions, from public and private sources, by level of education - Dataset frmo Government of Canada
"https://www150.statcan.gc.ca/n1/tbl/csv/37100212-eng.zip"

