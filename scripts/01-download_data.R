#### Preamble ####
# Purpose: Downloads and saves PISA scores from the Learning Tower package
# Author: David James Dimalanta
# Date: 24 April 2024
# Contact: david.dimalanta@mail.utoronto.ca
# License: MIT
# Pre-requisites: none


### Workspace Setup###
library(learningtower)

#### Download PISA data from Learning Tower ####
raw_data <- load_student("all")

data(countrycode)

# download federal education spending from The World Bank by URL
"https://api.worldbank.org/v2/en/indicator/SE.XPD.TOTL.GB.ZS?downloadformat=csv"