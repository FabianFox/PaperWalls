# Read data on visa issuance, rejections and applications

# Notes/issues

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode", "states", "lubridate")

# Notes
# Visa categories:
# - A: airport transit visa (ATV)
# - B (defunct): transit visa (over land)
# - C: uniform visa (single entry) or MEV (multiple entry visa)
# - LTV: limited territorial validity

# All available tables 2015 - 2020
# Note: Data dates back to 2009 but format changes
files.df <- tibble(
  names = dir_ls("./data")) %>%
  filter(str_detect(names, "visa_statistics_[:digit:]{4}")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}"))) %>%
  filter(between(year, 2015, 2020)) 

# Import rejection rates
### ------------------------------------------------------------------------ ###
visa_reject.df <- files.df %>%
  mutate(data = map(names, ~import(.x, sheet = 7, skip = 1))) %>%
  mutate(data = map(data, ~.x %>%
                      select(2:4, 6:8) %>%
                      set_names(c("country", "applications", "issued", 
                                  "issued_ltv", "not_issued", "rejection_rate")) %>%
                      mutate(across(where(is.character), str_to_title),
                             country_iso3 = countrycode(country, 
                                                        "country.name.en", 
                                                        "iso3c", 
                                                        custom_match = c("Kosovo" = "RKS",
                                                                         "Grand Total" = "Total")))))

# Join datasets, unnest, clean
### ------------------------------------------------------------------------ ###
# Data for 2015 - 2020
visa_reject_long.df <- visa_reject.df %>%
  unnest(data) %>%
  filter(!country == "Grand Total")

# Subset to countries that implemented the Schengen Agreement
# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Remove visa applications from within the Schengen Area
# Note: Check whether composition of Schengen Area changed
visa_reject_long.df <- visa_reject_long.df %>%
  filter(!country_iso3 %in% schengen.df$iso3_state)
  
# Continents and regions
visa_reject_long.df <- visa_reject_long.df %>%
  mutate(continent = countrycode(country_iso3,
                                 "iso3c", "continent", 
                                 custom_match = c("RKS" = "Europe", 
                                                  "MKD" = "Europe", 
                                                  "VAT" = "Europe")),
         region = countrycode(country_iso3, "iso3c", "region",
                              custom_match = c("RKS" = "Europe & Central Asia",
                                               "MKD" = "Europe & Central Asia",
                                               "TWN" = "East Asia & Pacific",
                                               "VAT" = "Europe")))

# Export
### ------------------------------------------------------------------------ ###
export(visa_reject_long.df, "./data/visa_rejection_2015-2020.rds")
