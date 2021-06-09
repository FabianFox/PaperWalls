# Read data on visa requirements

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode")

# Notes

# Load data on visa requirements
# created in Repository "Visa" in file Visa_Scraper.R and CreateVisaNetworkData2020.R
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/visa_2020.rds")

# Subset to countries that implemented the Schengen Agreement
### ------------------------------------------------------------------------ ###
# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Merge Schengen member states
visa.df <- visa.df %>%
  mutate(destination_iso3 = if_else(destination_iso3 %in% schengen.df$iso3_state, 
                                    "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 %in% schengen.df$iso3_state, 
                                    "EU", nationality_iso3)) %>%
  distinct(destination_iso3, nationality_iso3, .keep_all = TRUE) %>%
  filter(!(destination_iso3 == "EU" & nationality_iso3 == "EU")) 

# Export
### ------------------------------------------------------------------------ ###
export(visa.df, "./data/visa_requirements_2020.rds")
