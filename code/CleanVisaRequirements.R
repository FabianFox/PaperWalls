# Read data on visa requirements

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode")

# Notes

# Load data on visa requirements
# created in Repository "Visa" in file Visa_Scraper.R
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/VWP_07_2020.RDS") %>%
  mutate(requirement = flatten_chr(requirement)) %>%
  ungroup() %>%
  filter(destination_iso3 != "XKX")

# Data cleaning
### ------------------------------------------------------------------------###
# (1) Split requirement into information on visa and passport requirements
# (2) Create a binary indicator on visa waivers (1 = visa req. waived; 0 = visa req)
visa.df <- visa.df %>%
  mutate(
    passport_requirement = str_extract(requirement, "(?<=\\n).+"),
    visa_requirement = str_extract(requirement, ".+(?=\\n)"),
    visa_requirement_binary = case_when(
      str_detect(requirement, 
                 "Visa is required|Es ist ein Visum erforderlich|Visa may be obtained on arrival") ~ 0,
      str_detect(requirement, 
                 "Visa is not required") ~ 1,
      TRUE ~ NA_real_),
    across(c("destination_iso3", "nationality_iso3"), ~str_replace(.x, "\\bD\\b", "DEU")))

# Subset
# also exclude Taiwan (TWN) due to many missing values
visa.df <- visa.df %>%
  filter(!destination_iso3 %in% c("ESH", "PSE"), 
         !nationality_iso3 %in% c("ESH", "PSE"))

# Manually add missing information on a few dyads in 2020
# Remaining missing cases: 
# - CHN - HKG
# - BRA - RKS
# - ZAF - RKS
# - BGD - RKS
# - CHN - MAC
# - CHN - TWN
# - CUB - RKS
# - SRB - RKS

### ------------------------------------------------------------------------###
# CAN -> SOM (visa required)
visa.df[visa.df$destination_iso3 == "CAN" & 
               visa.df$nationality_iso3 == "SOM",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# BRA -> CAF (visa required)
visa.df[visa.df$destination_iso3 == "BRA" & 
               visa.df$nationality_iso3 == "CAF",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# ARE -> ISR (visa required / not applicable)
visa.df[visa.df$destination_iso3 == "ARE" & 
               visa.df$nationality_iso3 == "ISR",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# GBR -> NAM (no visa required)
visa.df[visa.df$destination_iso3 == "GBR" & 
               visa.df$nationality_iso3 == "NAM",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# BLZ -> PAN (visa required)
visa.df[visa.df$destination_iso3 == "BLZ" & 
               visa.df$nationality_iso3 == "PAN",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# ARE -> QAT (visa required)
visa.df[visa.df$destination_iso3 == "ARE" & 
               visa.df$nationality_iso3 == "QAT",
             c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)


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
