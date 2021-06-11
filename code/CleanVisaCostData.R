# Read data on visa costs

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode")

# Notes

# Load data on visa costs
### ------------------------------------------------------------------------ ###
visa_cost.df <- import("https://zenodo.org/record/4572529/files/GMP_GlobalVisaCostDataset_version1.1.csv")

# Subset to target countries that implemented the Schengen Agreement
### ------------------------------------------------------------------------ ###
# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Merge Schengen member states
visa_cost.df <- visa_cost.df %>%
  filter(target_iso3 %in% schengen.df$iso3_state)

# Schengen visas have a standard cost
visa_cost.df %>% 
  count(tourist_visa)

# Join data on visa requirements
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/visa_requirements_2020.rds") %>%
  filter(destination_iso3 == "EU") 

# Join visa requirements to visa rejection rates
visa_cost.df <- visa_cost.df %>%
  left_join(y = visa.df %>%
              select(nationality_iso3, visa_requirement_binary), 
            by = c("source_iso3" = "nationality_iso3"))

# Schengen visas have a standard cost
visa_cost.df %>% 
  filter(visa_requirement_binary %in% c(0, NA)) %>%
  distinct(source_iso3, .keep_all = TRUE) %>%
  count(tourist_visa)
