# Modeling visa rejection rates
 
# Notes/issues

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "fs", "countrycode", "sampleSelection", "wbstats")

# Load data
### ------------------------------------------------------------------------ ###
visa_rejection.df <- import("./data/visa_rejection_2015-2020.rds") %>%
  filter(year == 2019)

# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Remove rows with missing values
### ------------------------------------------------------------------------ ###
visa_rejection.df <- visa_rejection.df %>%
  filter(across(everything(), ~!is.na(.x)))

# Remove states that are exempted from visa requirements
visa.df <- import("./data/visa_requirements_2020.rds") %>%
  filter(destination_iso3 == "EU") 

# Join visa requirements to visa rejection rates
visa_rejection.df <- visa_rejection.df %>%
  left_join(y = visa.df %>%
              select(nationality_iso3, visa_requirement_binary), 
            by = c("country_iso3" = "nationality_iso3"))

# Recode missing values of TWN and XKX
visa_rejection.df[visa_rejection.df$country_iso3 == "TWN", "visa_requirement_binary"] <- 1
visa_rejection.df[visa_rejection.df$country_iso3 == "XKX", "visa_requirement_binary"] <- 0

# Join independent variables
# PolityV data
### ------------------------------------------------------------------------ ###
# PolityV project
# Year: 2018 
# Custom match for PolityV data
custom.pol <- c("342" = "SRB", "348" = "MNE", "525" = "SSD", "529" = "ETH", 
                "818" = "VNM")
# Load data
polity.df <- import("http://www.systemicpeace.org/inscr/p5v2018.sav") %>%
  filter(year == 2018) %>%
  mutate(iso3c = countrycode(ccode, "cown", "iso3c", 
                             custom_match = custom.pol)) %>%
  select(iso3c, polity2)

# Join to visa.df 
visa_rejection.df <- visa_rejection.df %>%
  left_join(y = polity.df, by = c("country_iso3" = "iso3c"))

# Economic capabilities
# World Bank Indicator
# GDP per capita, PPP (current international $) - "NY.GDP.PCAP.PP.CD"
# Total Population - "SP.POP.TOTL"
# Year: Mean/Median (2015-2019)
## -------------------------------------------------------------------------- ##
# (1) Download WB data
# Download data (mrv = newest available)
wb.info <- wb_data(country = unique(visa_rejection.df$country_iso3),
                   indicator = c("NY.GDP.PCAP.PP.CD", "SP.POP.TOTL"), 
                   start_date = 2019, end_date = 2019, return_wide = TRUE)

# Rename
wb.info <- wb.info %>%
  select(iso3c, 
         "gdp_pp_pc" = "NY.GDP.PCAP.PP.CD", 
         "population_size" = "SP.POP.TOTL")

# Join
visa_rejection.df <- visa_rejection.df %>%
  left_join(y = wb.info, by = c("country_iso3" = "iso3c"))

# Model
## -------------------------------------------------------------------------- ##
# Create model data
visa_model.df <- visa_rejection.df %>%
  select(country_iso3, rejection_rate, region, visa_requirement_binary, 
         rejection_rate, polity2, gdp_pp_pc) %>%
  filter(across(c(visa_requirement_binary, rejection_rate,
                  polity2, gdp_pp_pc), ~!is.na(.))) %>%
  mutate(visa_requirement_binary = car::recode(visa_requirement_binary, 
                                               "0='1';1='0'"))

# Sample selection model
m1 <- selection(visa_requirement_binary ~ polity2,
                rejection_rate ~ gdp_pp_pc, data = visa_model.df)
