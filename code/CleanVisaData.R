# Read data on visa issuance, rejections and applications

# Notes/issues
# - empty cells in 'rejection_rate_uniform_visa' are turned into 0.0 not NA
# - check whether computation of rejection rate varies between tables

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode")

# Notes
# Visa categories:
# - A: airport transit visa (ATV)
# - B (defunct): transit visa (over land)
# - C: uniform visa (single entry) or MEV (multiple entry visa)
# - LTV: limited territorial validity

# All available tables 2009 - 2020
files.df <- tibble(
  names = dir_ls("./data")) %>%
  filter(str_detect(names, "visa_statistics_[:digit:]{4}")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}")))

# 2014 - 2020
files2014f.df <- files.df %>%
  filter(between(year, 2014, 2020)) %>%
  mutate(data = map(names, ~import(.x, sheet = 2))) %>%
  mutate(data = map(data, ~.x %>%
                      select(1:3, 9:15) %>%
           clean_names())) %>%
  mutate(data = map(data, ~set_names(.x, c("schengen_state", "application_country",
                                           "consulate_location", "num_uniform_visa_applied", 
                                           "total_uniform_visa_issued", "num_mev_visa_issued",
                                           "share_mev_of_total_uniform_visa_issued", 
                                           "total_limited_visa_issued", 
                                           "num_uniform_visa_not_issued", 
                                           "rejection_rate_uniform_visa"))),
         # end of relevant table (multiple table per sheet)
         cutpoint = c(1963, 1943, 1881, 1872, 1901, 1835, 1706)) %>% 
  mutate(data = map2(data, cutpoint, ~.x %>%
                      filter(row_number() < .y))) %>%
  select(-cutpoint)
  
# Data for 2013
files2013.df <- files.df %>%
  filter(year == 2013) %>%
  mutate(data = map(names, ~import(.x, sheet = 1))) %>%
  mutate(data = map(data, ~.x %>%
                      select(1:3, 9:15) %>%
                      clean_names())) %>%
  mutate(data = map(data, ~set_names(.x, c("schengen_state", "application_country",
                                           "consulate_location", "num_uniform_visa_applied", 
                                           "total_uniform_visa_issued", "num_mev_visa_issued",
                                           "share_mev_of_total_uniform_visa_issued", 
                                           "total_limited_visa_issued", 
                                           "num_uniform_visa_not_issued", 
                                           "rejection_rate_uniform_visa"))),
         cutpoint = 1992) %>%
  mutate(data = map2(data, cutpoint, ~.x %>%
                       filter(row_number() < .y))) %>%
  select(-cutpoint)

# Data for 2011 - 2012
files2011f.df <- files.df %>%
  filter(!is.na(year), between(year, 2011, 2012)) %>%
  mutate(data = map(names, ~import(.x, sheet = 1))) %>%
  mutate(data = map(data, ~.x %>%
                      select(1, 3:4, 12, 9:11, 18, 13) %>%
                      clean_names() %>%
                      filter(row_number() != 1))) %>%
  mutate(data = map(data, ~set_names(.x, c("schengen_state", "application_country",
                                           "consulate_location", "num_uniform_visa_applied", 
                                           "total_uniform_visa_issued", "num_mev_visa_issued",
                                           "share_mev_of_total_uniform_visa_issued", 
                                           "total_limited_visa_issued", 
                                           "num_uniform_visa_not_issued"))),
         cutpoint = c(2071, 2024)) %>%
  mutate(data = map2(data, cutpoint, ~.x %>%
                       filter(row_number() < .y))) %>%
  select(-cutpoint) %>%
  mutate(data = map(data, ~.x %>% 
                      mutate(rejection_rate_uniform_visa = num_uniform_visa_not_issued / num_uniform_visa_applied)))

# Data for 2010
files2010.df <- files.df %>%
  filter(year == 2010) %>%
  mutate(data = map(names, ~import(.x, sheet = 1))) %>%
  mutate(data = map(data, ~.x %>%
                      select(3, 1:2, 10, 7:9, 15) %>%
                      clean_names())) %>%
  mutate(data = map(data, ~set_names(.x, c("schengen_state", "application_country",
                                           "consulate_location", "num_uniform_visa_applied", 
                                           "total_uniform_visa_issued", "num_mev_visa_issued",
                                           "share_mev_of_total_uniform_visa_issued", 
                                           "total_limited_visa_issued"))),
         cutpoint = 2198) %>%
  mutate(data = map2(data, cutpoint, ~.x %>%
                       filter(row_number() < .y))) %>%
  select(-cutpoint) %>%
  mutate(data = map(data, ~.x %>% 
                      mutate(num_uniform_visa_not_issued = num_uniform_visa_applied - (total_uniform_visa_issued + total_limited_visa_issued),
                             rejection_rate_uniform_visa = num_uniform_visa_not_issued / num_uniform_visa_applied,
                             schengen_state = countrycode(schengen_state, "eurostat", "country.name.en"))))

# Data for 2009
# Data in different format; needs further clarification

# Join datasets, unnest, clean
### ------------------------------------------------------------------------ ###
# Data for 2010 - 2020
visa.df <- files2014f.df %>%
  bind_rows(files2013.df, files2011f.df, files2010.df) %>%
  select(-names) %>%
  unnest(data)

# Further cleaning
visa.df <- visa.df %>%
  mutate(across(where(is_character), str_to_title),
         across(c(schengen_state, application_country), 
                ~countrycode(., "country.name.en", "iso3c", 
                             custom_match = c("Kosovo" = "XKX"))))

# Subset to countries that implemented the Schengen Agreement
# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Remove visa applications from within the Schengen Area
visa.df <- visa.df %>%
  filter(!application_country %in% schengen.df$iso3_state)

# The tables provide a rejection rate calculated as: visa not issued / visa applied
# Hobolth (2014) defines it as "the number of refusals divided by the total number of 
# decisions (refused plus issued)." (p. 429)

# Sample with full information by country
visa.df <- visa.df %>%
  group_by(schengen_state, application_country, year) %>%
  filter(!any(is.na(c(num_uniform_visa_not_issued, 
                      num_uniform_visa_applied)))) %>%
  ungroup()

# Remove data that exceeds logical range
#

# Create the refusal rate by countries (aggregating consulate data)
visa.df <- visa.df %>%
  select(schengen_state, application_country, year, num_uniform_visa_not_issued, 
         num_uniform_visa_applied) %>%
  group_by(schengen_state, application_country, year) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(refusal_rate = num_uniform_visa_not_issued / num_uniform_visa_applied)

# Arrange
visa.df %>%
  arrange(desc(refusal_rate))

# Remove negative and Inf refusal rates
#

# Regions
visa.df <- visa.df %>%
  mutate(application_continent = countrycode(application_country,
                                          "iso3c", "continent", 
                                          custom_match = c("XKX" = "Europe")))

# Explorative data analysis
visa_region.df <- visa.df %>%
  group_by(application_continent, year) %>%
  summarise(mean_refusal_rate = mean(refusal_rate)) %>%
  filter(!is.na(mean_refusal_rate) & !is.nan(mean_refusal_rate) & mean_refusal_rate >= 0)

ggplot(visa_region.df, aes(x = factor(year), y = mean_refusal_rate, group = application_continent, colour = application_continent)) +
  geom_point(stat = "identity") +
  geom_line() +
  theme_minimal()
