# Read data on visa issuance, rejections and applications

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs")

# Notes
# Types of visas: 
# - A: airport transit visa (ATV)
# - C: uniform visa (single entry) or MEV (multiple entry visa)
# - LTV: limited territorial validity

# Data from 2014 - 2019
files.df <- tibble(
  names = dir_ls("./data")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}"))) %>%
  filter(!is.na(year), between(year, 2014, 2019), str_detect(names, "consulates")) %>%
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
                                           "rejection_rate_uniform_visa"))))
  
# Data for 2013
file2013.df <- tibble(
  names = dir_ls("./data")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}"))) %>%
  filter(!is.na(year), year == 2013, str_detect(names, "synthese")) %>%
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
                                           "rejection_rate_uniform_visa"))))

# Data for 2010 - 2012
# Data for 2013
file2011_2012.df <- tibble(
  names = dir_ls("./data")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}"))) %>%
  filter(!is.na(year), between(year, 2011, 2012), str_detect(names, "synthese")) %>%
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
                                           "num_uniform_visa_not_issued")))) %>%
  mutate(data = map(data, ~.x %>% 
                      mutate(rejection_rate_uniform_visa = num_uniform_visa_not_issued / num_uniform_visa_applied)))
