# Read data on visa issuance, rejections and applications

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs")

#
files.df <- tibble(
  names = dir_ls("./data")) %>%
  mutate(year = strtoi(str_extract(names, "[:digit:]{4}"))) %>%
  filter(!is.na(year), between(year, 2014, 2019), str_detect(names, "consulates")) %>%
  mutate(data = map(names, ~import(.x, sheet = 2))) %>%
  mutate(data = map(data, ~.x %>%
                      select(1:3, 9:15) %>%
           clean_names())) %>%
  mutate(data = map(data, ~set_names(.x, c("schengen_state", "origin_country",
                                           "consulate", "num_applied_visa", 
                                           "total_visa_issued", "num_mev_visa_issued",
                                           # add colnames))))
           
  
