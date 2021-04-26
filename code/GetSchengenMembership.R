# Information on Schengen membership from Wikipedia
if(!require(xfun)) install.packages("xfun")
pkg_attach2("tidyverse", "rvest", "countrycode", "lubridate")

schengen.df <- read_html("https://en.wikipedia.org/wiki/Schengen_Area") %>%
  html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div/table[3]") %>%
  html_table(header = T, fill = T) %>%
  select(1,4,5) %>%
  set_names("member_state", "signed", "implemented") %>%
  mutate_all(list(~qdap::bracketX(.))) %>%
  mutate_at(vars("signed", "implemented"), list(~dmy(.))) %>%
  .[-27,] %>% # - Schengen Area
  mutate(iso3_state = countrycode(member_state, "country.name.en", "iso3c")) %>%
  select(-member_state)

# Iceland/Norway have an additional agreement. Only the multilateral agreement
# is considered here. 
schengen.df[11, "signed"] <- dmy("19 December 1996")
schengen.df[19, "signed"] <- dmy("19 December 1996")

# Save Schengen membership data
export(schengen.df, "./data/SchengenMembership.rds")
