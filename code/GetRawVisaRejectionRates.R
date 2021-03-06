# Download statistics on short-stay visas from European Commission
# URL: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/visa-policy_en
# Date: 2021/04/22

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rvest")

# Download .xlsx files from https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/visa-policy_en
### ------------------------------------------------------------------------ ###

# Base url
url <- "https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/visa-policy_en"

# Files
page <- read_html(url)

# Locate href to .xlsx
# also gets a document on the location of consulates, but not on external border
links.df <- tibble(
  links = html_nodes(page, xpath = ".//a[contains(@href, '.xls')]") %>% 
    html_attr("href"),
  name = paste0("visa_statistics_", str_extract_all(links, "[:digit:]{4}"),
                ifelse(str_detect(links, "xlsx"), ".xlsx", ".xls"))) %>%
  filter(!str_detect(links, "bcps|external_border_crossing|list_of_consular_presence"))

# Download
map2(links.df$links, links.df$name, ~download.file(url = .x, 
                                   destfile = paste0(getwd(), "/data/", .y), 
                                   mode = "wb"))
