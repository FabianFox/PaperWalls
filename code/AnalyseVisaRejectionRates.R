# Analyse visa rejection rates

# Notes/issues

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "fs", "countrycode")

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

# Plot rejection rates
### ------------------------------------------------------------------------ ###
# Load world shapefile
world.shp <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Aggregate EU
world.shp <- world.shp %>%
  filter(type %in% c("Sovereign country", "Country"),
         !name %in% c("Greenland")) %>%
  mutate(
    eu = case_when(
      iso_a3_eh %in% schengen.df$iso3_state ~ "EU",
      sovereignt == "Norway" ~ "EU",
      sovereignt == "Kosovo" ~ "XKX",
      TRUE ~ iso_a3_eh
    )) %>%
  group_by(eu) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  filter(!is.na(eu))

# Join rejection rates
world.shp <- world.shp %>%
  left_join(y =  visa_rejection.df %>%
              select(country_iso3, rejection_rate, visa_requirement_binary),
            by = c("eu" = "country_iso3"))

# Plot
### ------------------------------------------------------------------------ ###
ggplot(world.shp) +
  geom_sf(aes(fill = rejection_rate, linetype = "no data")) +
  scale_fill_viridis_c(na.value = "black") +
  scale_colour_manual(values = NA, name = "No data") +
  guides(fill = guide_legend("Rejection rate"), linetype = guide_legend("No data", override.aes = list(fill = "black"))) +
  theme_void() 
