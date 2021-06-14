# Analyse visa rejection rates

# Notes/issues

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "fs", "countrycode", "sf")

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

# Join data on visa requirements
### ------------------------------------------------------------------------ ###
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

# Variable for Schengen members and visa exempted states
world.shp <- world.shp %>%
  mutate(rejection_rate_fct = as.numeric(Hmisc::cut2(rejection_rate, cuts = seq(0.1, 0.5, 0.1))),
         
         rejection_rate_cat = case_when(
           visa_requirement_binary == 1 ~ 7,
           eu == "EU" ~ 8,
           TRUE ~ rejection_rate_fct),
         
         rejection_rate_cat = ifelse(is.na(rejection_rate_cat), 9, rejection_rate_cat),
         
         rejection_rate_cat = factor(rejection_rate_cat,  
                                     labels = c("<10%", "<20%", "<30%", 
                                              "<40%", "<50%", ">50%",
                                              "Visa-free entry", "Schengen area",
                                              "No data")))
# Plot
### ------------------------------------------------------------------------ ###
ggplot(world.shp) +
  geom_sf(aes(fill = rejection_rate_cat), lwd = 0.1) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(6, "OrRd"), "#a1d99b", "#e0e0e0", "#1a1a1a")) +
  guides(fill = guide_legend("Rejection rate")) +
  theme_void() 

# Mean rejection rate by world region 
### ------------------------------------------------------------------------ ###
mean_rejection.df <- visa_rejection.df %>%
  filter(visa_requirement_binary %in% c(0, NA)) %>%
  group_by(region) %>%
  summarise(region_mean_rejection_rate = mean(rejection_rate, na.rm = TRUE))

# Join to full df
mean_rejection.df <- visa_rejection.df %>%
  left_join(y = mean_rejection.df, by = c("region")) %>%
  filter(visa_requirement_binary %in% c(0, NA) & !is.na(rejection_rate)) %>%
  mutate(global_mean_rejection_rate = mean(rejection_rate, na.rm = TRUE))

# Plot rejection rates by region
region_rejection_rate.fig <- ggplot(mean_rejection.df, 
                                    aes(
                                      x = fct_reorder(
                                        region, region_mean_rejection_rate),
                                      y = rejection_rate)) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  geom_point(aes(x = region, y = region_mean_rejection_rate), size = 5, 
             shape = 15, alpha = 0.05) + 
  geom_hline(aes(yintercept = global_mean_rejection_rate), color = "black", 
             size = 0.6, linetype = "dashed") +
  annotate("text", x = 1, y = 0.217, label = "Global mean", 
           vjust = 0.5, hjust = 0) +
  annotate("segment", x = 0.75, xend = 1, y = 0.195, yend = 0.215) +
  annotate("text", x = 5.7, y = 0.28, label = "Regional mean",
           vjust = 0.5, hjust = 0) +
  annotate("segment", x = 5.9, xend = 5.7, y = 0.262, yend = 0.278) +
  coord_flip() +
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) +
  labs(x = "", y = "Rejection rate") +
  theme_minimal()