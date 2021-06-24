# Analysis of visa requirements

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "sf", "ggraph", "tidygraph", 
            "igraph", "extrafont")

# theme
theme_basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 16, family = "Garamond"),
    axis.ticks.x = element_line(size = .5),
  )

# Load data
### ------------------------------------------------------------------------ ###
# Visa requirements
visa.df <- import("./data/visa_requirements_2020.rds")

# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Compute in-/ and outdegree
# Note: Check countrycode for Kosovo (XKX or RKS)
### ------------------------------------------------------------------------ ###
# Outdegree
visa_outdegree.df <- visa.df %>%
  group_by(destination_iso3) %>%
  summarise(outdegree = sum(visa_requirement_binary))

# Indegree
visa_indegree.df <- visa.df %>%
  group_by(nationality_iso3) %>%
  summarise(indegree = sum(visa_requirement_binary))

# Join
visa_degree.df <- visa_outdegree.df %>%
  left_join(visa_indegree.df, by = c("destination_iso3" = "nationality_iso3")) %>%
  rename(country_iso3 = destination_iso3)

# Scatterplot: In-/outdegree
### ------------------------------------------------------------------------ ###
eu_data <- visa_degree.df %>% 
  filter(country_iso3 == "EU")

# Plot
visa_scatter.fig <- ggplot(visa_degree.df, aes(x = outdegree, y = indegree)) +
  geom_point(color = "grey") +
  geom_point(visa_degree.df %>%
               filter(country_iso3 == "EU"), 
             mapping = aes(x = outdegree, y = indegree), color = "red",
             size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  annotate("segment", x = eu_data$outdegree, xend = eu_data$outdegree, 
            y = eu_data$outdegree, yend = eu_data$indegree, colour = "black", linetype = "dashed") +
  annotate("text", x = 47, y = 80, 
           label = paste0(
             "The EU receives ", eu_data$indegree - eu_data$outdegree, 
             " more\nvisa waivers than it provides."), 
           hjust = 0, family = "Garamond", size = 5) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Visa Freedom (sent)", y = "Visa freedom (received)", title = "Visa Freedom, 2020", 
       caption = "Data: ICTS Europe Systems (2020)") +
  theme_basic

# Plot network of EU visa agreements
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

# Find centroids
ctr.df <- st_centroid(world.shp)

# Get countries
countries <- ctr.df %>%
  rename(country_iso3 = eu) %>%
  pull(country_iso3)

# Get centroids
nodes.df <- as.data.frame(as(st_geometry(ctr.df$geometry), "Spatial")@coords) %>%
  cbind(countries) %>%
  rename(name = countries,
         x = coords.x1,
         y = coords.x2) %>%
  select(name, everything()) %>%
  arrange(name) %>%
  data.frame() 

# Prepare graph data
### ------------------------------------------------------------------------ ###
# Edges
edges.df <- visa.df %>%
  filter(destination_iso3 == "EU" & visa_requirement_binary == 1) %>%
  select(from = destination_iso3, to = nationality_iso3)

# Join world map with visa data
world.shp <- world.shp %>%
  left_join(y = visa.df %>%
              filter(destination_iso3 == "EU") %>%
              select(nationality_iso3, visa_requirement_binary), by = c("eu" = "nationality_iso3")) %>%
  mutate(visa_requirement = case_when(
    eu == "EU" ~ "EU",
    visa_requirement_binary == 0 | is.na(visa_requirement_binary) ~ "visa required",
    visa_requirement_binary == 1 ~ "visa not required",
    TRUE ~ NA_character_
  ))

# Graph object
graph.df <- graph_from_data_frame(d = edges.df, vertices = nodes.df, directed = TRUE)

# Plot graph and geom_sf
ggraph(graph = graph.df, layout = "manual", x = x, y = y) +
  geom_sf(data = world.shp$geometry,
          aes(fill = factor(world.shp$visa_requirement)), 
          show.legend = FALSE) +
  geom_edge_parallel(start_cap = circle(2, "mm"), 
                     end_cap = circle(0, "mm"),
                     arrow = arrow(type = "closed", 
                                   length = unit(1.5, "mm")),
                     sep = unit(5, "mm")) +
  scale_fill_manual(values = c("#f0f0f0", "#636363", "#cccccc")) +
  theme_graph()

# Export
### ------------------------------------------------------------------------ ###
ggsave(
  plot = visa_scatter.fig, "./figures/Fig 2 - VisaScatter.tiff", 
  width = 10, height = 6, unit = "in",
  dpi = 300
)
