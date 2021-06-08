# Read data on visa requirements

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "fs", "countrycode", "sf", 
            "ggraph", "tidygraph", "igraph")

# Notes

# Load data on visa requirements
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/visa_2020.rds")

# Subset to countries that implemented the Schengen Agreement
### ------------------------------------------------------------------------ ###
# created in: GetSchengenMembership.R
schengen.df <- import("./data/SchengenMembership.rds")

# Merge Schengen member states
visa.df <- visa.df %>%
  mutate(destination_iso3 = if_else(destination_iso3 %in% schengen.df$iso3_state, 
                                    "EU", destination_iso3),
         nationality_iso3 = if_else(nationality_iso3 %in% schengen.df$iso3_state, 
                                    "EU", nationality_iso3)) %>%
  distinct(destination_iso3, nationality_iso3, .keep_all = TRUE) %>%
  filter(!(destination_iso3 == "EU" & nationality_iso3 == "EU")) 

# Compute in-/ and outdegree
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
ggplot(visa_degree.df, aes(x = outdegree, y = indegree)) +
  geom_point(color = "grey") +
  geom_point(visa_degree.df %>%
               filter(country_iso3 == "EU"), 
             mapping = aes(x = outdegree, y = indegree), color = "red",
             size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Visa Freedom (sent)", y = "Visa freedom (received)", title = "Visa Freedom, 2020", 
       caption = "Data: Visa Network Data (2020)") +
  theme_minimal()

# Plot network of EU visa agreements
### ------------------------------------------------------------------------ ###
# Load world shapefile
world.shp <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Aggregate EU
world.shp <- world.shp %>%
  filter(type %in% c("Sovereign country", "Country")) %>%
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

# 
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

# Graph object
graph.df <- graph_from_data_frame(d = edges.df, vertices = nodes.df, directed = TRUE)

# Plot graph and geom_sf
ggraph(graph = graph.df, layout = "manual", x = x, y = y) +
  geom_sf(data = world.shp$geometry) +
  geom_edge_parallel(start_cap = circle(2, "mm"), 
                     end_cap = circle(0, "mm"),
                     arrow = arrow(type = "closed", 
                                   length = unit(1.5, "mm")),
                     sep = unit(5, "mm")) +
  theme_graph()
