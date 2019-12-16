library(tidyverse)
library(geojsonio)
library(scales)
library(broom)
library(rgeos)

#### Data ####

dog_moves <- read_csv("data/data_2019-12-17.csv", col_types = "cdddl")
spdf <- geojson_read("ressources/us_states_hexgrid.geojson", what = "sp")


#### Tables ####

df_states <-
  dog_moves %>%
  filter(inUS) %>%
  select(id = location, total) %>%
  mutate(id = if_else(id == "Washington DC", "District of Columbia", id))

spdf@data <-
  spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <-
  spdf %>%
  tidy(region = "google_name") %>%
  left_join(df_states, by = "id") %>%
  rename(x = long, y = lat)

centers <-
  spdf %>%
  gCentroid(byid = TRUE) %>%
  data.frame(id = spdf@data$iso3166_2)

df_title <-
  data.frame(x = rep(mean(range(centers$x)), 2), y = c(54.5, 52.5),
    id = c("Snapshot of adoptable dogs", "from Petfinder.com in the USA"))


#### Plot ####

ggplot() +
  aes(x = x, y = y, label = id) +
  geom_polygon(data = spdf_fortified,
               aes(group = group, fill = total),
               color = "white") +
  geom_text(data = centers, family = "balonez fantasia br", size = 9) +
  geom_text(data = df_title, size = 12, family = "balonez fantasia br") +
  scale_fill_distiller(palette = "YlGn", direction = 1, 
                       trans = log10_trans()) +
  coord_map() +
  geom_text(x = -100, y = 50, label = "title") +
  labs(fill = "Count",
       caption = "Source: The Pudding     \n@_abichat for #TidyTuesday     \n") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#f1fef9", color = "#f1fef9"),
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(family = "Arial Rounded MT Bold", size = 14),
        legend.position = c(0.08, 0.5),
        legend.direction = "vertical", 
        legend.title = element_text(family = "Arial Rounded MT Bold", size = 15),
        legend.text = element_text(family = "Arial Rounded MT Bold", size = 12),
        legend.key.size = unit(1, "cm"))

ggsave("plots/plot_2019-12-17.png", width = 29, height = 18.55, units = "cm", dpi = "retina")
