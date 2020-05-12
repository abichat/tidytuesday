library(tidyverse)
library(rworldmap)
library(ggtext)
library(sf)

#### Data ####

volcano <- read_csv("data/data_2020-05-12.csv")

world <- st_as_sf(getMap(resolution = "low"))


#### Tables and maps ####

volcano_sf <-
  volcano %>% 
  select(volcano_number, volcano_name, last_eruption_year, latitude, longitude, country) %>% 
  mutate(last_eruption_year = if_else(last_eruption_year == "Unknown", 
                                      NA_character_, last_eruption_year),
         last_eruption_year = as.numeric(last_eruption_year)) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>%
  st_transform("+proj=eqearth +wktext") 

world_proj <-
  world %>%
  st_as_sf() %>%
  st_transform("+proj=eqearth +wktext") 

world_hex <-
  world_proj %>% 
  st_make_grid(what = "polygons", square = FALSE,
               n = c(400, 200)) %>% # soooo long
  st_sf() %>% 
  mutate(id_hex = 1:n()) %>% 
  select(id_hex, geometry)

df_hex <-
  volcano_sf %>% 
  st_join(world_hex) %>% 
  group_by(id_hex) %>% 
  nest() %>% 
  mutate(last_eruption_year = map(data, pull, last_eruption_year),
         last_eruption_year = map_dbl(last_eruption_year, max, na.rm = TRUE),
         recent_eruption = last_eruption_year > 0) %>% 
  drop_na(id_hex) %>% 
  select(id_hex, recent_eruption)

world_point <-
  world_hex %>% 
  left_join(df_hex, by = "id_hex") %>% 
  mutate(volcano = case_when(is.na(recent_eruption) ~ "no",
                             recent_eruption ~ "recent",
                             TRUE ~ "old")) %>% 
  st_centroid() %>% 
  st_transform("+proj=eqearth +wktext +lon_0=-105")


#### Plots ####

font <- "Volcanic Dungeon"

ggplot(world_point) +
  geom_sf(aes(color = volcano), size = 0.01) +
  scale_color_manual(values = c("no" = "#4C453A", "recent" = "#EC5E29", "old" = "#D6FC79")) +
  annotate(geom = "text", x = 17000000, y = -9500000, family = font, size = 3,
           color = "#E3E2CE", hjust = 1.06, lineheight = 1.7,
           label = "Source: The Smithsonian Institution\n@_abichat for #TidyTuesday ") +
  labs(title = "Volcanoes around the world", 
       subtitle = "<span style='color:#EC5E29'>Active</span> or <span style='color:#D6FC79'>not</span> during this era") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_markdown(color = "#E3E2CE", hjust = 0.5, size = 20, family = font),
        plot.subtitle = element_markdown(color = "#E3E2CE", hjust = 0.5, size = 12, family = font),
        panel.grid = element_blank(), 
        plot.margin = margin(10, -25, 5, -25),
        plot.background = element_rect(fill = "black"))

ggsave("plots/plot_2020-05-12.png", width = 29, height = 18.98, units = "cm", dpi = "retina")
