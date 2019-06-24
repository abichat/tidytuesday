library(tidyverse)
library(lubridate)
library(gganimate)
library(lwgeom)
library(sf)

#### Data ####

ufo_sightings <-
  read_csv("data/data_2019-06-25.csv", col_types = "cccccdcccdd") %>%
  mutate(date_time = mdy_hm(date_time),
         date = as_date(date_time),
         year = year(date))


#### Maps ####

world <- st_as_sf(rworldmap::getMap(resolution = "low"))
graticule <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)
outline <-
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"
world_wintri <- st_transform_proj(world, crs = crs_wintri)
graticule_wintri <- st_transform_proj(graticule, crs = crs_wintri)
outline_wintri <- st_transform_proj(outline, crs = crs_wintri)


#### Tables ####

df_ufo <-
  ufo_sightings %>%
  filter(year >= 1980)

coord_wintri <-
  df_ufo %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  lwgeom::st_transform_proj(crs = crs_wintri) %>%
  `$`(geometry) %>%
  map(as.matrix) %>%
  reduce(rbind)

df_ufo$longitude_wintri <- coord_wintri[, 1]
df_ufo$latitude_wintri <- coord_wintri[, 2]
  

#### Plot ####

anim <-
  df_ufo %>% 
  ggplot() + 
  geom_sf(data = outline_wintri, fill = "black", color = NA) + 
  geom_sf(data = graticule_wintri, color = "gray30", size = 0.25/.pt) +
  geom_sf(data = world_wintri, fill = "forestgreen", color = NA, size = 0.5/.pt) + 
  geom_sf(data = outline_wintri, fill = NA, color = "grey30", size = 0.5/.pt) + 
  geom_point(aes(x = longitude_wintri, y = latitude_wintri, alpha = encounter_length), 
             color = "red") + # points
  coord_sf(datum = NA, expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Reported UFO in {current_frame}", 
       caption = "Source: National UFO Reporting Center \n@_abichat for #TidyTuesday") +
  theme_void() +
  theme(plot.margin = margin(6, 1.5, 3, 1.5), legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        plot.caption = element_text(size = 12)) +
  transition_manual(year) +
  enter_appear() +
  exit_shrink()

animate(anim, width = 29, height = 19, units = "cm", res = 320, nframes = 170)

anim_save("plots/plot_2019-06-25.gif")




        