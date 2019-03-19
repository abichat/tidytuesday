library(tidyverse)
library(ggimage)
library(sf)

##### Data ####

combined_data <- read_csv("data/data_2019-03-19.csv", col_types = "cccdddddddd")


#### Function ####

plot_pie <- function(df) {
  ggplot(df) +
    aes(x = 0, y = stops_per_year, fill = driver_race) +
    geom_col(position = "fill", show.legend = FALSE, color = "grey30") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c(Black = "#3C2C2B", Hispanic = "#B27A58", 
                                 White = "#F8DED2")) +
    theme_void() +
    theme_transparent()
}


#### Get maps ####

sf_connecticut <-
  maps::map("county", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  separate(ID, into = c("state", "county"), sep = ",") %>% 
  filter(state == "connecticut")

mat_coord_cent <-
  sf_connecticut %>%
  st_centroid() %>%
  arrange(county) %>%
  st_coordinates()

sf_adjacent <-
  maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  filter(ID %in% c("connecticut", "massachusetts",
                   "new york", "rhode island"))


#### Tables ####

data_CT <-
  combined_data %>%
  filter(state == "CT") %>%
  select(location, driver_race, stops_per_year)

df_pie <-
  data_CT %>%
  arrange(location) %>%
  group_by(location) %>%
  nest() %>%
  mutate(total = map_dbl(data, ~ sum(pull(., stops_per_year))),
         total = total / 10 ^ 5 / 1.1,
         pie = map(data, plot_pie)) %>%
  mutate(x = mat_coord_cent[, 1],
         y = mat_coord_cent[, 2])


#### Plots ####

p_legend <-
  ggplot(df_pie$data[[1]]) +
  aes(x = 0, y = stops_per_year, fill = driver_race) +
  geom_col(position = "fill",color = "grey30") +
  scale_fill_manual(values = c(Black = "#3C2C2B", Hispanic = "#B27A58", 
                               White = "#F8DED2")) +
  guides(fill = guide_legend(title = "Driver race")) +
  theme(legend.background = element_rect(fill="#d2ecf8"))

leg <- cowplot::get_legend(p_legend)

df_pie <-
  df_pie %>% 
  add_row(total = 0.5, pie = list(leg), x = -71.75, y = 41.1)

p <-
  ggplot() +
  geom_sf(data = sf_adjacent, fill = "#f8f1d2") +
  geom_sf(data = sf_connecticut, fill = "#d2f8de") +
  coord_sf(xlim = c(-73.7, -71.75), ylim = c(41.05, 42.05)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#d2ecf8", color = "grey30"),
        panel.grid = element_line(colour = NA),
        plot.title = element_text(hjust = 0.5, face = "bold", 
                                  size = 20, lineheight = 0.1),
        plot.caption = element_text(size = 12)) +
  labs(title = "\nRepartition of annual traffic stops per race in Connecticut\n",
       caption = "Source: Stanford Open Policing Project\n@_abichat for #TidyTuesday\n")

p + geom_subview(data = df_pie, aes(x = x, y = y, subview = pie, 
                                    width = total, height = total))

ggsave("plots/plot_2019-03-19.png", width = 29, height = 21, units = "cm", dpi = "retina")

