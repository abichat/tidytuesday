library(tidyverse)
library(ggexpanse)
library(cowplot)

#### Data ####

nuclear_explosions <- 
  read_csv("data/data_2019-08-20.csv", col_types = "cddcccdddddddccc") %>% 
  mutate(country = fct_lump(country, n = 5, other_level = "Pakistan \n& India"),
         country = fct_infreq(country))


#### Tables ####

df_ts <- 
  nuclear_explosions %>% 
  group_by(year, country, .drop = FALSE) %>% 
  summarise(count = n()) 

df_count <- count(nuclear_explosions, country)


#### Plot ####

p_count <-
  ggplot(df_count) +
  aes(x = country, y = n, fill = country) +
  geom_col(color = NA) +
  coord_flip() +
  scale_fill_expanse() +
  labs(x = NULL, y = NULL,
       title = "Total per country") +
  theme_expanse(grid = "XY", 
                plot_title_size = 15,
                axis_text_size = 9) +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = alpha(expanse_cols$white, 1/2)),
        plot.margin = unit(c(10, 10, 10, 10), "points"))

p_year <-
  ggplot(df_ts) +
  aes(x = year, y = count, fill = country) +
  geom_area(color = NA) +
  scale_fill_expanse() +
  labs(x = NULL, y = "Count", fill = "Device deployed by",
       title = "Number of nuclear explosions per year and country",
       caption = "Source: SIPRI\n@_abichat for #TidyTuesday") +
  theme_expanse(grid = "XY", 
                plot_title_size = 22,
                caption_size = 11,
                axis_title_size = 10) +
  theme(legend.position = "none", plot.margin = unit(c(20, 20, 20, 20), "points"))

ggdraw(p_year) +
  draw_plot(p_count, .528, .47, .4, .4)

ggsave("plots/plot_2019-08-20.png", width = 29, height = 21, units = "cm", dpi = "retina")
