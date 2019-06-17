library(tidyverse)
library(ggthemes)
library(cowplot)

#### Data ####

bird_counts <- read_csv("data/data_2019-06-18.csv", col_types = "dccddd") 


#### Table ####

df_diff <-
  bird_counts %>% 
  select(species_latin, year, how_many_counted_by_hour) %>% 
  drop_na(how_many_counted_by_hour) %>% 
  filter(year %in% c(1957, 1967, 1977, 1987, 1997, 2007, 2017)) %>%
  mutate(year = paste0("Y", year)) %>% 
  spread(year, how_many_counted_by_hour) %>% 
  mutate(diff1707 = Y2017 - Y2007,
         diff0797 = Y2007 - Y1997,
         diff9787 = Y1997 - Y1987,
         diff8777 = Y1987 - Y1977,
         diff7767 = Y1977 - Y1967,
         diff6757 = Y1967 - Y1957) %>% 
  mutate(species_latin = fct_lump(species_latin, n = 20, w = Y2017)) %>%
  filter(species_latin != "Other") %>% 
  mutate(species_latin = fct_reorder(species_latin, desc(diff1707)))


#### Plots ####

p1707 <-
  ggplot(df_diff) +
  aes(x = diff1707, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff1707 > 0)) +
  geom_point(aes(color = diff1707 > 0, size = Y2017)) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "right") +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  labs(x = NULL, y = NULL,
       title = "Difference in number of observations per hour between 2017 and 2007",
       subtitle = "Area are proportional to the number of observations per hour during the current year (2017)",
       caption = "Source: Bird Studies Canada\n@_abichat for #TidyTuesday") +
  theme_fivethirtyeight() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0),
        title = element_text(size = 12), axis.text = element_text(size = 10))

p0797 <-
  ggplot(df_diff) +
  aes(x = diff0797, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff0797 > 0)) +
  geom_point(aes(color = diff0797 > 0, size = Y2007)) +
  labs(x = NULL, y = NULL,
       title = "Difference in number of observations per hour between 2007 and 1997",
       subtitle = "Area are proportional to the number of observations per hour during the current year (2007)") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "right") +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", 
        title = element_text(size = 8), axis.text = element_text(size = 6),
        plot.margin = margin(0.5, 0.1, 0.5, 0.5, "lines"))

p9787 <-
  ggplot(df_diff) +
  aes(x = diff9787, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff9787 > 0)) +
  geom_point(aes(color = diff9787 > 0, size = Y1997)) +
  labs(x = NULL, y = NULL,
       title = "Difference in number of observations per hour between 1997 and 1987") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "left") +
  expand_limits(y = c(NA, 21)) +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", 
        title = element_text(size = 4), axis.text = element_text(size = 4),
        plot.margin = margin(0.5, 0.5, 0.5, 0.1, "lines"))

p8777 <-
  ggplot(df_diff) +
  aes(x = diff8777, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff8777 > 0)) +
  geom_point(aes(color = diff8777 > 0, size = Y1987)) +
  labs(x = NULL, y = NULL,
       title = "Difference in number of observations per hour between 1987 and 1977") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "left") +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  scale_size_continuous(range = c(1, 4)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", title = element_text(size = 1.8), 
        axis.text.x = element_text(size = 2), axis.text.y  = element_blank(), 
        panel.grid.major = element_line(size = 0.1),
        plot.margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit = "pt"))

p7767 <-
  ggplot(df_diff) +
  aes(x = diff7767, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff7767 > 0)) +
  geom_point(aes(color = diff7767 > 0, size = Y1977)) +
  labs(x = NULL, y = NULL,
       title = "Difference in number of observations per hour between 1977 and 1967") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "left") +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  scale_size_continuous(range = c(1, 3)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", 
        title = element_text(size = 1), axis.text = element_blank(), 
        panel.grid.major = element_line(size = 0.05),
        plot.margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit = "pt"))

p6757 <-
  ggplot(df_diff) +
  aes(x = diff6757, y = species_latin) +
  geom_segment(aes(xend = 0, yend = species_latin, color = diff6757 > 0)) +
  geom_point(aes(color = diff6757 > 0, size = Y1967)) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = c("TRUE" = "#77AB43", "FALSE" = "#FF2700")) +
  scale_size_continuous(range = c(1, 2)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", axis.text = element_blank(), 
        panel.grid.major = element_line(size = 0.05),
        plot.margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit = "pt"))

ggdraw(p1707) +
  draw_plot(p0797, x = 0, y = 0.1, width = 0.609, height = 0.609) +
  draw_plot(p9787, x = 0, y = 0.2, width = 0.355, height = 0.355) +
  draw_plot(p8777, x = 0.15, y = 0.255, width = 0.13, height = 0.13) +
  draw_plot(p7767, x = 0.182, y = 0.262, width = 0.079, height = 0.079) +
  draw_plot(p6757, x = 0.192, y = 0.267, width = 0.04, height = 0.04) 

ggsave("plots/plot_2019-06-18.png", width = 29, height = 21, units = "cm", dpi = "retina")

