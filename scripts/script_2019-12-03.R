library(tidyverse)
library(lubridate)

#### Data ####

tickets <- read_csv("data/data_2019-12-03.csv")


#### Tables ####

count_total <-
  tickets %>%
  select(issue_datetime) %>%
  mutate(weekday = wday(issue_datetime, label = TRUE, 
                        abbr = FALSE, week_start = 1),
         time_rounded = floor_date(issue_datetime, unit = "1hour"),
         week = week(time_rounded),
         hour = hour(time_rounded),
         minutes = minute(time_rounded),
         hour_dec = hour + minutes / 60) %>%
  count(week, weekday, hour_dec) %>%
  group_by(weekday, hour_dec) %>%
  summarise(n = mean(n)) %>%
  ungroup()

count_total_midnight <-
  filter(count_total, hour_dec == 0) %>%
  mutate(hour_dec = 24,
         weekday = fct_c(weekday[n()], weekday[1:(n() - 1)])) 

count_total <-
  count_total %>%
  rbind(count_total_midnight) %>%
  mutate(weekday_num = as.numeric(weekday) - 1,
         hour_week = hour_dec + weekday_num * 24)


#### Plot ####

count_total %>% 
  filter(weekday_num %% 2 == 0) %>%
  ggplot() +
  aes(x = hour_week, y = n, color = weekday, 
      fill = weekday, group = weekday) +
  geom_line() +
  geom_point(size = 1) +
  geom_area(alpha = 0.4, color = NA) +
  geom_line(data =  filter(count_total, weekday_num %% 2 == 1)) +
  geom_point(data = filter(count_total, weekday_num %% 2 == 1), size = 1) +
  geom_area(data =  filter(count_total, weekday_num %% 2 == 1), alpha = 0.4, color = NA) +
  geom_text(label = "Monday",    x = 14 + 0 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Tuesday",   x = 14 + 1 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Wednesday", x = 14 + 2 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Thursday",  x = 14 + 3 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Friday",    x = 14 + 4 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Saturday",  x = 14 + 5 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Sunday",    x = 14 + 6 * 24 , y = -10, color = "#564221", family = "Ink Free") +
  geom_text(label = "Mean number of parking violations\nper hour in Philadelphia",
            x = 142, y = 400, size = 7, angle = -30,
            color = "#564221", family = "Ink Free") +
  scale_x_continuous(breaks = 6 * 0:(4*7), 
                     labels = c(rep(c(0, 6, 12, 18), 7), 24),
                     minor_breaks = 2*0:(12*7)) +
  scale_color_viridis_d(drop = FALSE, option = "C", end = 0.9, direction = -1) +
  scale_fill_viridis_d(drop = FALSE , option = "C", end = 0.9, direction = -1) +
  labs(x = NULL, y = NULL, 
       caption = "Source: OpenDataPhilly\n@_abichat for #TidyTuesday") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#FAF7F2", color = "#FAF7F2"), 
        axis.text = element_text(family = "Ink Free", color = "#564221"), 
        plot.caption = element_text(family = "Ink Free", size = 12, color = "#564221"))
  
ggsave("plots/plot_2019-12-03.png", width = 29, height = 21, units = "cm", dpi = "retina")

