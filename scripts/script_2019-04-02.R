library(tidyverse)
library(lubridate)

#### Data ####

bike_traffic <- read_csv("data/data_2019-04-02.csv", col_types = "cccdd")


#### Functions ####

my_max <- partial(max, na.rm = TRUE)
my_mean <- partial(mean, na.rm = TRUE)


#### Tables ####

traffic <-
  bike_traffic %>%
  mutate(date = mdy_hms(date)) %>%
  filter(crossing != "Sealth Trail") %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    week = week(date),
    weekday = weekdays(date),
    crossing = fct_reorder(crossing, bike_count, my_max, .desc = TRUE)
  )

traffic_per_month <-
  traffic %>%
  group_by(year, month, crossing) %>%
  summarise(day = mean(date), total_bike = my_mean(bike_count))

traffic_per_week <-
  traffic %>%
  group_by(year, month, week, crossing) %>%
  summarise(day = mean(date), total_bike = my_mean(bike_count))

traffic_per_day <-
  traffic %>%
  group_by(year, month, week, weekday, crossing) %>%
  summarise(day = mean(date), total_bike = my_mean(bike_count))


#### Plot ####

ggplot(traffic_per_day) +
  aes(x = day, y = total_bike, group = crossing) +
  geom_line(color = "#a57259", alpha = 0.3) +
  geom_line(data = traffic_per_week, color = "#a57259") +
  geom_line(data = traffic_per_month, color = "#73503e") +
  scale_y_continuous(limits = c(NA, 80)) +
  facet_wrap(~ crossing) +
  labs(x = "Date", y = "Average number of bikes per hour",
       title = "Use of bike lanes in Seattle", 
       subtitle = "Smoothed per day, week and month",
       caption = "Source: Seattle Department of Transportation\n@_abichat for #TidyTuesday") +
  ggthemes::theme_economist()

ggsave("plots/plot_2019-04-02.png", width = 29, height = 21, units = "cm", dpi = "retina")
