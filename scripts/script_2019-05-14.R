library(LaCroixColoR)
library(tidyverse)
library(ggthemes)
library(ggimage)

#### Data and ressources ####

nobel_winners <- read_csv("data/data_2019-05-14.csv",
                          col_types = "dccccdccDccccccDcc")

df_countrycode <-
  tribble(~country, ~code, 
          "United States of America", "US",
          "Germany", "DE",
          "United Kingdom", "GB",
          "France", "FR",
          "Japan", "JP",
          "Netherlands", "NL",
          "Sweden", "SE",
          "Russia", "RU",
          "Canada", "CA",
          "Austria", "AT")


#### Tables ####

countries <-
  nobel_winners %>%
  filter(category == "Chemistry") %>%
  count(birth_country, sort = TRUE) %>%
  head(n = 10) %>%
  pull(birth_country)

nobel_counts <-
  nobel_winners %>%
  filter(birth_country %in% countries, category == "Chemistry") %>%
  select(prize_year, birth_country) %>%
  arrange(prize_year) %>%
  group_by(birth_country) %>%
  mutate(n_prize = n(),
         first_prize = min(prize_year),
         last_prize = max(prize_year),
         cum = row_number()) %>%
  ungroup()

nobel_counts <-
  nobel_counts %>%
  filter(cum == 1) %>%
  mutate(cum = 0) %>%
  bind_rows(nobel_counts) %>%
  arrange(prize_year, cum) %>%
  mutate(birth_country = fct_reorder(birth_country, n_prize, .desc = TRUE))

first_last_nobel <-
  nobel_counts %>%
  select(birth_country, n_prize, first_prize, last_prize) %>%
  unique() %>%
  left_join(df_countrycode, by = c("birth_country" = "country"))


#### Plot ####

ggplot(nobel_counts) +
  aes(x = prize_year, y = cum, group = birth_country) +
  geom_line(aes(color = birth_country)) +
  geom_point(data = first_last_nobel, y = 0,
             aes(x = first_prize, y = n_prize, color = birth_country)) +
  geom_flag(data = first_last_nobel, size = 0.03, asp=2,
            aes(x = last_prize, y = n_prize, image = code)) +
  scale_color_manual(values = lacroix_palette("PeachPear", n = 10, type = "continuous")) +
  scale_y_continuous(limits = c(NA, 60)) +
  labs(title = "Number of chemistry Nobel prizes by birth country", color = NULL,
       caption = "Source: The Nobel Prize\n@_abichat for #TidyTuesday") +
  theme_wsj(color = "gray") +
  theme(legend.position = "bottom", 
        plot.caption = element_text(size = 12), 
        plot.title = element_text(size = 25),
        legend.text = element_text(family = "mono"))

ggsave("plots/plot_2019-05-14.png", width = 29, height = 21, units = "cm", dpi = "retina")


