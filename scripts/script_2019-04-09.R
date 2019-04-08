library(tidyverse)
library(gganimate)

#### Data ####

grand_slam_timeline <- read_csv("data/data_2019-04-09.csv", col_types = "cdccc")


#### Tables ####

ordered_outcomes <-c("Absent", "Lost Qualifier", "Qualification Stage 1", 
                     "Qualification Stage 2", "1st Round", "2nd Round", 
                     "3rd Round", "4th Round", "Quarterfinalist", 
                     "Semi-finalist", "Finalist", "Won")

df_frenchOpen <-
  grand_slam_timeline %>%
  mutate(outcome = as_factor(outcome)) %>%
  mutate(player = str_remove_all(player, "^/* ")) %>%
  filter(tournament == "French Open") %>%
  group_by(player) %>%
  mutate(
    winner = "Won" %in% outcome,
    begining = min(year),
    median = median(year)
  ) %>%
  ungroup() %>%
  filter(winner == TRUE) %>%
  mutate(
    outcome = fct_explicit_na(outcome, "Absent"),
    outcome = fct_collapse(outcome, Absent = c("Retired")),
    outcome = fct_relevel(outcome, ordered_outcomes)
  )

ordered_players <-
  df_frenchOpen %>% 
  arrange(median, begining) %>% 
  pull(player) %>% 
  unique()

df_frenchOpen <- mutate(df_frenchOpen,
                        player = factor(player, level = ordered_players))


#### Plot ####

p <-
  df_frenchOpen %>%
  ggplot() +
  aes(x = year, y = outcome, group = player) +
  geom_line(aes(color = gender), show.legend = FALSE) +
  geom_point() +
  labs(x = "Year", y = "Outcome",
       title = "Outcomes at Roland-Garros for {closest_state}",
       caption = "Source: Wikipedia\n@_abichat for #TidyTuesday") +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 9, 5.5, 5.5),
        plot.title = element_text(face = "bold"))

anim <-
  p +
  transition_states(player) +
  enter_fade() +
  exit_fade()

animate(anim, nframes = 12 * length(ordered_players))

anim_save("plots/plot_2019-04-09.gif")
