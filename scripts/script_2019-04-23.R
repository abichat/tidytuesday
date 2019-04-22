library(tidyverse)
library(lubridate)
library(ggpomological)

#### Data ####

anime <- read_csv("data/data_2019-04-23.csv")


#### Tables ####

df_movies <-
  anime %>%
  filter(type == "Movie") %>% 
  select(title_english, start_date, score, scored_by, source) %>% 
  filter(scored_by > 10000) %>% 
  drop_na() %>% 
  unique() %>% 
  mutate(year = year(start_date)) %>% 
  mutate(ntop = dense_rank(desc(score)), 
         nbot = dense_rank(score)) %>% 
  arrange(ntop) %>% 
  filter(ntop %in% 1:10 | nbot %in% 1:10) %>% 
  mutate(title_english = fct_reorder(title_english, score),
         source = fct_collapse(source, 
                               "Other" = "Other", 
                               "Other" = "Unknown", 
                               "Novel" = "Light novel"))

middle <- mean(range(df_movies$score))
min <- min(range(df_movies$score))
max <- max(range(df_movies$score))

seg_top <-
  seq(middle, max, length.out = 100) %>%
  tibble(x = ., xend = lag(x)) %>%
  drop_na() %>%
  mutate(alpha = 0.5 + (row_number() - 1) / (2 * n()))

df_seg_top <-
  df_movies %>%
  select(title_english, score) %>%
  filter(score > middle) %>%
  mutate(data = rerun(n(), seg_top)) %>%
  unnest(data) %>%
  filter(xend < score)

seg_bot <-
  seq(min, middle, length.out = 100) %>%
  tibble(xend = ., x = lead(xend)) %>%
  drop_na() %>%
  mutate(alpha = 1 - (row_number() - 1) / (2 * n()))

df_seg_bot <-
  df_movies %>%
  select(title_english, score) %>%
  filter(score < middle) %>%
  mutate(data = rerun(n(), seg_bot)) %>%
  unnest(data) %>%
  filter(x > score)

df_seg <- bind_rows(df_seg_bot, df_seg_top)


#### Plot ####

ggplot(df_movies) +
  aes(x = score, y = title_english) +
  geom_segment(data = df_seg, aes(x = x, y = title_english, xend = xend, 
                                  yend = title_english, alpha = alpha),
               size = 1) +
  geom_segment(data = df_seg, aes(x = x, y = title_english, xend = xend, 
                                  yend = title_english, alpha = alpha ), 
               size = 1) +
  geom_point(aes(fill = source, shape = source), size = 3.5) +
  geom_label(aes(x = middle, label = title_english, fill = source), 
             color = "white", show.legend = FALSE, 
             size = 4, family = "Luminari") +
  labs(x = "Score (out of 10)", y = NULL, 
       fill = "Type", shape = "Type", 
       title = "Best and worst anime movies", 
       caption = "Source: MyAnimeList\n@_abichat for #TidyTuesday") +
  theme_pomological_plain() +
  guides(alpha = FALSE) +
  scale_shape_manual(values = 21:25) +
  scale_fill_pomological() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 15, family = "Luminari"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

ggsave("plots/plot_2019-04-23.png", width = 29, height = 21, units = "cm", dpi = "retina")

