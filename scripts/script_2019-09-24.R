library(ggeconodist)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(scales)

#### Data ####

school_diversity <- read_csv("data/data_2019-09-24.csv", col_types = "cccccdddddddcdc")


#### Tables ####

df_diversity <-
  school_diversity %>% 
  select(LEAID, ST, SCHOOL_YEAR, White) %>%
  group_by(LEAID) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  filter(N == 2) %>%
  group_by(ST, SCHOOL_YEAR) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  filter(N > 10) %>%
  ungroup() %>%
  select(-N) %>%
  mutate(ST = fct_reorder(ST, White, .desc = TRUE), 
         White = White / 100)


#### Plot ####

# Legend 

p_legend <-
  ggplot(df_diversity) +
  aes(x = SCHOOL_YEAR, y = White, fill = SCHOOL_YEAR) +
  geom_col(alpha = 0.2) +
  scale_fill_tableau() +
  theme_econodist() +
  labs(fill = "School Year") +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.title = element_text(family = "EconSansCndLig", color = "#3b454a", size = 10),
        legend.text = element_text(family = "EconSansCndLig", color = "#3b454a", size = 10))

legend <- get_legend(p_legend)

# Final plot 

p <-
  ggplot(df_diversity) +
  aes(x = ST, y = White, fill = SCHOOL_YEAR) +
  geom_econodist(tenth_col = "#b07aa1", ninetieth_col = "#591a4f",
                 show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_fill_tableau() +
  labs(x = NULL, y = NULL, 
       title = "Evolution of the Proportion of Whites in US Schools",
       caption = "Source: The Washington Post\n@_abichat for #TidyTuesday") +
  theme_econodist() +
  theme(title = element_text(size = 15))

ggdraw(p) +
  draw_plot(legend, x = -0.305, y = -0.465) + 
  draw_plot(econodist_legend_grob(tenth_col = "#b07aa1", ninetieth_col = "#591a4f"),
            x = 0.35, y = 0.027) 

ggsave("plots/plot_2019-09-24.png", width = 29, height = 21, units = "cm", dpi = "retina")

