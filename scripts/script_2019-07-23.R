library(wesanderson)
library(tidyverse)
library(cowplot)
library(waffle)
library(scales)

#### Data ####

wildlife_impacts <- 
  read_csv("data/data_2019-07-23.csv", col_types = "Tcccccccccdddcdddcccd") %>% 
  mutate(phase_of_flt = str_to_title(phase_of_flt))

levels_phase_of_flt <- c("Parked", "Taxi", "Take-Off Run", "Climb", "En Route", 
                         "Descent", "Approach", "Landing Roll")

levels_damage <- c("None", "Minor", "Damaged", "Substantial", "Unknown")

color_damage <- c(wes_palette(name = "FantasticFox1")[c(3, 2, 1, 5)], "grey80")
names(color_damage) <- levels_damage


#### Tables ####

df_phase_damage_height <-
  wildlife_impacts %>%
  select(phase_of_flt, damage, height) %>%
  filter(phase_of_flt %in% levels_phase_of_flt) %>%
  mutate(phase_of_flt = factor(phase_of_flt, levels = levels_phase_of_flt)) %>%
  mutate(damage = fct_recode(damage, None = "N", Minor = "M", 
                             Damaged = "M?", Substantial = "S"),
    damage = fct_explicit_na(damage, "Unknown"),
    damage = fct_relevel(damage, levels_damage))

df_phase_damage_count <-
  df_phase_damage_height %>%
  count(phase_of_flt, damage) %>%
  mutate(n = ceiling(n / 10))

df_height <-
  df_phase_damage_height %>%
  group_by(phase_of_flt) %>%
  summarise(n = n(), height = median(height, na.rm = TRUE)) %>%
  mutate(y = ceiling(n / 100) + 1,
         height = paste(round(height, 0), "ft")) 


#### Plots ####

p <-
  df_phase_damage_count %>%
  ggplot() +
  geom_waffle(aes(fill = damage, values = n), flip = TRUE, color = "white", 
              n_rows = 10, radius = unit(1, "pt")) +
  geom_text(data = df_height, aes(y = y, label = height), 
            x = 5.5, nudge_y = 1.9, family = "Ink Free") +
  facet_wrap(~ phase_of_flt, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(labels = function(x) comma(x * 1000), expand = c(0, 1.7)) +
  scale_fill_manual(values = color_damage) +
  labs(x = NULL, y = NULL, fill = "Damage", 
       caption = "Source: FAA\n@_abichat for #TidyTuesday") +
  theme_minimal(base_family = "Ink Free") +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.57, 0.69),
    legend.justification = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.title.y.right = element_blank()
  )


ggdraw(p) +
  draw_label("Number of impacts between flights and wildlife since 1990 in the US",
             x = 0.4, y = 0.81, fontfamily = "Ink Free", size = 20) +
  draw_label("Each square represents 10 collisions, figures correspond to median collision height", 
             x = 0.35, y = 0.75, fontfamily = "Ink Free", size = 13) +
  draw_label("Count", x = 0.02, y = 0.15, fontfamily = "Ink Free", angle = 90, size = 13)


ggsave("plots/plot_2019-07-23.png", width = 29, height = 21, units = "cm", dpi = "retina")
  
