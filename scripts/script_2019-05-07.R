library(tidyverse)
library(cowplot)
library(scales)

#### Data ####

student_ratio <- read_csv("data/data_2019-05-07.csv", col_types = "ccccddcc")

oecd <-
  tribble(~country, ~shortname,
          "Australia", "Australia",
          "Austria", "Austria",
          "Belgium", "Belgium",
          "Canada", "Canada",
          "Chile", "Chile",
          "Czechia", "Czechia",
          "Denmark", "Denmark",
          "Estonia", "Estonia",
          "Finland", "Finland",
          "France", "France", 
          "Germany", "Germany",
          "Greece", "Greece",
          "Hungary", "Hungary",
          "Iceland", "Iceland",
          "Ireland", "Ireland", 
          "Israel", "Israel",
          "Italy", "Italy",
          "Japan", "Japan",
          "Republic of Korea", "South Korea",
          "Latvia", "Latvia", 
          "Lithuania", "Lithuania", 
          "Luxembourg", "Luxembourg",
          "Mexico", "Mexico", 
          "Netherlands", "Netherlands",
          "New Zealand", "New Zealand",
          "Norway", "Norway", 
          "Poland", "Poland", 
          "Portugal", "Portugal", 
          "Slovakia", "Slovakia", 
          "Slovenia", "Slovenia", 
          "Spain", "Spain", 
          "Sweden", "Sweden", 
          "Switzerland", "Switzerland",
          "Turkey", "Turkey",
          "United Kingdom of Great Britain and Northern Ireland", "UK",
          "United States of America", "USA")


#### Table ####

df_oecd <-
  student_ratio %>%
  inner_join(oecd, by = "country") %>%
  filter(indicator == "Primary Education") %>%
  mutate(shortname = fct_reorder(shortname, student_ratio, .desc = TRUE))


#### Plot ####

p_ocde <-
  ggplot(df_oecd) +
  aes(x = student_ratio, y = shortname) +
  geom_point(color = "grey90", alpha = 0.7, size = 3) +
  scale_x_continuous(breaks = pretty_breaks()) +
  annotate("text", x = 21, y = 25, size = 8, family = "MV Boli", color = "grey90", 
           label = "Average number of students per professor\nin primary school for OECD members") +
  labs(x = "Ratio", y = NULL,
       caption = "Source: UNESCO Institute of Statistics\n@_abichat for #TidyTuesday") +
  theme(panel.grid.major = element_line(color = "grey80", size = 0.05), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        text = element_text(color = "grey90", family = "MV Boli"), 
        plot.caption = element_text(size = 13), 
        axis.text = element_text(color = "grey90", family = "MV Boli"), 
        axis.text.y = element_text(size = 13))

ggdraw() +
  draw_image("ressources/img_2019-05-07.jpg", scale = 1.5) + 
  draw_plot(p_ocde)

ggsave("plots/plot_2019-05-07.png", width = 29, height = 21, units = "cm", dpi = "retina")

