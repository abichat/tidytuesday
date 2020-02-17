library(countrycode)
library(ggbeeswarm)
library(tidyverse)
library(gganimate)

#### Data ####

food_consumption <- read_csv("data/data_2020-02-18.csv", col_types = "ccdd")

olympic_colors <- c(Oceania = "#00A651", Africa = "#000000", Asia = "#FCB131", 
                    Europe = "#0081C8", Americas = "#EE334E")


#### Table ####

df <-
  food_consumption %>% 
  mutate(continent = countrycode(country, origin = "country.name", 
                                 destination = "continent"),
         food_category = str_to_lower(food_category),
         food_category = case_when(
           food_category == "wheat and wheat products" ~ "wheat",
           food_category == "milk - inc. cheese" ~ "milk & cheese",
           food_category == "nuts inc. peanut butter" ~ "nuts",
           TRUE ~ food_category),
         food_category = fct_reorder(food_category, consumption))


#### Plot ####

p <-
  ggplot(df) +
  aes(x = continent, y = consumption) +
  geom_boxplot(aes(fill = continent, color = continent), alpha = 0.7) +
  geom_quasirandom(aes(color = continent)) +
  scale_fill_manual(values = olympic_colors) +
  scale_color_manual(values = olympic_colors) +
  scale_y_continuous(sec.axis = dup_axis()) +
  labs(title = "Average consumption of {closest_state} per country", 
       x = NULL, y = "In kilogram per person per year",
       caption = "Source: nu3.de\n@_abichat for #TidyTuesday\n") +
  theme_bw() +
  theme(legend.position = "none", 
        text = element_text(size = 20, family = "EconSansCndReg"), 
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank(), 
        plot.margin = unit(c(10, 10, 10, 10), units = "points"),
        plot.background = element_rect(fill = "#f4f4e9"), 
        panel.background = element_rect(fill = "#f4f4e9"), 
        panel.grid.major = element_line(color = "#dbdbb6"),
        panel.grid.minor = element_blank()) 

a <-
  p +
  transition_states(food_category, state_length = 2) +
  view_zoom(step_length = 2, nsteps = 11)

animate(a, nframes = 300, width = 29, height = 21, units = "cm", res = 320)

anim_save("plots/plot_2020-02-18.gif")





