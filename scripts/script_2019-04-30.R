library(harrypotter)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggimage)
library(cowplot)
library(ggtree)
library(yatah)

#### Data ####

bird_collisions <-
  read_csv("data/data_2019-04-30.csv") %>%
  mutate(species = paste(genus, species))
  
months <- 
  as_factor(c("January", "February", "March", "April", 
              "May", "June", "July", "August", "September", 
              "October", "November", "December")) 

font <- "Trattatello"


#### Tables and data ####

collision_per_month <-
  bird_collisions  %>%
  mutate(month = months[month(date)], 
         month = fct_drop(month),
         monthyear = floor_date(date, unit = "month")) %>% 
  count(species, month, monthyear) %>% 
  group_by(species, month) %>% 
  summarise(mean = mean(n)) %>% 
  ungroup()

most_collision_per_month <- 
  collision_per_month %>% 
  group_by(species) %>% 
  summarise(total = sum(mean)) %>% 
  top_n(n = 15)

ymax <- max(most_collision_per_month$total)

species_most <- most_collision_per_month$species

taxonomy <-
  bird_collisions %>% 
  select(family, genus, species) %>% 
  filter(species %in% species_most) %>% 
  taxtree(root = "Passerine")


#### Plots ####

# Insets

ggcol_inset <- function(df) {
  s <- round(sum(df$mean))
  ggplot(df) +
    aes(x = 0, y = mean, fill = month) +
    geom_col(position = position_stack(reverse = T)) +
    geom_text(x = 0, y = s, label = s, family = font, hjust = -0.2) +
    scale_fill_hp(discrete = TRUE, option = "HarryPotter", 
                  name = "", direction = -1, drop = FALSE) +
    ylim(c(0, ymax + 1)) +
    coord_flip() +
    theme_inset()
}

splitlist <-
  collision_per_month %>% 
  filter(species %in% species_most) %>% 
  group_by(species) %>% 
  group_split() %>% 
  set_names(species_most) %>% 
  `[`(taxonomy$tip.label) %>% 
  set_names(seq_along(species_most))

inset_cols <- map(splitlist, ggcol_inset)


# Legend

p_legend <-
  splitlist[[1]] %>% 
  ggplot() +
  aes(x = 0, y = mean, fill = month) +
  geom_col(position = position_stack(reverse = T)) +
  scale_fill_hp(discrete = TRUE, option = "HarryPotter", 
                name = "", direction = -1, drop = FALSE) +
  theme_wsj() +
  theme(legend.text = element_text(family = font, size = 15), 
        legend.direction = "vertical", legend.box = "vertical") 

legend <- get_legend(p_legend)


# Taxonomy

ptree <-
  ggtree(taxonomy, alpha = 0.8, color = "grey") +
  geom_nodelab(geom = "label", family = font, size = 4) +
  geom_tiplab(hjust = 1, vjust = -0.6, family = font, size = 5)


# Final plot

inset(ptree, inset_cols, width = 1, height = 1, hjust = -0.46) + 
  geom_subview(x = 1.75, y = 4.5, subview = legend) +
  xlim(NA, 1.88) +
  labs(title = "\n\nNumber of bird collisions per month in Chicago\n",
       caption = "Source: Winger et al, 2019\n@_abichat for #TidyTuesday") +
  theme_wsj() +
  theme(plot.title = element_text(family = font, hjust = 0.5, 
                                  face = "bold", size = 25,
                                  lineheight = 0.1), 
        plot.caption = element_text(family = font, size = 15), 
        panel.grid = element_blank(), axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank())

ggsave("plots/plot_2019-04-30.png", width = 29, height = 21, units = "cm", dpi = "retina")




