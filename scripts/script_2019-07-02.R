library(gameofthrones)
library(ggchicklet)
library(hrbrthemes)
library(tidyverse)
library(glue)

#### Data ####

media_franchises <- read_csv("data/data_2019-07-02.csv", col_types = "ccddccc")


#### Table ####

df_biggest <- 
  media_franchises %>%
  group_by(franchise, revenue_category, year_created) %>%
  summarise(revenue = sum(revenue)) %>%
  ungroup() %>%
  mutate(revenue_category = fct_reorder(revenue_category, 
                                        revenue, sum, .desc = TRUE),
         franchise = fct_lump(franchise, n = 15, w = revenue)) %>%
  filter(franchise != "Other", 
         revenue > 0.5) %>%
  mutate(franchise = str_remove_all(franchise, "^.*/ "), 
         franchise = str_remove_all(franchise, " &.*$"), 
         franchise = glue("{franchise} ({year_created})"),
         franchise = fct_reorder(franchise, revenue, sum))


#### Plot ####

ggplot(df_biggest) +
  aes(x = franchise, y = revenue, fill = revenue_category) +
  geom_chicklet(width = 0.75, color = NA, radius = grid::unit(4, "pt")) +
  scale_fill_got_d(option = "Daenerys") +
  coord_flip() +
  guides(fill = guide_legend(override.aes = list(size = 10))) +
  labs(x = NULL, y = "Revenue (B$)", fill = NULL, 
       title = "Highest Grossing Media Franchises", 
       caption = "Source: Wikipedia\n@_abichat for #TidyTuesday") +
  theme_ft_rc() +
  theme(legend.position = c(0.8, 0.3),
    plot.title = element_text(color = "#929299", hjust = 0.5, size = 25),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 15),
    plot.caption = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15))

ggsave("plots/plot_2019-07-02.png", width = 29, height = 21, units = "cm", dpi = "retina")
