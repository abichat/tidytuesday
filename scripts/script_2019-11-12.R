library(ggchicklet)
library(fishualize)
library(tidyverse)
library(grid)

#### Data ####

cran_code <- read_csv("data/data_2019-11-12.csv", col_types = "dcdddcc")


#### Table ####

df_characters <-
  cran_code %>%
  pull(pkg_name) %>%
  unique() %>%
  str_split(pattern = "") %>%
  unlist() %>%
  table() %>% 
  enframe(name = "alphanum", value = "count") %>% 
  mutate(
    count = as.numeric(count),
    type = case_when(alphanum == "."   ~ "dot",
                     alphanum %in% 0:9 ~ "digits",
                     TRUE          ~ "letters"),
    family = case_when(
      alphanum %in% c("r", "R") ~ "R",
      type == "letters" & alphanum == str_to_lower(alphanum) ~ "lower",
      type == "letters" & alphanum == str_to_upper(alphanum) ~ "upper",
      TRUE ~ type),
    type = fct_relevel(type, "dot"),
    family = fct_relevel(family, "R"))


#### Plot ####

ggplot(df_characters) +
  aes(x = alphanum, y = count, fill = family) +
  geom_chicklet(radius = unit(2, "pt")) +
  geom_text(aes(label = alphanum), vjust = -0.2, family = "Akbar", size = 4) +
  geom_text(x = 42, y = 8600, family = "Akbar", size = 8,
            label = "Character counts in R packages names") +
  labs(caption = "Source: CRAN\n@_abichat for #TidyTuesday") +
  scale_fill_fish_d(option = "Scarus_quoyi") +
  scale_y_continuous(breaks = 1:4 * 2000, sec.axis = dup_axis()) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), 
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(family = "Akbar"), 
        plot.caption = element_text(family = "Akbar", vjust = 6, size = 10))

ggsave("plots/plot_2019-11-12.png", width = 29, height = 11.5, units = "cm", dpi = "retina")


 