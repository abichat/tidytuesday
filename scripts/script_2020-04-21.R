library(tidyverse)
library(ggupset)
library(glue)

#### Data ####

gdpr_violations <- read_tsv("data/data_2020-04-21.tsv", col_types = "dccdccccccc")


#### Tables ####

articles_violated <-
  gdpr_violations %>%
  select(id, price, article_violated) %>%
  filter(str_detect(article_violated, "GDPR")) %>%
  mutate(article_violated_splitted = str_split(article_violated, pattern = "\\|"),
         major_article_violated_splitted = map(article_violated_splitted, 
                                               str_extract, "Art\\.? ?[0-9]+"),
         major_article_violated_splitted = map(major_article_violated_splitted, 
                                               ~ sort(unique(.))),
         major_article_violated_splitted = map(major_article_violated_splitted, 
                                               str_replace, "Art\\.? ?", "Art. ")) %>%
  select(-price, -article_violated, -article_violated_splitted)

counts <-
  articles_violated %>% 
  count(major_article_violated_splitted, sort = TRUE) %>% 
  group_by(happedone = n == 1) %>% 
  summarise(unique_composition = n(), total_by_composition = sum(n), 
            article = c(major_article_violated_splitted)) %>% 
  nest(article = article) %>%
  mutate(article = map(article, pull, article),
         article = map(article, unlist),
         article = map(article, unique))


#### Plot ####

family <- "Roboto Condensed"
txtcolor <- "#C7AB47"

p <- 
  ggplot(articles_violated) +
  aes(x = major_article_violated_splitted) +
  geom_text(label = glue("Overall, {nrow(articles_violated)} fines for violating specific
                         GDPR articles have been issued."), 
            x = 12, y = 25, size = 5, color = txtcolor, family = family, hjust = 0) +
  geom_bar(fill = "grey70", color = "black") +
  scale_y_continuous(breaks = c(10, 30, 50, 70)) +
  scale_x_upset(order_by = "freq", n_intersections = counts$unique_composition[1]) +
  labs(x = NULL, y = NULL, 
       title = "GDPR fines by articles violations compositions",
       subtitle = glue("Compositions that happened only one time are not \\
                        displayed ({counts$total_by_composition[2]} fines and \\
                        {length(setdiff(counts$article[[2]], counts$article[[1]]))} \\
                        other articles)."),
       caption = "Source: Privacy Affairs\n@_abichat for #TidyTuesday") +
  theme_minimal() +
  theme(title = element_text(color = txtcolor, size = 20, family = family), 
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size = 11),
        plot.title.position = "plot",
        axis.text.y = element_text(hjust = 0, color = txtcolor, size = 10, family = family),
        panel.grid = element_line(color = "#C6AE62"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(size = 0.2),
        panel.background = element_rect(fill = "grey20", color = "grey20"),
        plot.background = element_rect(fill = "grey10"), 
        plot.margin = margin(10, 10, 10, 10)) +
  theme_combmatrix(combmatrix.panel.point.color.fill = "black", 
                   combmatrix.panel.point.color.empty = "grey70",
                   combmatrix.panel.striped_background.color.one = "grey90",
                   combmatrix.panel.striped_background.color.two = "#C6BA95", 
                   combmatrix.label.height = unit(9, "cm"))

ggsave("plots/plot_2020-04-21.png", p, width = 29, height = 21, units = "cm", dpi = "retina")

