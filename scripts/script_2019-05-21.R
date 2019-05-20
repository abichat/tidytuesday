library(countrycode)
library(treemapify)
library(tidyverse)
library(janitor)
library(glue)

#### Data and ressources ####

waste_vs_gdp <-
  read_csv("data/data_2019-05-21.csv", col_types = "ccdddd") %>%
  rename(WastePC = `Per capita plastic waste (kilograms per person per day)`,
         Population = `Total population (Gapminder)`)

olympic_colors <- c(Oceania = "#00A651", Africa = "#000000", Asia = "#FCB131", 
                    Europe = "#0081C8", Americas = "#EE334E")


#### Tables ####

df_waste <-
  waste_vs_gdp %>%
  select(Entity, Code, Year, WastePC, Population) %>%
  drop_na() %>%
  mutate(TotalWaste = WastePC * Population,
         Continent1 = countrycode(Entity, origin = "country.name", 
                                  destination = "continent", warn = FALSE),
         Continent2 = countrycode(Code, origin = "iso3c", 
                                  destination = "continent", warn = FALSE),
         Continent = coalesce(Continent1, Continent2),
         Continent1 = NULL, Continent2 = NULL) %>%
  filter(Year == 2010, !is.na(Continent)) %>%
  group_by(Continent) %>%
  mutate(Country_TTW = fct_lump(Entity, n = 9, w = TotalWaste)) %>%
  ungroup()

df_waste_sum <-
  df_waste %>%
  group_by(Continent, Country_TTW) %>%
  summarise(TotalWaste = sum(TotalWaste)) %>%
  mutate(rank = min_rank(TotalWaste)) %>%
  ungroup() %>%
  mutate(label = case_when(Country_TTW == "Other" ~ glue("Other {Continent}"),
                           Country_TTW == "Democratic Republic of Congo" ~ "DR Congo",
                           TRUE ~ Country_TTW),
         label = case_when(Continent == "Oceania" ~ glue('{label} - {round(TotalWaste/10^6, 1)} kT'),
                           TRUE ~ glue('{label}\n{round(TotalWaste/10^6, 1)} kT')))


#### Plot ####

ggplot(df_waste_sum) +
  aes(area = TotalWaste, subgroup = Continent) +
  geom_treemap(aes(fill = Continent, alpha = rank), size = 0.5, color = "white") +
  geom_treemap_text(aes(label = label), color = "white", 
                    family = "Basicdots", place = "topleft", grow = FALSE) +
  scale_fill_manual(values = olympic_colors) +
  scale_alpha(range = c(0.3, 1)) +
  labs(title = "Daily amount of plastic waste entering the ocean",
       caption = "Source: Our World in Data\n@_abichat for #TidyTuesday") +
  theme(legend.position = "none",
        title = element_text(family = "Andale Mono", hjust = 0.5, size = 20),
        plot.caption = element_text(family = "Andale Mono", size = 10))

ggsave("plots/plot_2019-05-21.png", width = 29, height = 21, units = "cm", dpi = "retina")

