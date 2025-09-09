library(readxl)
library(janitor)
library(tidyverse)


# forest herb-layer species richness --------------------------------------

# load data
forest_herb <- read_xlsx("DataManipulationVisualisation/data/forest-understory-diversity_ia/Axmanova-Forest-understory-diversity-analyses.xlsx")

# nefunguje, slo by pro csv
forest_herb <- read_xlsx("https://github.com/BotzoolDataAnalysis/BotzoolDataAnalysis.github.io/blob/5fb3c2af5256f06d42d828e0986697d554546344/DataManipulationVisualisation/data/forest_understory/Axmanova-Forest-understory-diversity-analyses.xlsx") |> 
  clean_names()

glimpse(forest_herb)

# species richness vs canopy cover
forest_herb |> 
  ggplot(aes(Canopy_E3, Herbs, color = ForestTypeName, fill = ForestTypeName)) + 
  geom_point(color = 'black', pch = 21) +
  geom_smooth(method = "lm") +
  theme_bw()+
  labs(x = "Canopy cover (%)", y = "Herb-layer species richness", 
       color = "Forest type", fill = 'Forest type')

# species richness in different forest types - boxplot
forest_herb |> 
  ggplot(aes(ForestTypeName, Herbs, fill = ForestTypeName)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Herb-layer species richness") + 
  theme(legend.position = 'none', axis.title.x = element_blank())

# species richness in different forest types - violin + jitter
forest_herb |> 
  ggplot(aes(ForestTypeName, Herbs, fill = ForestTypeName, color = ForestTypeName)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(color = 'black', pch = 21, width = 0.1)+
  theme_bw() +
  labs(y = "Herb-layer species richness") + 
  theme(legend.position = 'none', axis.title.x = element_blank())

# species richness in different forest types - half-violin + boxplot
forest_herb |> 
  ggplot(aes(ForestTypeName, Herbs, fill = ForestTypeName, color = ForestTypeName)) +
  see::geom_violinhalf(aes(y = Herbs), alpha = 0.4) +
  geom_boxplot(color = 'black', width = 0.1)+
  theme_bw() +
  labs(y = "Herb-layer species richness") + 
  theme(legend.position = 'none', axis.title.x = element_blank())

# feederwatch - left_join, filter, maps
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2023/2023-01-10/readme.md
# nps_usa - filtering, maps
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-10-08/readme.md

# bird collisions
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2019/2019-04-30

# numbats <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-03-07/numbats.csv')
# factor reordering, barplot
# glimpse(numbats)
# 
# 
# numbats |> 
#   filter(!is.na(eventDate)) |> 
#   ggplot(aes(x = wday)) +
#   geom_bar() +
#   theme_bw() +
#   labs(y = "Number of observations") + 
#   theme(axis.title.x = element_blank())

# squirrels by colour -----------------------------------------------------

squirrels <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-23/squirrel_data.csv') |> 
  clean_names()

glimpse(squirrels)

squirrels |> 
  filter(primary_fur_color %in% c('Gray', 'Cinnamon', 'Black')) |> 
  ggplot(aes(x = primary_fur_color, fill = primary_fur_color)) +
  geom_bar() +
  scale_fill_manual(values = c('Gray' = 'gray', 'Cinnamon' = 'sandybrown', 'Black' = 'black')) +
  theme_bw() +
  labs(y = "Number of observations", fill = 'Primary fur colour') + 
  theme(axis.title.x = element_blank())


# pokemons ----------------------------------------------------------------

# load data
pokemon_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

glimpse(pokemon_df)

# weight distribution - histogram
pokemon_df |> 
  ggplot(aes(weight)) +
  geom_histogram() +
  theme_bw() 

# weight distribution - density plot
pokemon_df |> 
  ggplot(aes(weight)) +
  geom_density() +
  theme_bw() 

# weight distribution - log10 scale
pokemon_df |> 
  ggplot(aes(weight)) +
  geom_histogram() +
  scale_x_log10() +
  theme_bw() 

# weight distribution - log10 scale, density plot
pokemon_df |> 
    ggplot(aes(weight)) +
    geom_density() +
    scale_x_log10() +
    theme_bw() 

# height vs weight of water-type pokemons
pokemon_df |> 
  filter(type_1 == 'water') |> 
  ggplot(aes(x = attack, y = special_attack)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Attack", y = "Special attack")

# height vs weight of water-type pokemons
pokemon_df |> 
  ggplot(aes(x = type_1, y = attack)) +
  geom_boxplot()+
  theme_bw() +
  labs(y = "Attack") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
  
# attack vs defense with faceting
pokemon_df |> 
  ggplot(aes(x = attack, y = defense, color = type_1, fill = type_1)) +
  geom_point(pch = 21, color = 'black') +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~type_1) +
  theme_bw() +
  labs(x = "Attack", y = "Defense", color = 'Type', fill = 'Type') 

