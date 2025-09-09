library(palmerpenguins)
library(tidyverse)

# 1. Rewrite the following code as a function: --------------------------------
x <- c(1, 2, 3, NA)

x / sum(x, na.rm = T)*100

percent <- function(x){
  x / sum(x, na.rm = T)*100
}

percent(x)

# 2. Use the function to calculate the percentage of each species in the penguins dataset: --------------------------------

glimpse(penguins)

penguins |> 
  group_by(species) |> 
  summarise(n = n()) |> 
  mutate(percent = percent(n))

# 3. Write a function that converts temperatures from Fahrenheit to Celsius: --------------------------------
f_to_c <- function(fahrenheit) {
  (fahrenheit - 32) * 5/9
}

f_to_c(c(0, 20, 68, 86, 100))

# 4. Write a function that calculates Shannon diversity index for a given vector of species abundances: --------------------------------
shannon_diversity <- function(abundances) {
  proportions <- abundances / sum(abundances, na.rm = T)
  -sum(proportions * log(proportions), na.rm = T)
}

spe |> 
  group_by(releve_nr) |> 
  summarise(shannon = shannon_diversity(cover_perc))

# 5. Pokémon defence power distribution --------------------------------

pokemon_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

glimpse(pokemon_df)

pokemon_df |> 
  filter(type_1 == 'water') |> 
  ggplot(aes(defense)) +
  geom_histogram(binwidth = 5, color = 'black') +
  theme_bw() +
  labs(title = 'Distribution of defence power of water-type Pokémon',
       x = 'Defence power',
       y = 'Count')

# function
plot_defense_dist <- function(data, pokemon_type) {
  data |> 
    filter(type_1 == pokemon_type) |> 
    ggplot(aes(defense)) +
    geom_histogram(binwidth = 5, color = 'black') +
    theme_bw() +
    labs(title = paste0('Distribution of defence power of ', pokemon_type, '-type Pokémon'),
         x = 'Defence power',
         y = 'Count')
}

plot_defense_dist(pokemon_df, 'fire')

# * generalize the function for any other numerical variable in the dataset
plot_dist <- function(data, pokemon_type, numeric_var, label) {
  data |> 
    filter(type_1 == pokemon_type) |> 
    ggplot(aes(.data[[numeric_var]])) +
    geom_histogram(binwidth = 5, color = 'black') +
    theme_bw() +
    labs(title = paste0('Distribution of ', numeric_var, ' of ', pokemon_type, '-type Pokémon'),
         x = label,
         y = 'Count')
}

plot_dist(pokemon_df, 'fire', 'speed', 'Speed')

# 6. Loop through multiple Pokémon types and create plots --------------------------------
pokemon_types <- unique(pokemon_df$type_1)

for (type in pokemon_types) {
  print(plot_dist(pokemon_df, type, 'defense', 'Defense'))
}

# * save the plots
for (type in pokemon_types) {
  p <- plot_dist(pokemon_df, type, 'defense', 'Defense')
  ggsave(filename = paste0('DataManipulationVisualisation/plots/defense_distribution_', type, '.png'), plot = p, width = 8, height = 6)
}

# do the same with purrr
poke_plots <- map(pokemon_types, ~plot_defense_dist(pokemon_df, .x))

walk2(poke_plots, pokemon_types, ~ggsave(filename = paste0('DataManipulationVisualisation/plots/defense_distribution_', .y, '.png'), plot = .x, width = 8, height = 6))

### data splitting for worked example
# write gapminder data split by year to separate csv files
data(gapminder, package = "gapminder")

gapminder |> 
  group_by(year) |> 
  group_walk(~write_csv(.x, paste0("DataManipulationVisualisation/data/gapminder/gapminder_", .y$year, ".csv")))

gapminder_nest <- gapminder_df |> 
  nest(data = -continent) |> 
  mutate(path = paste0('data/gapminder_continent/gapminder_', continent, '.csv')) 

walk2(gapminder_nest$data, gapminder_nest$path, ~write_csv(.x, paste0('data/gapminder_continent/gapminder_', .y, '.csv')))

paths <- list.files("data/gapminder", pattern = "[.]xlsx$", full.names = TRUE)
paths
#>  [1] "data/gapminder/1952.xlsx" "data/gapminder/1957.xlsx"
#>  [3] "data/gapminder/1962.xlsx" "data/gapminder/1967.xlsx"
#>  [5] "data/gapminder/1972.xlsx" "data/gapminder/1977.xlsx"
#>  [7] "data/gapminder/1982.xlsx" "data/gapminder/1987.xlsx"
#>  [9] "data/gapminder/1992.xlsx" "data/gapminder/1997.xlsx"
#> [11] "data/gapminder/2002.xlsx" "data/gapminder/2007.xlsx"