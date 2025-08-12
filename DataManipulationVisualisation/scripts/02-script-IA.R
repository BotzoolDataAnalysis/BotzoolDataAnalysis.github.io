library(tidyverse)
library(readxl)

# Tidy script -------------------------------------------------------------
###### Few useful tips how to handle your scripts tidy

### 1, use projects
# create directories data, results, maps, backup etc.

### 2, libraries
# list all the libraries you need in the beginning of the script

### 3, remarks
# ctrl + shift + C  # change the text to non-active / marked with hash tags
# add notes to your scripts, you will be grateful later


### 4, separate scripts or sections
#ctrl + shift + R   # insert named section
# use 4x#
# Alt + O           # fold all 
# Shift + Alt + O   # unfold all 


### 5, names of variables
# short and easy to handle, without spaces, strange symbols
# rename strange names one by one 
# %>% rename (ReleveNr = "Releve number")
# or change all difficult patterns at once
# RegEx Regular expression
# env <- env %>%
#   rename_all(~ str_replace_all(., c(
#     "\\." = "",     #remove dots in the variable names
#     "\\mÂ²" = "m2", 
#     "\\Â°" = "deg", 
#     "\\%" = "perc", # remove symbol % and change it to perc
#     "\\(" = "",     
#     "\\)" = "",
#     "\\/" = "",
#     "\\?" = "",
#     "\\s", "."))) #remove spaces

### 6, piping
# pipe binds individual steps into a sequence 
# ctrl+shift+M inserts a pipe %>% 


### extra reading
# https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
# https://davidzeleny.net/wiki/doku.php/recol:clean_and_tidy_script



# Tidy data ---------------------------------------------------------------
##### Tidy data
# Tidy data makes it easy for an analyst or a computer to extract needed
# variables because it provides a standard way of structuring a dataset.
# A dataset is messy or tidy depending on how rows, columns and tables are
# matched up with observations, variables and types. In tidy data:
# 1.	Every column is a variable.
# 2.	Every row is an observation.
# 3.	Every cell is a single value.

#extra reading
# https://r4ds.had.co.nz/tidy-data.html
# https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

#### to tidy up messy data / see examples


# Data handling with tidyverse -----------------------------------------------------------
# easy to handle 
# main functions: arrange, select, filter, mutate and summarise
# we will use data from the folder Forest-understory-diversity_IA
# "Forest-understory-diversity-analyses.xlsx"

#### import data, check structure ####
# Tidyverse: readr, readxl
# see cheatsheet
# cheating with R studio
# check the folder content

list.files() #all files
list.files("data") #content of data directory
list.files("data/forest-understory-diversity_iA")

# we will work with the forest data 
# headers with some variables based on species data already prepared
data <- read_excel("data/forest-understory-diversity_iA/Axmanova-Forest-understory-diversity-analyses.xlsx")

#few tips how to check the structure
tibble(data)
names(data)
glimpse(data)
table(data$ForestTypeName)

#### arrange ####
data %>% 
  arrange(ForestType)

data %>% 
  arrange(desc(Biomass))


#### select ####
data %>% 
  select(PlotID, ForestType, ForestTypeName) 

# challenge: find at least 3 different options of selecting 3 variables 

#### distinct ####
data %>%
  arrange(ForestType) %>%
  select(ForestType, ForestTypeName) %>%
  distinct() #keep only unique rows

data %>%      # same as above but shorter
  arrange(ForestType) %>%
  distinct(ForestType, ForestTypeName)


#### filter, slice ####
data %>% 
  filter(Biomass > 80) #[g/m2]

data %>% 
  filter(ForestTypeName %in% c("alluvial forest", "ravine forest"))

data %>% 
  filter(plotID %in% selected$plotID) #e.g. from different analysis, stratification
#tidyverse alternative is to use semi-join, see below

data %>% 
  filter(!is.na(Juveniles)) %>%
  slice_max(Juveniles, n = 3) 

#challenge find 5 plots with lowest biomass, but not from oak forests 


#### mutate ####
data %>% 
  mutate(SpeciesRichness = Herbs + Juveniles)
data %>% 
  mutate(selection = 1)

data %>%
  mutate(productivity = ifelse(Biomass<60,"low","high")) %>% 
  select (PlotID, ForestTypeName, productivity) %>%
  print(n=20)

data %>% 
  select(PlotID, Herbs, Biomass, pH_KCl) %>% 
  mutate(across(c(Herbs, Biomass, pH_KCl), round))



#### group_by, count ####
data %>% 
  group_by(ForestType) %>%
  count()

data %>% 
  group_by(ForestType, ForestTypeName) %>%
  count()

#challenge: create two groups of soil reaction (acidic, basic) and count plots 


#### summarise ####
data %>% 
  group_by(ForestTypeName) %>% 
  summarise (UnderstoreyRichness = mean(Herbs),
             Productivity = mean(Biomass))

# alternative / summarise across

data %>% 
  group_by(ForestType) %>% 
  summarise(across(c(Herbs, Biomass, pH_KCl), 
                   list(mean = mean, sd = sd)))
# %>% write_csv('output/statistics.csv')

# challenge> summarise - calculating trait means
#we will use species data and traits to calculate means per sample
# e.g. mean height of all the plants in the particular vegetation plot

traits <- read_excel("data/Forest-understory-diversity_IA/traits.xlsx")
names(traits)

spe <- read_excel("data/Forest-understory-diversity_IA/Axmanova-Forest-spe.xlsx")
names(spe)

#mean per Height and SeedMass
speSum<- spe %>% 
  left_join(traits, by = c("Species" = "Taxon"))%>%
  group_by(RELEVE_NR) %>%
  summarise(across(c(Height, SeedMass),list(mean = mean), na.rm = TRUE))

# mean per EIV values
speSum <- read_excel("data/Forest-understory-diversity_IA/Axmanova-Forest-spe.xlsx") %>% 
  left_join(traits, by = c("Species" = "Taxon"))%>%
  group_by(RELEVE_NR) %>%
  summarise(across(c(starts_with("EIV")),list(mean = mean), na.rm = TRUE))



#### join functions ####
### import new data from the same folder / spe and traits
traits <- read_excel("data/Forest-understory-diversity_IA/traits.xlsx")
names(traits)
# [1] "Taxon"         "Height"        "SeedMass"      "EIV_Light"     "EIV_Moisture" 
# [6] "EIV_Reaction"  "EIV_Nutrients"

spe <- read_excel("data/Forest-understory-diversity_IA/Axmanova-Forest-spe.xlsx")
names(spe)
# [1] "RELEVE_NR" "Species"   "Layer"     "CoverPerc"

# define matching names or rename in the original file
spe<- spe %>%
  left_join(traits, by = c("Species" = "Taxon"))
            
            
#challenge > use forest data, make two selections of the data and join them back

#challenge, prepare a subset based on Forest type, select different variables
# try full_join

### filtering join
# we prepare this to pretend that we have a selection from before
selected<- data %>%  
  filter(Biomass>80) %>% #more productive plots
  select(PlotID, ForestType, ForestTypeName)

data %>% semi_join(selected) 

rm(selected)

#### pivoting long<>wide, unite/separate ####
### unite/separate
spe %>% 
  unite("SpeciesLayer", Species,Layer, na.rm = TRUE, remove = FALSE)%>%
  separate (SpeciesLayer, into=c("SpeciesX", "LayerX"), sep='_')

spe %>% 
  unite("SpeciesLayer", Species,Layer, na.rm = TRUE, remove = FALSE)%>%
  separate_wider_delim(SpeciesLayer, "_", names = c("SpeciesX", "LayerX"))

#### pivoting long<>wide
#to wide
spe <- read_excel("data/Forest-understory-diversity_IA/Axmanova-Forest-spe.xlsx")
spe.wide<- spe %>% 
  mutate(Species=gsub(" ", "_",spe$Species, fixed=T))%>%
  unite("SpeciesLayer", Species,Layer, na.rm = TRUE, remove = FALSE)%>%
  select(-c(Species, Layer)) %>%
  pivot_wider(names_from = SpeciesLayer, values_from = CoverPerc,
              values_fill = 0)
#to long
spe.long<- spe.wide %>% 
  pivot_longer(cols = 2:370, 
               names_to ="speciesLayer",values_to = "cover", 
               values_drop_na = TRUE) %>%
  print()



# Exercises / tidy data --------------------------------------------------------------
####### import messy data from the folder Tidy-data-IA-KC and make them tidy
# Example 1,Missing headers ("PlotID", "Species","Layer","CoverPerc","CoverCode")
# Example 2, Multiple variables are stored in one column.
# Example 3, Variables are stored in both rows and columns.
# Example 4, Duplicate rows





