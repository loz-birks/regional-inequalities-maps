# Read Libraries ----------------------------------------------------------
# not sure that all of these are required...

library(tidyverse)
library(gmapsdistance)
library(geojsonR)
library(sf)
library(here)
library(readxl)
library(PostcodesioR)
library(parallel)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

# Geographical Lookups ----------------------------------------------------
# change file paths as appropriate
# read lookups from shared GIS data folder
lsoalookup <- read.csv("backup files/Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2021)_Lookup_in_England.csv")
ccglookup <- read.csv("backup files/Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv")

# read boundaries and join to lookups
boundaries_lsoa <- st_read("backup files/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.geojson") # transform to long-lat
boundaries_lsoa <- boundaries_lsoa %>%
  # join lsoas to ccgs
  left_join(lsoalookup) %>%
  # join CCGs to regions
  left_join(ccglookup, by = c("CCG21CD" = "CCG21CD")) %>% 
  # filter to region of interest
  subset(NHSER21NM == "South East") 

# Thematic Layer ----------------------------------------------------------
# import adjusted IMD UK text file from shared data folder (is there a better place to get this?)
df_imd <- as_tibble(read.csv("backup files/File_1_-_IMD2019_Index_of_Multiple_Deprivation.csv", sep = ','))
df_imdquintile <- df_imd %>% 
  mutate(IMDquintile = ntile(as.numeric(IMD_Rank), 5),
         lsoa11cd = as.character(LSOA_code_2011)) %>% 
  select(-LSOA_code_2011)

# create colour palette
mycols <- colors()[c(107, 98, 415, 498, 624)]

# Create function to plot by region/STP -----------------------------------
# input geog is either an STP code or "Region"

map_fn <- function(geog){
  
  if (geog == "Region"){
    boundaries <- boundaries_lsoa
    title1 <- "Regional Level"
  }
  else {
    boundaries <- boundaries_lsoa %>%
      filter(STP21CDH == geog)
    title1 <- boundaries$STP21NM[1]
  }
  
  borders <- st_union(boundaries)
  
  map <- boundaries %>%
    # change imd labels
    left_join(df_imdquintile, by = c("LSOA11CD" = "lsoa11cd")) %>%
    ggplot() +
    # change imd labels
    geom_sf(aes(fill = as.factor(IMDquintile)), colour = NA) + # replace tt with variable to fill  colour = "lightgrey"
    # uncomment if you want a border around lsoas
    # geom_sf(
    #  data = boundaries_lsoa,
    #  colour = "darkgrey",
    #  fill = NA
    # ) +
    geom_sf(
      data = borders,
      fill = NA, colour = "black",
      size=1,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c("#750D37", "#F05365", "#CDDDDD", "#40798C", "#18314F")) +
    labs(
      title = title1,
      # change imd labels for titles
      subtitle = "LSOAs by Quintiles of Index of Multiple Deprivation (2019)",
      caption = "1 = Most Deprived, 5 = Least Deprived",
      fill = "IMD Quintile"
    ) +
    theme_void()
  
  return(map)
}

# South East Region
map_fn("Region")
# Kent and Medway
map_fn("QKS")
# Frimley
map_fn("QNQ")
# Sussex
map_fn("QNX")
# Hampshire and the Isle of Wight
map_fn("QRL")
# Buckinghamshire, Oxfordshire and Berkshire West
map_fn("QU9")
# Surrey
map_fn("QXU")

# Quickly draw a regional map with borders around ICB ---------------------
boundaries <- boundaries_lsoa
borders <- st_union(boundaries)

boundaries %>%
  # change imd labels
  left_join(df_imdquintile, by = c("LSOA11CD" = "lsoa11cd")) %>%
  ggplot() +
  # change imd labels
  geom_sf(aes(fill = as.factor(IMDquintile)), colour = NA) +
  geom_sf(
   data = boundaries_lsoa %>% filter(STP21CDH == "QKS") %>% st_union(),
   colour = "black",
   size = 1,
   fill = NA
  ) +
  geom_sf(
    data = boundaries_lsoa %>% filter(STP21CDH == "QNQ") %>% st_union(),
    colour = "black",
    size = 1,
    fill = NA
  ) +
  geom_sf(
    data = boundaries_lsoa %>% filter(STP21CDH == "QNX") %>% st_union(),
    colour = "black",
    size = 1,
    fill = NA
  ) +
  geom_sf(
    data = boundaries_lsoa %>% filter(STP21CDH == "QRL") %>% st_union(),
    colour = "black",
    size = 1,
    fill = NA
  ) +
  geom_sf(
    data = boundaries_lsoa %>% filter(STP21CDH == "QU9") %>% st_union(),
    colour = "black",
    size = 1,
    fill = NA
  ) +
  geom_sf(
    data = boundaries_lsoa %>% filter(STP21CDH == "QXU") %>% st_union(),
    colour = "black",
    size = 1,
    fill = NA
  ) +
  scale_fill_manual(values = c("#750D37", "#F05365", "#CDDDDD", "#40798C", "#18314F")) +
  labs(
    title = "South East IMD Map",
    subtitle = "LSOAs by Quintiles of Index of Multiple Deprivation (2019)",
    caption = "1 = Most Deprived, 5 = Least Deprived",
    fill = "IMD Quintile"
  ) +
  theme_void()


































