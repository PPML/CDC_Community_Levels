library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(here)

options(tigris_use_cache = TRUE)

## Pull county shapefile

##This version works but doesn't shift PR, and "shift_geo" is getting deprecated ...
#counties <- tidycensus::get_acs(geography = "county", geometry = TRUE, shift_geo = TRUE, variables = "B19013_001")
census_api_key(key = "0f8b22bec934b904f4eb93a34f1928de53b60a1d", overwrite = TRUE)

##This is the recommended implementation but gives crs error
counties <- tidycensus::get_acs(geography = "county", geometry = TRUE, variables = "B19013_001") %>% 
  shift_geometry()

##Same situation for state shapefiles
##Pull state shapefile for outlines

##Works but obsolete, and no PR
#states <- tidycensus::get_acs(geography = "state", geometry = TRUE, shift_geo = TRUE, variables = "B19013_001")

#Recommended approach, gives CRS error
states <- tidycensus::get_acs(geography = "state", geometry = TRUE, variables = "B19013_001") %>% shift_geometry()

data2 <- read_csv(here("0 - Data", "24-mar-22-export.csv"))

data2$GEOID <- sprintf("%05d", data2$county_fips)

plot_data <- merge(counties, data2, by = "GEOID")

ggplot(data = plot_data, aes(fill = factor(newscheme))) +
  geom_sf(size = .05, color= "white") +
  geom_sf(data = states, size = .1, fill = NA, color = "#444444") +
  scale_fill_manual(values = c("#00CC99", "#FFFF99", "#FC8D59"),labels = c("<5/50", "5/50", "10/100")) +
  theme_void() +
  labs(fill = "Community Level", title = "Not The CDC Community Levels,\nMarch 24, 2022") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"), legend.position = "right", legend.key.width = unit(.5, "cm"))
