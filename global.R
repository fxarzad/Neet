library(shiny)
library(tidyverse)
library(shinythemes)
library(rgdal)
library(latticeExtra)
library(shinycssloaders)


neet_data <- read_csv("neet.csv")
neet_data <- neet_data %>%
  mutate(Standard_Territory = case_when(
    Territory == "Puglia" ~ "Apulia",
    Territory == "Sicilia" ~ "Sicily",
    Territory == "Trentino Alto Adige / Südtirol" ~ "Trentino-Alto Adige",
    Territory == "Valle d'Aosta / Vallée d'Aoste" ~ "Valle d'Aosta",
    TRUE ~ Territory
  ))

black_bg_theme <- standard.theme(color = FALSE) 
black_bg_theme$background$col <- '#2c3e50'
  

gadm_shapes <- readOGR("gadm36_ITA_shp/gadm36_ITA_1.shp")
colnames(neet_data) <- c("ITTER107", "Territory", "TIPO_DATO_FOL", "Data_type", "SEXISTAT1", "Gender", "ETA1", "Age_class", "TIME", "Select_time", "Value", "Flag_Codes", "Flags",'Standard_Territory')


neet_data <- neet_data %>%
  select(Standard_Territory, Gender, Age_class, Select_time, Value,Territory) %>%
  filter(!grepl("Q", Select_time)) %>% 
  mutate(Year = as.integer(Select_time)) %>%
  select(-Select_time)
gadm_sf <- gadm_shapes@data

gadm_shapes@data<- gadm_shapes@data %>%
  mutate(Standard_Territory = as.character(NAME_1))
int = intersect(neet_data$Standard_Territory,gadm_shapes@data$Standard_Territory)
neet_map_data = neet_data %>% 
  filter(Standard_Territory %in% int)
