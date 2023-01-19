# Imports -------

library(tidyverse)
library(lubridate)
library(tidycensus)

library(tigris)
library(sf)
library(h3jsr)

library(leaflet)
library(htmltools)

options(digits=10) 


# visualize boundaries ------

show.poly <- function(df){
  map <- leaflet(df) %>%
    addProviderTiles('CartoDB.Positron')  %>%
    addPolygons(weight = 2, color='orange',
                label = ~ pri_neigh)
  
  map 
}


# Map neighborhood names for each area -------- 
get.chi.matches <- function(cook.area.centroid, chi.nbh, id_cols){
  st_join(cook.area.centroid, chi.nbh, join=st_within, left=T) %>% 
    dplyr::select(id_cols, NAME_NEIGH=pri_neigh, NAME_NEIGH_BACKUP=sec_neigh) %>% 
    st_drop_geometry() %>% as_tibble()
}

get.cook.matches <- function(cook.area.centroid, cook.nbh, id_cols){
  st_join(cook.area.centroid, cook.nbh, join=st_within, left=T) %>% 
    dplyr::select(id_cols, NAME_TOWN=township_name) %>% 
    st_drop_geometry() %>% as_tibble()
}


get.neighborhoods <- function(cook.area, chi.nbh, cook.nbh, id_cols = c('GEOID')){
  # 1. find centroid of area
  cook.area.centroid <- st_centroid(cook.area)
  
  # 2. find which neighborhood that centroid falls into
  chi.area.matches <- get.chi.matches(cook.area.centroid, chi.nbh, id_cols)
  cook.area.matches <- get.cook.matches(cook.area.centroid, cook.nbh, id_cols)
  
  area.matches <- full_join(chi.area.matches, cook.area.matches, by=id_cols) %>%
    mutate(DISPLAY_NAME = coalesce(NAME_NEIGH, NAME_TOWN),
           IS_CITY = !is.na(NAME_NEIGH)
           ) %>%
    dplyr::select(-NAME_NEIGH, -NAME_NEIGH_BACKUP, -NAME_TOWN)
  
  area.matches
}

# leaflet ----------map-

addMyResetMapButton <- function(map) {
  map %>%
    addEasyButton(
      easyButton(
        icon = "ion-arrow-shrink", 
        title = "Reset View", 
        onClick = JS(
          "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
        )
      )
    ) %>% 
    htmlwidgets::onRender(
      JS(
        "
function(el, x){ 
  var map = this; 
  map.whenReady(function(){
    map._initialCenter = map.getCenter(); 
    map._initialZoom = map.getZoom();
  });
}"
      )
    )
}


