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


# clean digits -----
prettify <- function(x, suffix="", ...){
  format(x, nsmall=0L, digits=0L, scientific=F) %>% 
    prettyNum(big.mark = ',') %>%
    add.suffix(suffix)
} 

prettify.rank <- function(x, suffix='th', ...){
  # format as int, paste 'th' as X'th', but change to 1'st', 2'nd', and 3'rd' for ints 1:3
  x2 <- data.frame(old = format(x, digits=0L)) %>% 
    mutate(digit = str_extract(as.character(old), "\\d$") %>% as.numeric,
           suffix = dplyr::case_when(digit == 0 ~ 'th',
                                     digit == 1 ~ "st",
                                     digit == 2 ~ 'nd',
                                     digit == 3 ~ 'rd',
                                     digit >= 4 ~ 'th',
                                     TRUE ~ '',
                                     ),
           new = paste0(x, suffix)
           ) 
  x2[is.na(x2$old), 'new'] <- NA_character_
  x2$new
}

prettify.pct <- function(x, suffix="%", ...){
  format(x*100, nsmall=0L, digits=0L, scientific=F, na.encode=F) %>% add.suffix(suffix)
  # include % iff not NA
} 

prettify.race <- function(x, lookup){
  lookup$lbl[match(x, lookup$cd)]
}

add.suffix <- function(x, suffix){
  x2 <- data.frame(old = x) %>% mutate(new = paste0(old, suffix))
  x2[is.na(x2$old), "new"] <- NA_character_
  x2[str_remove(x2$old, " ") == 'NA', 'new'] <- NA_character_
  x2$new
}

# visualize boundaries ------

show.poly <- function(df){
  leaflet(df) %>%
    addProviderTiles('CartoDB.Positron')  %>%
    addPolygons(weight = 2, color='orange',
                label = ~ pri_neigh)
}


# Map neighborhood names for each area -------- 
get.chi.matches <- function(cook.area.centroid, chi.nbh, id_cols){
  st_join(cook.area.centroid, chi.nbh, join=st_within, left=T) %>% 
    dplyr::select(all_of(id_cols), NAME_NEIGH=pri_neigh, NAME_NEIGH_BACKUP=sec_neigh) %>% 
    st_drop_geometry() %>% as_tibble()
}

get.cook.matches <- function(cook.area.centroid, cook.nbh, id_cols){
  st_join(cook.area.centroid, cook.nbh, join=st_within, left=T) %>% 
    dplyr::select(all_of(id_cols), NAME_TOWN=township_name) %>% 
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


addBoundaries <- function(map, chi.bounds=T, cook.bounds=F, neighborhoods=T){
  line.params <- list(weight = 1,  dashArray = '3', color = "#595959",  opacity = .8,  fill = F)
  layers <- character()
  
  if(chi.bounds) {
    map <- do.call(addGeoJSON, c(list(map=map, geojson=chi.bounds.geojson, group='Chicago Limits'), line.params))
    layers <- append(layers, 'Chicago Limits')
  }
  if(cook.bounds) {
    map <- do.call(addGeoJSON, c(list(map=map, geojson=cook.bounds.geojson, group='Cook County Limits'), line.params))
    layers <- append(layers, 'Cook County Limits')
  }
  if(neighborhoods) {
    map <- do.call(addPolygons, c(list(map=map, data=cook.nbh, group='Neighborhoods'), line.params)) %>% 
      hideGroup('Neighborhoods')
    layers <- append(layers, 'Neighborhoods')
  } 
  map %>% 
    {if(any(c(chi.bounds, cook.bounds, neighborhoods))) addLayersControl(., overlayGroups = unique(layers), position = "topleft") else .} # can change neighborhoods to sep layer
}


