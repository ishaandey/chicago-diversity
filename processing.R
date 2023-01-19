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


# scale range of values between 0 to 1 -------
min.max <- function(x, floor=0, ceil=1){
  x.min = min(x, na.rm=T); x.max = max(x, na.rm=T)
  (ceil - floor) * (x - x.min) / (x.max - x.min)
}


# calculate proportions -------

get.proportions <- function(df.acs){
  df.acs %>% 
    # clean NA vs. NaNs
    mutate(pop.TOTE = na_if(pop.TOTE, 0)) %>% 
    # define proportions
    mutate(prp.TOT = pop.TOTE / pop.TOTE, 
           prp.WHT = pop.WHTE / pop.TOTE, 
           prp.BLK = pop.BLKE / pop.TOTE, 
           prp.HLT = pop.HLTE / pop.TOTE, 
           prp.NAM = pop.NAME / pop.TOTE, 
           prp.ASN = pop.ASNE / pop.TOTE, 
           prp.HPI = pop.HPIE / pop.TOTE, 
           prp.MUL = pop.MULE / pop.TOTE, 
           prp.OTH = pop.OTHE / pop.TOTE) 
}

# calculate diversity index -------
get.DI.score <- function(df.acs){
  df.acs %>% mutate(DI = 1-(prp.WHT^2 + prp.BLK^2 + prp.HLT^2 + prp.NAM^2 + prp.ASN^2 + prp.HPI^2 + prp.MUL^2 + prp.OTH^2),
                    DI.simple= 1-(prp.WHT^2 + prp.BLK^2 + prp.HLT^2 + prp.ASN^2 + (prp.MUL^2+prp.HPI+prp.NAM+prp.OTH)^2)) 
}



# get prevalnce scores ---- 
get.ranks <- function(df.acs, id_cols = c('GEOID', 'NAME')){
  df.acs.ranks <- df.acs %>% dplyr::select(id_cols, starts_with("prp.")) %>%
    pivot_longer(cols = starts_with("prp."),
                 names_prefix = "prp\\.",
                 names_to = "RACE",
                 values_to = "PRP"
    ) %>% filter(RACE != "TOT") %>%
    group_by(across(id_cols)) %>% 
    mutate(PRV = rank(desc(PRP), ties.method="first")) %>%
    ungroup() %>% arrange(across(id_cols), PRV)
  
  df.acs.ranks[is.na(df.acs.ranks$PRP), "RACE"] <- NA_character_
  
  df.acs.ranks
}


get.prevalence <- function(df.acs.ranks, id_cols = c('GEOID', 'NAME')){
  # get the top 3 groups per block
  df.acs.prv <- df.acs.ranks %>% 
    filter(PRV <= 3) %>%
    pivot_wider(id_cols=id_cols,
                names_from = PRV,
                names_prefix = c("PRV_TOP"),
                values_from = c(RACE, PRP),
    ) %>% arrange(across(id_cols))
  
  df.acs.prv
}

get.diffusion <- function(df.acs.ranks, id_cols = c('GEOID', 'NAME')){
  # calculate diffusion score as total of those not in the top 3
  df.acs.dif <- df.acs.ranks %>% filter(PRV > 3) %>%
    group_by(across(id_cols)) %>% 
    summarise(DIF = sum(PRP, na.rm=F)) %>% 
    ungroup() %>% arrange(across(id_cols)) 
  
  df.acs.dif
}


get.prv.dif.scores <- function(df.acs, id_cols = c('GEOID', 'NAME')){
  # put it all together
  df.acs.ranks <- get.ranks(df.acs, id_cols)
  df.acs.prv   <- get.prevalence(df.acs.ranks, id_cols)
  df.acs.dif   <- get.diffusion(df.acs.ranks, id_cols)
  
  df.acs %>% 
    left_join(df.acs.prv, by=id_cols) %>% 
    left_join(df.acs.dif, by=id_cols) 
}
