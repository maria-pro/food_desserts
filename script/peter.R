## load packages

library(sf)
library(absmapsdata)
library(tidyverse)
library(openxlsx)
library(OpenStreetMap)
library(tmap)
library(sf)
library(tidyverse)
library(openxlsx)
library(googleway)
library(absmapsdata)
library(OpenStreetMap)

## get the sa1 area you after

melbsa1 <- sa12021

melbsa1 <- melbsa1 %>% filter(gcc_code_2021 == "2GMEL")

View(melbsa1)

## filter by region if you want to refine further 

melbinner <- melbsa1 %>% filter(sa4_code_2021 == "206")

## transform so you can use metres in making the grid

melbinner <- st_transform(melbinner, crs = st_crs(4462))

melbinnergrid <- st_make_grid(melbinner, cellsize = 4000)[melbinner]

## get the centroids and then transform back to long lat and combine into a data frame

melbinnergridcentroid <- st_centroid( melbinnergrid)

melbinnergridcentroid <- st_transform(melbinnergridcentroid, crs = st_crs(4326))

melbinnergridcentroid  <- cbind(melbinnergridcentroid, st_coordinates(melbinnergridcentroid))

## run the 
## remember to have set up google maps API key
## note that unlist is 3:2 because of need to swap long and lat order for google

trial <- lapply(1:nrow(melbinnergridcentroid), function(x){
  foo <- google_places(location = unlist(melbinnergridcentroid[x,3:2]), place_type = "supermarket", radius = 3000, key = key)
  
  
  return(foo)
})

## extract results into separate data frame

super1 <- sapply(trial, function(x) x["results"])

## extract next_page_tokens and add NAs to enable matching of rows

npt1 <- sapply(trial, function(x) x["next_page_token"])
npt1 [sapply(npt1, is.null)] <- NA
melbinnergridcentroid2 <- cbind(melbinnergridcentroid, npt1)

## turn into data frame to remove nas and then back to a list again

melbinnergridcentroid2 <- as.data.frame(melbinnergridcentroid2)
melbinnergridcentroid2 <- melbinnergridcentroid2 %>% filter(!is.na(npt1))
melbinnergridcentroid2 <- list(melbinnergridcentroid2)


trial2 <- lapply(1:nrow(melbinnergridcentroid2[[1]]), function(x){
  foo2 <- google_places(location = unlist(melbinnergridcentroid2[[1]][x,3:2]), place_type = "supermarket", radius = 3000, key = key, page_token = unlist(melbinnergridcentroid2[[1]][[x,"npt1"]]))
  return(foo2)
})

super2 <- sapply(trial2, function(x) x["results"])


