library(tidyverse)
library(osmdata)
library(leaflet)
library(sf)


for (i in c(
  "Victoria, Australia",
  "Australian Capital Territory, Australia",
  "New South Wales, Australia",
  "Northern Territory, Australia",
  "South Australia, Australia",
  "Queensland, Australia",
  "Western Australia, Australia",
  "Tasmania, Australia")) {
  
  
  
  #------------
  
  #get each category of food shop location
  coffee_shop<-opq(bbox =i, timeout = 1000) %>%
    add_osm_feature(key="shop", 
                    value=c("chocolate",
                            "coffee")
                    )%>%
    osmdata_sf()
  
  coffee_amenity<-opq(bbox = i, timeout = 1000) %>%
    add_osm_feature(key="amenity", 
                    value="cafe"
    )%>%
    osmdata_sf()

  
  #write to rds
  
  
  coffee_shop%>%write_rds(paste0(
    "/Users/e5028514/Desktop/Projects/food/coffee/data/osm_shop_",
    i, ".rds", sep=""
  ))
  
  coffee_amenity%>%write_rds(paste0(
    "/Users/e5028514/Desktop/Projects/food/coffee/data/osm_amenity_",
    i, ".rds", sep=""
  ))
  
}

food_data_combine<-function(data_file="/Users/e5028514/Desktop/Projects/food/data/New Folder With Items/Queensland, Australia.rds"){
  
  data<-read_rds(data_file)
  
  data_g<-data[["osm_polygons"]]|> 
    st_centroid()
  
  #get points
  
  data_p<-data$osm_points  
  
  data_all<-bind_rows(data_g, data_p)
}

test<-read_rds("/Users/e5028514/Desktop/Projects/food/coffee/data/osm_amenity_Victoria, Australia.rds")


test<-test$osm_points

# create leaflet map
leaflet() %>%
  addTiles() %>%
  addMarkers(data = test, popup = ~osm_id)

df <- list.files( path="/Users/e5028514/Desktop/Projects/food/coffee/data/", full.names=TRUE ) %>%
  map(
    food_data_combine) |>bind_rows() 

df<-bind_rows(df)
df<-df|>distinct()


# create leaflet map
leaflet() %>%
  addTiles() %>%
  addMarkers(data = df, popup = ~osm_id)

df|>write_rds("/Users/e5028514/Desktop/Projects/food/coffee/melbourne/all_osm.rds")

