#this is the updated file - USE THIS
#this file locates food outlets in australia state by state according to osm food categories
#also it locates campuses using OSM, alternative campus location is sourced from ABR, same for food outlets

#also it locates retail areas as identified by OSM using landuse

library(tidyverse)
library(osmdata)
library(ggmap)
library(googleway)
library(sf)
library(plotly)
library(mapview)
library(tidygeocoder)


#----------------
#food categories at OSM

#key = shop

#key = shop


food_shop<-c(
  "bakery",
  "beverages",
  "butcher",
  "cheese",
  "chocolate",
  "coffee",
  "confectionery",
  "convenience",
  "deli",
  "dairy",
  "farm",
  "frozen_food",
  "greengrocer",
  "health_food",
  "ice_cream",
  "pasta",
  "pastry",
  "seafood",
  "spices",
  "tea",
  "water",
  "alcohol", 
  "wine",
  "department_store",
  "supermarket",
  "wholesale",
  "bar",
  "cafe",
  "fast_food",
  "food_court",
  "ice_cream",
  "pub",
  "restaurant",
  "kiosk", "mall"
)




#----------------
#osm lookup
# https://www.openstreetmap.org/node/26706320


for (i in c(
  "Victoria, Australia",
  "ACT, Australia",
  "New South Wales, Australia",
  "Northern Territory, Australia",
  "South Australia, Australia",
  "Queensland, Australia",
  "Tasmania, Australia")) {
  
#get higher ed locations
  
#------------


bb_box <- getbb(i, format_out = "polygon") 

campus<-bb_box%>%
  opq(timeout = 1000) %>%
  add_osm_feature(key = "amenity", value = c("university","college")) %>%
  osmdata_sf()

#get each category of food shop location
data_food_shop<-bb_box %>%
  opq(timeout = 1000) %>%
  add_osm_feature(key="shop", value=food_shop)%>%
  osmdata_sf()

#write to rds

campus%>%write_rds(
  paste0(
    i, "_campus.rds", sep=""
  ))


data_food_shop%>%write_rds(paste0(
  "data/osm_new/",
  i, ".rds", sep=""
))


}

#---function bind data


food_data_combine<-function(data_file="/Users/e5028514/Desktop/Projects/food/data/New Folder With Items/Queensland, Australia.rds"){
  
  data<-read_rds(data_file)
  
  data_g<-data[["osm_polygons"]]|> 
    st_centroid()
  
  #get points

  data_p<-data$osm_points  
  
  data_all<-bind_rows(data_g, data_p)
  data_all|>write_csv(paste0(data_file,
                             ".csv"))
  
}

  
df <- list.files( path="/Users/e5028514/Desktop/Projects/food/data/osm_new/data_rds", full.names=TRUE ) %>%
  map(
food_data_combine) |>bind_rows() 

df<-bind_rows(df)

df|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/osm_combined_all.rds")

#df<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/osm_combined_all.rds")|>distinct()

#---convert to points (from multipoints in some cases for mapping)

all_food_osm<-df%>%
  select(
    osm_id,
    shop, 
    amenity,
    geometry
  )

all_food_osm%>%st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/aus_all_food_short.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

all_food_osm|>write_csv("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_short.csv")

all_food_osm|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_short.rds")

test_g<-all_food_osm|>st_cast("MULTIPOINT") %>%
  st_cast("POINT")

all_food_osm_coord<-unlist(st_geometry(test_g)) %>% 
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))#%>%distinct()

all_food_osm_flat<-all_food_osm|>st_drop_geometry()|>
  cbind(all_food_osm_coord)|>distinct() #distinct is really needed as there are obvious duplicates re osm_id and type of shop

all_food_osm_flat|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_flat_coord.rds")

all_food_osm_flat|>count(amenity, sort=TRUE)
all_food_osm_flat|>count(shop, sort=TRUE)

all_food_osm_flat|>filter(is.na(shop) & is.na(amenity))
#------------------------------

#reverse geocode to remove duplicates

all_food_osm_flat_address<-all_food_osm_flat|>
 # head(10)|>
  tidygeocoder::reverse_geocode(   lat=lat,
                                              long=lon,
                                              address="address",
                                              method="osm",
                                              full_results = TRUE) 

#final file for osm 
all_food_osm_flat_address|>write_csv("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_flat_address.csv")

all_food_osm_flat_address|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_flat_address.rds")

  

all_food_osm_flat_address|>count(address, sort=TRUE)

all_food_osm<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_flat_address.rds")|>
  select(-c(boundingbox))|>
  rename(
    "osm_id"="osm_id...1"
  )
#all_food_osm_og<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/osm_combined_all.rds")

all_food_osm<-df|>left_join(all_food_osm)


#final for osm before
all_food_osm|>write_csv("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_before_combine.csv")

all_food_osm|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_before_combine.rds")

all_food_osm|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_before_combine.geojson")







#reverse geocode abr data - too big and does not work - skip for now and go straight to osm
#-------------
food_abr_full<-read_csv("/Users/e5028514/Desktop/Projects/food/data/temp/abr_all_clean.csv")
  #read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses_sa.rds") with sf

food_abr_full<-food_abr_full|>filter(main_industry_class_code < 4310 | main_industry_class_code >4500)|>
  filter(latitude!=0)

all_food_abr_flat_address1<-
  tidygeocoder::reverse_geocode( food_abr_full[0:50000,],
    lat=latitude,
                                   long=longitude,
                                   address="address",
                                   method="osm",
                                   full_results = TRUE) 


all_food_abr_flat_address1|>write_rds("all_food_abr_flat_address1.rds")

#t<-food_abr_full[0:100000,]


#------------------------
#------------------------
#retail areas in Australia

#looop to go through each state to locate

for (i in c(
  "Victoria",
  "ACT",
  "New South Wales",
  "Northern Territory",
  "South Australia",
  "Queensland",
  "Tasmania")) {
  
  #get higher ed locations
  
  bb_box <- getbb(i, format_out = "polygon") 
 
  #use OSM landuse to locate 
  retail<-bb_box%>%
    opq(timeout = 1000) %>%
    add_osm_feature(key = 'landuse', value = 'retail') %>%
    osmdata_sf()
  
  #write to rds
  
  retail%>%write_rds(
    paste0(
      i, "_retail.rds", sep=""
    ))

}

#---
# some areas are located as points and others as polygons
#saving them separately
# points - locate venues in the radius
#polygons locate venues within

#---function bind data

#save polygons
retail_data_poly<-function(data_file){
  
  data<-read_rds(data_file)
  
  data_g<-data$osm_polygons|>select(
    osm_id,
    geometry
  )
  
  return (data_g)
}  
  
#save points
retail_data_points<-function(data_file){
  
  data<-read_rds(data_file)
  
  data_g<-data$osm_points|>
    select(
      osm_id,
      geometry
    )
  
  return (data_g)
} 
  
 
#do not use retail for OSM data
#moved all retail OSM files from osm to a separate dir for merging to run and combine

#merging all polygons
df <- list.files( path="/Users/e5028514/Desktop/Projects/food/data/osm/retail areas_raw/", full.names=TRUE )
test<-df|>map_dfr(\(df) retail_data_poly(data_file=df)) 

test|>distinct(osm_id, .keep_all=TRUE)|>write_rds("retail_all_poly.rds")

test|>distinct(osm_id, .keep_all=TRUE)|>st_write("retail_all_poly.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

#merging all points
df <- list.files( path="/Users/e5028514/Desktop/Projects/food/data/osm/retail areas_raw/", full.names=TRUE )
test<-df|>map_dfr(\(df) retail_data_points(data_file=df)) 

test|>distinct(osm_id, .keep_all=TRUE)|>write_rds("retail_all_points.rds")

test|>distinct(osm_id, .keep_all=TRUE)|>st_write("retail_all_points.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")
#---------------

#merging both points and polygons together
test_geom<-df|>map_dfr(\(df) retail_data_poly(data_file=df)) 
test_point<-df|>map_dfr(\(df) retail_data_points(data_file=df)) 

test_geom<-test_geom|>
  mutate(dataset="polygon")

test_point<-test_point|>
  mutate(dataset="point")

retail_all<-rbind(test_geom, test_point)
#retail_all|>write_rds("retail_all.rds")

#retail_all|>st_write("retail_all.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

#moved to proper location
retail_all<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm/retail areas_clean/retail_all.rds")

class(retail_all)
st_geometry(retail_all)

retail_all_polygons<-retail_all|>filter(dataset=="polygon")
food_all_osm<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm/food_osm_done/all_food_osm_coord.rds")
#----------------
#add postcode, sa2-sa4 areas - link ses


sa4_2021<-absmapsdata::sa42021
sa2_2021<-absmapsdata::sa22021
sa1_2021<-absmapsdata::sa12021
options(scipen=999)
postcode2021<-absmapsdata::postcode2021
  
retail_all_full<-st_join(postcode2021, retail_all, 
                              join = st_contains)%>%
  filter(!is.na(osm_id))  
  
retail_all_full<-retail_all_full|>select(osm_id, postcode_2021, postcode_num_2021, dataset)|>st_drop_geometry()

retail_all_full<-retail_all|>left_join(retail_all_full, relationship = "many-to-many")|>distinct(osm_id, postcode_2021, .keep_all=TRUE)

#checks
retail_all_full|>count(dataset)

#retail_all_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/retail_osm_all_postcode.rds")

#retail_all_full|>count(postcode_2021)

#----------
#join with sa2

sa2_all_full<-st_join(sa2_2021, retail_all_full, 
                         join = st_contains)%>%
  filter(!is.na(osm_id))  

sa2_all_full<-sa2_all_full|>select(
  -c(
    cent_lat, cent_long, areasqkm_2021
  )
)|>st_drop_geometry()

retail_all_full

retail_all_full<-retail_all_full|>left_join(sa2_all_full)|>
  distinct(osm_id, .keep_all=TRUE)

retail_all_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/retail_osm_all_postcode_sa.rds")

#--------------
#------------- DO SAME FOR OSM FOOD

all_food_osm

#from
#> all_food_osm<-read_csv("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_before_combine.csv")

#all_food_osm<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/all_food_osm_before_combine.rds")

class(all_food_osm)

#test<-st_as_sf(all_food_osm)

#all_food_osm <- st_as_sf(all_food_osm, coords = c("osm_lat", "osm_lon"))
#st_geometry(all_food_osm)

#st_crs(postcode2021)
#st_crs(all_food_osm) <- 4326
#st_crs(all_food_osm)

library(leaflet)
all_food_osm|>head(20)|>leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%  # Choose a tile provider
  addMarkers() 

#postcodes

food_osm_full<-st_join(postcode2021, all_food_osm, 
                         join = st_contains)%>%
  filter(!is.na(osm_id))  


food_osm_full|>filter(is.na(postcode_2021))

food_osm_full<-food_osm_full|>select(osm_id, postcode_2021, postcode_num_2021)|>st_drop_geometry()

food_osm_full<-all_food_osm|>left_join(food_osm_full)|>distinct(osm_id, postcode_2021, .keep_all=TRUE)

#food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode.rds")

#food_osm_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")


#food_osm_full|>count(postcode_2021, sort=TRUE)

#----------
#join with sa2

sa2_all_full<-st_join(sa2_2021, food_osm_full, 
                      join = st_contains)%>%
  filter(!is.na(osm_id))  

sa2_all_full<-sa2_all_full|>select(
  -c(
    cent_lat, cent_long, areasqkm_2021
  )
)|>st_drop_geometry()

food_osm_full<-food_osm_full|>left_join(sa2_all_full)|>
  distinct(osm_id, .keep_all=TRUE)

food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa.rds")
food_osm_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

#--------------
#-------------- food intersect with polygons for retail OSM - do last step

retail_all_polygons<-retail_all_polygons|>
  rename(
    "osm_id_retail"="osm_id"
  )

food_osm_retail_intersect<-st_join(retail_all_polygons, food_osm_full, 
        join = st_contains)%>%
  filter(!is.na(osm_id_retail))


food_osm_retail_intersect|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_retail_intersect.rds")

food_osm_retail_intersect|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_retail_intersect.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

#--- add ses data
#ses is linked to postcode
ses<-read_csv("/Users/e5028514/Desktop/Projects/food/data/additional/seifa2021.csv")|>
  select(
    postcode,
    seifa_decile,
    seifa_quartile,
    ses)

food_osm_full<-food_osm_full|>left_join(ses, 
                         by=c("postcode_2021"="postcode")
)

food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa.rds")
food_osm_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")


food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa_ses.rds")
food_osm_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa_ses.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")


#-----add remoteness
ra<-absmapsdata::ra2016|>
  select(ra_code, ra, geometry)

test<-st_join(ra, food_osm_full, 
                       join = st_contains)%>%
  filter(!is.na(osm_id))  

test<-test|>st_drop_geometry()

food_osm_full<-food_osm_full|>
  left_join(test)|>distinct(osm_id, .keep_all=TRUE)


food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa_ses_ra.rds")
food_osm_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa_ses_ra.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

food_osm_full|>write_csv("/Users/e5028514/Desktop/Projects/food/data/osm_new/food_osm_all_postcode_sa_ses_ra.csv")

#--------------
#-------------- food intersect with polygons for retail OSM - do last step

retail_all_polygons<-retail_all_polygons|>
  rename(
    "osm_id_retail"="osm_id"
  )

food_osm_retail_intersect<-st_join(retail_all_polygons, food_osm_full, 
                                   join = st_contains)%>%
  filter(!is.na(osm_id_retail))


food_osm_retail_intersect|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_retail_intersect.rds")

food_osm_retail_intersect|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_retail_intersect.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

food_osm_retail_intersect|>write_csv("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_retail_intersect.csv")
#--------------------------DO ALL SAME FOR ABR data

df <- list.files( path="/Users/e5028514/Desktop/Projects/food/data/abr/food_45_41", full.names=TRUE ) %>% 
  map_dfr(read_csv) |>
  janitor::clean_names()

df|>count(main_industry_class_code)
df<-df|>filter(main_industry_class_code>4100)

df<-df|>filter(main_industry_class_code!=4400)|>
  filter(main_industry_class_code<4529)

df<-df|>filter(latitude!=0)
df|>write_csv("/Users/e5028514/Desktop/Projects/food/data/temp/abr_all_clean.csv")

food_abr<-st_as_sf(df, coords = c("longitude", "latitude"), crs=4326)

#add ses

ses<-ses|>mutate(postcode_num=parse_number(postcode))
food_abr_full<-food_abr|>left_join(ses, 
                                        by=c("business_location_postcode"="postcode_num")
)

food_abr_full<-food_abr_full|>rowid_to_column("id")

food_abr_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses.rds")
food_abr_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

#food_abr_full<-read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses.rds")


#join with sa2

sa2_all_full<-st_join(sa2_2021, food_abr_full, 
                      join = st_contains)%>%
  filter(!is.na(australian_business_number))  

sa2_all_full<-sa2_all_full|>select(
  c(id, 
    sa2_code_2021, sa2_name_2021,
    sa3_code_2021, sa3_name_2021,
    sa4_code_2021, sa4_name_2021,
    gcc_code_2021, gcc_name_2021,
    state_code_2021, state_name_2021
  )
)|>st_drop_geometry()

#sa2_all_full|>write_csv("/Users/e5028514/Desktop/Projects/food/data/temp/abr_id_sa.csv")


test2<-food_abr_full|>left_join(sa2_all_full)


food_abr_full<-test2
food_abr_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_sa.rds")
food_abr_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_sa.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")
food_abr_full<-read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_sa.rds")

#messed up

#food_osm_full<-st_read("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_all_postcode_sa.geojson")

#food_osm_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_all_postcode_sa.rds")

#food_osm_full<-read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_all_postcode_sa.rds")

#-----add remoteness
ra<-absmapsdata::ra2016|>
  select(ra_code, ra, geometry)

ra_join<-st_join(ra, food_abr_full, 
              join = st_contains)%>%
  filter(!is.na(australian_business_number))|>
  select(id,
         ra_code, ra)|>st_drop_geometry()

food_abr_full<-food_abr_full|>left_join(ra_join)

food_abr_full|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses_sa.rds")
food_abr_full|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses_sa.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")
food_abr_full|>write_csv("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_sa.csv")

#intersect with abr remote

#-------------- food intersect with polygons for retail OSM - do last step

#retail_all_polygons<-retail_all_polygons|>
  rename(
    "osm_id_retail"="osm_id"
  )

food_abr_retail_intersect<-st_join(retail_all_polygons, food_abr_full, 
                                   join = st_contains)%>%
  filter(!is.na(australian_business_number))


food_abr_retail_intersect|>write_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_retail_intersect.rds")

food_abr_retail_intersect|>st_write("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_retail_intersect.geojson", delete_layer = TRUE, layer_options = "GEOMETRY=AS_XY")

food_abr_retail_intersect|>write_csv("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_retail_intersect")


#-------------------------  

###use google maps

#get geometry for postcodes, split into areas and get centroid to use for google maps

library(sf)
library(mapview)
library(tidyverse) 
library(dismo)
library(osmdata)  
library(mapview)

#if radius is 5km then n_areas= area/ 3.14*5^2=78

postcode2021<-absmapsdata::postcode2021

postcode2021

#googleway setup
library(googleway)

format_res <- function(res) {
  setNames(
    cbind(
      googleway::access_result(res, "coordinates"),
      googleway::access_result(res, "place_name"),
      googleway::access_result(res, "place")       ## store the unique place_id as well
    )
    , c("lat", "long", "name", "place_id")
  )
}

do_search <- function(place_type, key, location, radius, page_token = NULL) {
  
  google_places(
    place_type = place_type,
    location = location,
    key = key,
    radius = radius,
    page_token = page_token
  )
  
}

full_search <- function(place_type, key, location, radius) {
  
  counter <- 0
  
  page_token <- NULL ## can start on NULL because it means we're doing the first query
  is_another_page <- TRUE 
  
  ## initialise a data.frame to store the results
  df <- data.frame(
    lat = vector("numeric", 0L)
    , long = vector("numeric", 0L)
    , name = vector("character", 0L)
    , place_id = vector("character", 0L)
  )
  
  while( is_another_page ) {
    
    res <- do_search(place_type, key, location, radius, page_token)
    
    if( res$status == "OK" ) { ## check a valid result was returned
      
      if( counter == 0 ) {
        df <- format_res( res )
      } else {
        df <- rbind(df, format_res( res ) )
      }
      
      counter <- counter + 1
    } else {
      ## print a message for not-OK results
      print(paste0(res[["status"]], " for ", paste0(location, collapse = ", ") ))
    }
    
    page_token <- res[["next_page_token"]]
    is_another_page <- !is.null( page_token )
    Sys.sleep(3)  ## Sleep the function before the next call because there's a time limit
  }
  return(df)
}

## I've added a 3rd example that actually has results
dfCoords <- data.frame(
  coords = c("-37.76709, 144.9826" ,"-37.77396, 144.9728", "-37.77423, 144.9754")
)

key <- "AIzaSyCDh-0018LAAPHzQU4jtQq0lFpGUXNcNyo"
place_type <- "bakery" 
radius <- 1000

## create a list to store the results
lst_results <- vector("list", length = nrow(dfCoords))
## Using a list will be more efficient that `rbind`-ing a data.frame in each iteration

## loop through the indexes of the coordinates
## this wy we can assign the results to the correct index of the list
for (i in 1:nrow(dfCoords) ) {
  
  location <- dfCoords[i, "coords"]
  
  ## the coordiantes must be a numeric vector
  location <- as.numeric(strsplit(location, ",")[[1]])
  
  
  lst_results[[ i ]] <- full_search(
    place_type = place_type
    , key = key
    , location = location
    , radius = radius
  )
}

lapply(lst_results, head)

#-------

split_poly <- function(sf_poly, n_areas){
  # create random points
  points_rnd <- st_sample(sf_poly, size = 10000)
  #k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- st_area(equal_areas)
  return(equal_areas)
}
Using the same data as @Elio Diaz earlier in this thread:
  
  pol <- osmdata::getbb("aguascalientes", format_out = "sf_polygon") 
pol_areas <- split_poly(pol, 20)
mapview(pol_areas)


#-------------------------

#prepare abr for maps

#food_abr_full<-read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_abr_all_postcode_ses_sa.rds")

n<-as_tibble(names(food_abr_full))

abr_map<-food_abr_full|>mutate(
  dataset="abr",
  postcode_num_2021=parse_number(postcode),
  shop="not available",
  amenity="not available"
)|>
  rename(
    "postcode_2021"="postcode"
  )

#abr_map|>st_write("abr_map.geojson")

abr_map<-abr_map %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

#abr_map|>st_write("abr_map.geojson")
#abr_map|>write_rds("abr_map.rds")

#abr_map<-st_drop_geometry(abr_map_1)

class(abr_map_1)

#food_osm_full<-read_rds("/Users/e5028514/Desktop/Projects/food/data/temp/food_osm_all_postcode_sa_ses_ra.rds")

osm_map<-food_osm_full|>mutate(
  dataset="osm"
)|>
  select(
    -starts_with("addr.")
    #-starts_with("addr:")
  )|>mutate(
    australian_business_number="not available",
registered_business_name="not available",
entity_organisation_name="not available",
main_trading_name="not available",
entity_name="not available",
main_industry_class_code="not available",
main_industry_class="not available",
business_location_address_line_1="not available",
business_location_address_line_2="not available",
business_location_suburb="not available",
location_type="not available",
abs_sa2_significant_locality="not available",
business_location_postcode="not available")|>
  rename(
    "id"="osm_id"
  )

osm_map<-osm_map %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])


map_data<-rbind(abr_map,osm_map)

map_data|>write_rds("map_data.rds")
map_data|>write_csv("map_data.csv")

map_data|>st_write("map_data.geojson")

#osm_map|>write_rds("vic_map_data.rds")
#osm_map|>write_csv("vic_map_data.csv")
#osm_map_vic<-osm_map

#osm_map|>st_write("vic_map_data.geojson")


#map_data<-st_read("map_data.geojson")


#remove more irrelevant industryies

map_data<-map_data|>filter(main_industry_class_code < 4310 | main_industry_class_code >4500) 
  
map_data|>write_rds("map_data.rds")
map_data|>write_csv("map_data.csv")

map_data|>st_write("map_data.geojson", delete_layer = TRUE)

#merging vic


map_data<-st_read("map_data.geojson")
osm_map_vic<-st_read("/Users/e5028514/Desktop/Projects/food/data/final data/vic_map_data.geojson")


osm_map_vic<-osm_map_vic|>
  select(
  -starts_with("addr.")
  )
  
  
  
map_data_all<-rbind(map_data,osm_map_vic)

map_data_all<-map_data_all|>distinct()

map_data_all|>write_rds("map_data_all_plus_vic.rds")
map_data_all|>st_write("map_data_all_plus_vic.geojson")
map_data_all|>write_csv("map_data_all_plus_vic.csv")


map_data_all<-map_data_all|>filter(!is.na(postcode_2021))

map_data_all|>write_rds("map_data_all_plus_vic_r.rds")
map_data_all|>st_write("map_data_all_plus_vic_r.geojson")
map_data_all|>write_csv("map_data_all_plus_vic_r.csv")

test<-map_data_all|>
  mutate(
    postcode_2021=case_when(
    is.na(postcode_num_2021)~business_location_postcode,
    TRUE~postcode_2021
  ),
  postcode_num_2021=parse_number(postcode_2021)
  )

no_postcode<-test|>filter(is.na(postcode_2021))

map_data_all<-map_data_all|>filter(!is.na(postcode_2021))

osm<-map_data_all|>filter(dataset=='osm')

osm_no<-osm|>filter(is.na(postcode_2021))


osm_no_address<-tidygeocoder::reverse_geocode(osm_no,
                              lat=lat,
                              long=lon,
                              address="address",
                              method="osm") 

data_geocode_full<- SUNGEO::geocode_osm_batch(osm_no, 
                                      details=TRUE, verbose = TRUE)

#----------------------------
#to do

#locate top 50 postcodes across au with highest numbers of food venues, osm and abr separately and together - should match

#locate top categories in osm

#locate top categores in abr

# locate top suburbs in each category

# ses and - low vs high

# sa2 areas same as for postcodes

#same for lowest count in postcodes



#stopped here
  
  vic_retail_intersect<-st_join(vic_retail, all_vic, 
                                join = st_contains)%>%
  filter(!is.na(osm_retail_id))


vic_retail_intersect%>%st_drop_geometry()%>%count(osm_retail_id, sort=TRUE)%>%filter(n==0)

vic_retail_intersect<-vic_retail_intersect%>%select(
  osm,
)
st_within(pnts,eez_poly)

#interse  

#--------------
#view on map
library(tmap)
tmap_mode("view")
tm_shape(retail_all) + tm_polygons()

#load_data<-read_rds("/Users/e5028514/Desktop/Projects/food/data/osm/retail areas_raw/South Australia_retail.rds")

#----------
#wokring with located OSM retail areas


vic_retail<-vic_retail%>%
  select(
    osm_id,
    name,
    branch,
    brand,
    building,
    landuse,
    ref,
    service,
    shop,
    short_name,
    source,
    geometry
  )%>%
  rename(
    osm_retail_id=osm_id
  )

vic_retail%>%st_drop_geometry()%>%skimr::skim()
vic_retail%>%
  st_drop_geometry()%>%
  count(source, sort=TRUE)%>%
  reactable()

vic_retail%>%
  st_drop_geometry()%>%
  count(shop, sort=TRUE)%>%
  reactable()

vic_retail%>%
  st_drop_geometry()%>%
  count(building, sort=TRUE)%>%
  reactable()
```

#### Map
```{r}
mapview(vic_retail)
```

### Food venue data
```{r}
all_vic<-st_read("data/osm/all_vic_messy.geojson", quiet=TRUE)
#vic food data 
#all_vic

class(all_vic)
all_vic<-all_vic%>%
  select(
    osm_id,
    name,
    branch,
    brand,
    building,
    landuse,
    ref,
    service,
    shop,
    short_name,
    source,
    geometry
  )%>%
  rename(
    osm_venue_id=osm_id
  )

all_vic%>%st_drop_geometry()%>%skimr::skim()

all_vic_tbl<-all_vic%>%st_drop_geometry()

all_vic%>%
  st_drop_geometry()%>%
  count(source, sort=TRUE)%>%
  reactable()

all_vic%>%
  st_drop_geometry()%>%
  count(shop, sort=TRUE)%>%
  reactable()

all_vic%>%
  st_drop_geometry()%>%
  count(building, sort=TRUE)%>%
  reactable()
```

#### Map
```{r}
mapview(all_vic)
```


### Locate within

```{r eval=FALSE}
vic_retail_intersect<-st_join(vic_retail, all_vic, 
                              join = st_contains)%>%
  filter(!is.na(osm_retail_id))

class(vic_retail_intersect)
vic_retail_intersect%>%st_write("data/vic_retail_intersect.geojson")
```

```{r}
vic_retail_intersect<-st_read("data/vic_retail_intersect.geojson")
class(vic_retail_intersect)
vic_retail_intersect%>%
  st_drop_geometry()%>%
  count(osm_retail_id, sort=TRUE)%>%
  filter(n==0)%>%
  reactable()
```

```{r}
mapview(vic_retail_intersect)
```

