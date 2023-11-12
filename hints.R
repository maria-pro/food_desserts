Questions:
  
  1. Areas to be covered: 

  gcc_code_2021 == "2GMEL"

sa4_code_2021 == "206")
  
  2. outlet categories


  
  If a place ranks greater than 60th in prominence it will not be included in the search result, even if it is closer to the center of your search. 
  You can sort your results by distance instead of prominence by setting the rankby parameter in your query to distance and omitting the radius parameter.
  
  
  
  numeric Defines the distance (in meters) within which to return place results. Required if only a location search is specified. The maximum allowed radius is 50,000 meters. Radius must not be included if rankby is used. see Details.
  
  rankby	
  string Specifies the order in which results are listed. Possible values are "prominence" or "distance". 
  If rankby = distance, then one of keyword, name or place_type must be specified. If a search_string is used then rankby is ignored.
  
  
  
  
  https://stackoverflow.com/questions/15785960/how-to-sort-results-by-distance-with-google-places-api-using-nearby-search-reque?rq=2
  
  https://issuetracker.google.com/issues/35823583
  
  
  https://github.com/andrea-ballatore/google-places-api-r/blob/master/code/google_place_scraper.R
  
  bakery
  bar
  cafe
  convenience_store
  gas_station
  liquor_store
  meal_delivery
  meal_takeaway
  restaurant
  supermarket
  
  