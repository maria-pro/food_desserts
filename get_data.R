library(googleway)
library(tidyverse)
library(httr)

library(tidyverse)
library(googleway)

key   <- "AIzaSyCDh-0018LAAPHzQU4jtQq0lFpGUXNcNyo"
postcodes <- c("3151","3133")

postcodes_full<-read_csv("data/au_postcodes.csv")

#Victoria postcode

postcodes_vic<-postcodes_full%>%filter(state_code=="VIC")

postcodes_vic_q<-postcodes_vic$postcode
  
queries <- paste0("food in ", postcodes_vic_q, ", Victoria, Australia")

# A custom loop function for google_places() 
google_places_loop <- function(search_string, key, ntimes = 3, page_token = "") {
  
  print(search_string)    
  iter <- 0
  obj_df <- tibble()
  
  while(iter < ntimes & !is.null(page_token)) {
    iter <- iter + 1
    print(iter)
    obj_response <- google_places(location=vvvv,
                                  ty
      key = key, page_token = page_token,
                                  language = "DE", # Optional, but note that setting language to German might get you a few more locations
    )
    obj_df_new <-   as_tibble(obj_response$results) %>% mutate(iter = iter)
    obj_df <- bind_rows(obj_df, obj_df_new)
    page_token <- obj_response$next_page_token
    
    if(is.null(page_token) == TRUE) {
      print("No more pagination tokens")
      Sys.sleep(2)
    } else {
      Sys.sleep(3) 
    }
    
  }
  
  obj_df
  
}

# Finally, we loop through the queries by the custom function.
df_postcodes <- map_df(.x = queries, .f = google_places_loop, key = key)

vic_postcodes <- map_df(.x = queries, .f = google_places_loop, key = key)

df_postcodes%>%write_rds("peter_codes.rds")

vic_postcodes%>%write_rds("vic_codes.rds")

t<-read_rds("peter_codes.rds")
