library(knitr)
library(httr)
library(RCurl)
library(xml2)
library(jsonlite)
library(tidyr)
library(DTedit)
library(magrittr)
library(V8)
library(plyr)
library(dplyr)



get_flyer <- function(search_term) {
  res <- GET(sprintf("https://backflipp.wishabi.com/flipp/items/search?locale=en-ca&postal_code=M4Y2W4&q=%s&limit=1000", search_term))
  
  res_text <- content(res, "text")
  
  res_json <- fromJSON(res_text, flatten = TRUE)
    
  return(res_json)
}


parse_flyer <- function(res_json) {
  filtered_data <- res_json$items
  
  filtered_data <- filtered_data %>%
    select(clipping_image_url, merchant_name, name, valid_to, valid_from)
  
  filtered_data$valid_from <- format(as.Date(filtered_data$valid_from), "%b %d")
  filtered_data$valid_to <- format(as.Date(filtered_data$valid_to), "%b %d")
  
  filtered_data[is.na(filtered_data)] <- ""
  
  filtered_data <- filtered_data[order(filtered_data$merchant_name), ]
  
  return(filtered_data)
}


