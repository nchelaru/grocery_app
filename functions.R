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
library(shiny)


# which fields are mandatory
fieldsMandatory <- c("store", "receipt")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

# directory where responses get stored
responsesDir <- file.path(".")


# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("res.csv")
  
  write.csv(
    x = data,
    file = file.path(responsesDir, fileName),
    row.names = FALSE,
    quote = TRUE
  )
  
  data <- as.data.frame(data)
  
  #dbWriteTable(con, "apptest", data, append = TRUE)
}


# load all responses into a data.frame
loadData <- function() {
  # files <- list.files(file.path(responsesDir), full.names = TRUE)
  # data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  # data <- do.call(rbind, data)
  data
  
}

convert.dtype <- function(obj, types) {
  for (i in 1:length(obj)) {
    FUN <- switch(types[i],
                  character = as.character,
                  numeric = as.numeric,
                  factor = as.factor)
    obj[, i] <- FUN(obj[, i])
  }
  obj
}



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


unique_stores <- function(search_term) {
  res_json <- get_flyer(search_term)
  filtered_data <- parse_flyer(res_json)
  stores <- unique(filtered_data$merchant_name)

  
  nearby <- c("Coppa's Fresh Market", 'Food Basics', 'Loblaws', 'Metro', 'Sobeys', "Fortino's", 
              "FreshCo", 'Walmart', 'T&T Supermarket', "Galleria Supermarket", "H Mart ", "No Frills", 
              "Shoppers Drug Mart", 'Btrust', 'LCBO', 'Independent City Market', "Whole Foods", 'Canadian Tire')
  
  store_list <- list()
  
  k <- 1
  
  for (i in stores) {
    if (i %in% nearby) {
      store_list[[k]] <- i
      k <- k+1
    }
  }
  
  return(as.character(store_list))
}


card <- function(.img, item, store, valid_from, valid_to) {
  HTML(
    paste0(
      '<div class="card">
      <div class="container">
      <h5><b>',
      item,
      '</b></h5>
      </div>
      <center><img src="',
      .img,
      '" style="width:100%"></center>
      <div class="container">
      <h4><b>', store, '</b></h4>
      <p><i>', valid_from, ' - ', valid_to, '</i></p>
      </div>
      </div>'
    )
  )
}

create_grid_store <- function(session, search_term) {
  res_json <- get_flyer(search_term)
  
  if (length(res_json$items) == 0) {
    output$cards <-  renderText(HTML("<br><br><center><h2>Oops, the flyer is currently unavailable!</h2></center>"))
    
  } else {
    if (search_term == 'whole+foods') {
      filtered_data <- parse_flyer(res_json) %>% dplyr::filter(grepl("Whole Foods", merchant_name))  
    } else {
      filtered_data <- parse_flyer(res_json)
    }
    
    
    updateTabItems(session, "tabs", "flyers")
    
    cards <- renderUI({
      # First make the cards
      args <-
        lapply(1:dim(filtered_data)[1], function(x)
          card(filtered_data[x, "clipping_image_url"],
               store = filtered_data[x, "merchant_name"],
               item = filtered_data[x, "name"],
               valid_from = filtered_data[x, "valid_from"],
               valid_to = filtered_data[x, "valid_to"]))
      
      num <- dim(filtered_data)[1]
      
      
      if (num %% 4 == 0) {
        num2 <- num / 4
        
        num3 <- num2 -1
        
        cols <-
          lapply(seq(num2, num2*4, num2), function(x) {
            column(width = 3, verticalLayout(args[(x - num3):x], fluid = TRUE))
          })
      } else {
        num2 <- num %/% 4 + 1
        
        cols <-
          lapply(seq(num2, num2*4, num2), function(x) {
            column(width = 3, verticalLayout(args[(x - num%/%4):x], fluid = TRUE))
          })
      }
      
      
      # basically the same as flowLayout(cards[[1]], cards[[2]],...)
      do.call(shiny::fluidRow, cols)
    })
    
    return(cards)
  }
}



create_grid_item <- function(session, search_term, store) {
  res_json <- get_flyer(search_term)
  
  if (length(res_json$items) == 0) {
    output$cards <-  renderText(HTML("<br><br><center><h2>Oops, the flyer is currently unavailable!</h2></center>"))
    } else {
      filtered_data <- parse_flyer(res_json)  %>% dplyr::filter(grepl(store, merchant_name))  
    }
    
    
    updateTabItems(session, "tabs", "flyers")
    
    cards <- renderUI({
      # First make the cards
      args <-
        lapply(1:dim(filtered_data)[1], function(x)
          card(filtered_data[x, "clipping_image_url"],
               store = filtered_data[x, "merchant_name"],
               item = filtered_data[x, "name"],
               valid_from = filtered_data[x, "valid_from"],
               valid_to = filtered_data[x, "valid_to"]))
      
      num <- dim(filtered_data)[1]
      
      
      if (num %% 4 == 0) {
        num2 <- num / 4
        
        num3 <- num2 -1
        
        cols <-
          lapply(seq(num2, num2*4, num2), function(x) {
            column(width = 3, verticalLayout(args[(x - num3):x], fluid = TRUE))
          })
      } else {
        num2 <- num %/% 4 + 1
        
        cols <-
          lapply(seq(num2, num2*4, num2), function(x) {
            column(width = 3, verticalLayout(args[(x - num%/%4):x], fluid = TRUE))
          })
      }
      
      
      # basically the same as flowLayout(cards[[1]], cards[[2]],...)
      do.call(shiny::fluidRow, cols)
    })
    
    return(cards)
}



shoppers_parse <- function(v, k, i, receipt_date, script_type) {
  for (r in strsplit(v, '\t\r\n')) {
    k[[i]] <- strsplit(r, '\t')
    
    i <- i + 1
  }
  
  z <- as.data.frame(do.call(rbind, k[[1]]))
  
  colnames(z) <-
    c('Item', 'UnitPrice', 'TotalPrice')
  
  for (x in c('UnitPrice', 'TotalPrice')) {
    z[[x]] <- sub(" .*", "", z[[x]])
    z[[x]] <- gsub("$", "", z[[x]])
  }
  
  z$date <- as.character(receipt_date)
  
  z$store <- script_type
  
  return(z)
}



wf_parse <- function(v, k, i, receipt_date, script_type) {
  for (r in strsplit(v, '\t\r\n')) {
    k[[i]] <- strsplit(r, '\t')
    i <- i + 1
  }
  
  df <- as.data.frame(do.call(rbind, k[[1]]))
  
  df$V3 <- NULL
  
  df <- dplyr::filter(df, !grepl("ITEM", V1) & !grepl("TARE", V2) & !grepl("ITEM", V2 ))
  
  colnames(df) <- c('Item', 'TotalPrice')
  
  df$UnitPrice <- " "
  
  df$TotalPrice <-
    sapply(df$TotalPrice, function(x)
      gsub("\\$", "", x))
  
  df$date <- as.character(receipt_date)
  
  df$store <- script_type
  
  return(df)
}


hmart_parse <- function(v, k, i, receipt_date, script_type) {
  for (r in strsplit(v, '\t\r\n')) {
    k[[i]] <- strsplit(r, '\t')
    i <- i + 1
  }
  
  z <- as.data.frame(do.call(rbind, k[[1]]))
  
  z <- z[seq(1, nrow(z), 2) ,]
  
  z <- separate(
    data = z,
    col = 'V1',
    into = c("Item", "UnitPrice"),
    sep = " @"
  )
  
  colnames(z) <-
    c("Item", "UnitPrice", "TotalPrice")
  
  z$UnitPrice <-
    sapply(z$UnitPrice, function(x)
      gsub("\\$", "", x))
  
  z$date <- as.character(receipt_date)
  
  z$store <- script_type
  
  return(z)
}

