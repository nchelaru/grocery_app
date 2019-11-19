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
library(quantmod)
library(ggplot2)
library(zoo)
library(plotly)
library(RPostgres)
library(DBI)
library(networkD3)



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



date_heatmap <- function() {
  con <- dbConnect(
    Postgres(),
    dbname = "d2sjdih8tegcuc",
    host = "ec2-184-72-238-22.compute-1.amazonaws.com",
    port = '5432',
    user = "xkktypszvisogc",
    password = "35b7eb67cab6ea1750e5877d61c3415851d6f1ca9c5782ec0b6bf91ede8acbea",
    sslmode = 'require'
  )
  
  ## Aggregate by date
  pur_hist <- dbGetQuery(con, "SELECT * FROM grocery")
  
  colnames(pur_hist) <- c("item", "unitprice", "totalprice", "date", "store")
  
  pur_hist <-  pur_hist[!(is.na(pur_hist$item) | pur_hist$item==""), ]
  
  dat <- pur_hist %>% 
    group_by(date) %>%
    summarise(date_total=sum(totalprice))
  
  dat <- dat %>% 
    complete(date = seq.Date(as.Date("2019-08-01"), max(date), by="day"))
  
  dat$date_total[is.na(dat$date_total)] <- 0
  
  # We will facet by year ~ month, and each subgraph will
  # show week-of-month versus weekday
  # the year is simple
  dat$year <- as.numeric(as.POSIXlt(dat$date)$year+1900)
  
  # the month too 
  dat$month <- as.numeric(as.POSIXlt(dat$date)$mon+1)
  
  # but turn months into ordered facors to control the appearance/ordering in the presentation
  dat$monthf<-factor(dat$month,
                     levels=as.character(1:12),
                     labels=c("January","February","March","April","May", 
                              "June","July","August","September","October","November","December"),
                     ordered=TRUE)
  
  # the day of week is again easily found
  dat$weekday = as.POSIXlt(dat$date)$wday
  
  # I use the reverse function rev here to order the week top down in the graph
  dat$weekdayf<-factor(dat$weekday,
                       levels=rev(c(1, 2, 3, 4, 5, 6, 0)), 
                       labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun")), 
                       ordered=TRUE)
  
  # first a factor which cuts the data into month chunks
  dat$yearmonth<-as.yearmon(dat$date)
  
  dat$yearmonthf<-factor(dat$yearmonth)
  
  # then find the "week of year" for each day
  dat$week <- as.numeric(format(dat$date,"%W"))
  
  # and now for each monthblock we normalize the week to start at 1 
  dat <- ddply(dat,
               .(yearmonthf),
               transform,
               monthweek=1+week-min(week))
  
  ## Plot
  p <- ggplotly(ggplot(dat, aes(monthweek, weekdayf, fill = date_total)) + 
             geom_tile(colour = "grey") + facet_grid(year~monthf) + scale_fill_gradient(low="lightgreen", high="red") +
             xlab("Week of Month") + ylab("")  + theme_classic() + scale_x_continuous(breaks = seq(1, 6, 1)) + 
             theme(plot.margin = unit(c(1.5,0,1,0), "cm")) +
             theme(strip.text.x = element_text(size = 12, colour = "blue")) +
             labs(fill="Total spent ($)"))  %>% layout(height = 600)

  
  return(p)
}


sankey_diag <- function() {
  con <- dbConnect(
    Postgres(),
    dbname = "d2sjdih8tegcuc",
    host = "ec2-184-72-238-22.compute-1.amazonaws.com",
    port = '5432',
    user = "xkktypszvisogc",
    password = "35b7eb67cab6ea1750e5877d61c3415851d6f1ca9c5782ec0b6bf91ede8acbea",
    sslmode = 'require'
  )
  
  ## Get data from database
  pur_hist <- dbGetQuery(con, "SELECT * FROM grocery")
  
  ## Rename columns
  colnames(pur_hist) <- c("item", "unitprice", "totalprice", "date", "store")
  
  ## Drop blank lines
  pur_hist <-  pur_hist[!(is.na(pur_hist$item) | pur_hist$item==""), ]
  
  ## Extract month from date
  pur_hist$month <- as.numeric(as.POSIXlt(pur_hist$date)$mon+1)
  
  pur_hist$month <- factor(pur_hist$month,
                           levels=as.character(1:12),
                           labels=c("January","February","March","April","May", 
                                    "June","July","August","September","October","November","December"),
                           ordered=TRUE)
  
  ## Group by month and store
  dat <- pur_hist %>% 
    group_by(month, store) %>%
    summarise(month_total=sum(totalprice))
    
  dat <- dat[order(dat$month),]
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(dat$month), 
           as.character(dat$store)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  dat$IDsource <- match(dat$month, nodes$name)-1 
  dat$IDtarget <- match(dat$store, nodes$name)-1
  
  # Set group
  dat$group <- as.factor(c(dat$month))
  
  nodes$group <- as.factor(c("my_unique_group"))
  
  my_color <- 'd3.scaleOrdinal() .domain(["January","February","March","April","May",
                                          "June","July","August","September","October",
                                          "November","December", "my_unique_group"]) .range(["#FFE74C", "#FF5964", "#33E872", "#8A4F7D", "#FFB997",
                                                                                             "#FFE74C", "#FF5964", "#33E872", "#8A4F7D", "#FFB997",
                                                                                             "#FFE74C", "#FF5964", "#808080"])'
  # Make the Network
  p <- sankeyNetwork(Links = dat, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "month_total", NodeID = "name", 
                     colourScale =  my_color, LinkGroup='group', NodeGroup='group',
                     sinksRight=FALSE, iterations = 0, fontSize = 18, units="$", 
                     height=800)
  
  return(p)
}