library(shiny)
library(RPostgres)
library(DBI)
library(knitr)
library(httr)
library(RCurl)
library(xml2)
library(jsonlite)
library(tidyr)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(DTedit)
library(magrittr)
library(shinyjs)
library(V8)
library(plyr)
library(dplyr)
library(shinyBS)

source("functions.R")



jscode <- "shinyjs.refresh = function() { history.go(0); }"

options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

# directory where responses get stored
responsesDir <- file.path(".")

## Connect to podcast
con <- dbConnect(
  Postgres(),
  dbname = "d2sjdih8tegcuc",
  host = "ec2-184-72-238-22.compute-1.amazonaws.com",
  port = '5432',
  user = "xkktypszvisogc",
  password = "35b7eb67cab6ea1750e5877d61c3415851d6f1ca9c5782ec0b6bf91ede8acbea",
  sslmode = 'require'
)


# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# which fields are mandatory
fieldsMandatory <- c("store", "receipt")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

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



sidebar <- dashboardSidebar(sidebarMenu(
  id = "tabs", 
  menuItem(
    HTML("<b>Upload receipt</b>"),
    tabName = "upload",
    icon = icon("receipt")
  ),
  menuItem(HTML("<b>View purchase history</b>"),
           tabName = 'history',
           icon=icon("shopping-cart")
           ),
  menuItem(HTML("<b>Browse the weekly flyers</b>"),
           tabName = 'flyers',
           icon=icon("newspaper")
)), 
textInput(
  "search_box",
  label = HTML("<p style='color:black;'>Search the flyers</p>"),
  width = NULL,
  placeholder = "Enter item or store name"
),
splitLayout(actionButton("search_btn", "Search", class = "btn-primary"),
            actionButton("reset_btn", "Clear", class = "btn-primary"))
)         
           
           



body <- dashboardBody(tabItems(
  tabItem(
    tabName = "upload",
    fluidPage(
      shinyjs::useShinyjs(),
      extendShinyjs(text = jscode),
      shinyjs::inlineCSS(appCSS),
      fluidRow(HTML("<h3>Upload a shopping receipt here to record your purchase history! <a id=\"upload_help\" class=\"action-button\"><i class=\"fa fa-question-circle\"></i></a></h3>"), 
               style='padding-left:2%;'),
      fluidRow(column(
        4,
        div(
          id = "form",
          
          dateInput("date", "Date:", value = Sys.time()),
          
          selectInput(
            "store",
            "Store",
            c("",  "Loblaws", "H-Mart", "Shoppers Drug Mart", "Whole Foods")
          ),
          
          fileInput(
            "receipt",
            "Upload receipt",
            multiple = FALSE,
            accept = c("image/jpeg",
                       "image/png",
                       ".jpeg", ".png", ".jpg")
          ),
          
          actionButton("submit", "Submit", class = "btn-success"),
          
          shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                          div(id = "error",
                              div(
                                br(), tags$b("Error: "), span(id = "error_msg")
                              )))
        ),
        
        shinyjs::hidden(div(
          id = "thankyou_msg",
          h3("Thanks, your response was submitted successfully!"),
          actionLink("submit_another", "Submit another response")
        )),
        style='padding-left:2%;'),
      
      column(8, br(),
             withSpinner(
               DTOutput("contents"), type = 1
             ), style='padding-right: 5%;'))
    )
  ),
  tabItem(tabName = "history",
          fluidPage(
            fluidRow(valueBoxOutput("totalBox")),
            fluidRow(tabBox(tabPanel("Summary", "Test"),
                            tabPanel("View detailed history", DTOutput("purchase_history")),
                            width=12)))),
  tabItem(
    tabName = "flyers",
    fluidPage(
      tags$head(
      tags$style(
        '.card {
         width: auto;
         height: auto;
         clear: both; 
         padding: 10px 10px 10px 10px;
         margin-bottom: 20px;
         /* Add shadows to create the "card" effect */
         /* box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2); */
         transition: 0.3s;
          }
         /* On mouse-over, add a deeper shadow */
         .card:hover {
         box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
         }
         /* Add some padding inside the card container */
         .container {
         width: 100%;
         height: auto;
         padding: 2px 4px;
         background-color: #f7c38b;
         }
        .store-button {
        width: 16%;
        background-color: #ECF0F5;
        border: 0;
        }'
      )
    ),
    fluidRow(
              actionButton("metro_btn", 
                            label = img(src="https://www.metro.ca/images/components/logo/logo--metro.png", width="90%"),
                            class = "btn store-button"),
              actionButton("wholefoods_btn", 
                           label = img(src="https://upload.wikimedia.org/wikipedia/commons/f/f3/Whole_Foods_Market_logo.svg", width="70%"),
                           class = "btn store-button"),
              actionButton("fortinos_btn", 
                           label = img(src="https://www.waterdownbia.ca/wp-content/uploads/2015/05/13598_logo-fortinos.png", width="100%"),
                           class = "btn store-button"),
              actionButton("sobeys_btn", 
                           label = img(src="https://2rt9loawzcmbvlze40mhj9n0-wpengine.netdna-ssl.com/wp-content/themes/sobeys-com/images/Sobeys_Logo_2.svg", width="90%"),
                           class = "btn store-button"),
              actionButton("bulkbarn_btn", 
                           label = img(src="https://www.tamarackcentre.ca/wp-content/uploads/2018/06/bulkbarn.png", width="95%"),
                           class = "btn store-button"),
              actionButton("loblaws_btn", 
                           label = img(src="https://can-cdn.azureedge.net/logos/Loblaws.PNG", width="95%"),
                           class = "btn store-button")
             ),
    fluidRow(withSpinner(htmlOutput("cards"), type = 1),
    shinyjs::hidden(span(id = "search_msg", "Search..."),
                    div(id = "error",
                        div(
                          br(), tags$b("Error: "), span(id = "error_msg")
                        )))))
  )
))



# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPagePlus(title = "A Pinch of This, A Dash of That", 
            dashboardHeader(title = "A Pinch of This, A Dash of That", 
                                    titleWidth = 350),
                    sidebar,
                    body, 
                    skin="purple-light",
                    sidebar_fullCollapse = TRUE,
                    md=FALSE)


## define server
server <- function(input, output, session) {
  ## Define inputs
  script_type <- reactive({
    input$store
  })
  
  receipt_date <- reactive({
    input$date
  })
  
  re1 <- reactive({
    gsub("\\\\", "/",
         input$receipt$datapath)
  })
  
  
  term <- reactive({
    input$search_box
  })
  
  ## Parse OCR results
  get_res <- function (link, input) {
    res <-
      GET(
        sprintf(
          "https://api.ocr.space/parse/imageurl?apikey=2f420f513b88957&url=%s&isTable=true&OCREngine=2",
          link
        )
      )
    
    res_text <- content(res, "text")
    
    res_json <- fromJSON(res_text, flatten = TRUE)
    
    v <- res_json$ParsedResults['ParsedText']$ParsedText
    
    k <- list()
    
    i <- 1
    
    if (script_type() == "Shoppers Drug Mart") {
      for (r in strsplit(v, '\t\r\n')) {
        print(i)
        
        k[[i]] <- strsplit(r, '\t')
        
        print(k[[i]])
        
        i <- i + 1
      }
      
      z <- as.data.frame(do.call(rbind, k[[1]]))
      
      colnames(z) <-
        c('Item', 'UnitPrice', 'TotalPrice')
      
      for (x in c('UnitPrice', 'TotalPrice')) {
        z[[x]] <- sub(" .*", "", z[[x]])
        z[[x]] <- gsub("$", "", z[[x]])
      }
      
      z$date <- as.character(receipt_date())
      
      z$store <- script_type()
      
      z
    } else if (script_type() == "Whole Foods") {
      
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
      
      df$date <- as.character(receipt_date())
      
      df$store <- script_type()
      
      df
    } else if (script_type() == "H-Mart") {
      
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
      
      z$date <- as.character(receipt_date())
      
      z$store <- script_type()
      
      z
      
    }
    
  }
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    # data <- sapply(fieldsAll, function(x) input[[x]])
    # #data <- c(data, timestamp = epochTime())
    # data <- t(data)
    # data
    
    data <- get_res(imgur_upload(re1())[1])
    saveData(data)
    data
  })
  
  
  
  output$contents <- DT::renderDataTable({
    datatable(if (is.null(input$receipt$datapath)) {
      df <- data.frame()
    } else {
      df <- as.data.frame(get_res(imgur_upload(re1())[1]))
      
      df
    }, editable = T)
  })
  
  
  proxy = dataTableProxy('contents')
  
  observeEvent(input$contents_cell_edit, {
    df <- read.csv('res.csv')
    df <-
      convert.dtype(df,
                    c(
                      "character",
                      "character",
                      "character",
                      "character",
                      "character"
                    ))
    info = input$contents_cell_edit
    
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    df[i, j] = DT::coerceValue(v, df[i, j])
    
    replaceData(proxy, df, resetPaging = FALSE) # important
    
    saveData(df)
    
  })
  
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      df <- read.csv('res.csv')
      #saveData(formData())
      dbWriteTable(con, "apptest", df, append = TRUE)
      shinyjs::reset("form")
      shinyjs::hide("form")
      output$contents <- renderText({
        ' '
      })
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  
  
  ## submit another response
  
  output$contents <- DT::renderDataTable({
    datatable(if (is.null(input$receipt$datapath)) {
      df <- data.frame()
      df
    } else {
      df <- as.data.frame(formData())
      df
    }, editable = T)
  })
  
  
  observeEvent(input$submit_another, {
    js$refresh()
    
    
  })
  
  pur_hist <- dbGetQuery(con, "SELECT * FROM apptest ORDER BY date DESC")
  
  output$purchase_history <- DT::renderDataTable(pur_hist)
  
  output$totalBox <- renderValueBox({
    valueBox(
      sum(pur_hist$TotalPrice),
      "Spent",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  ## No search term
  observe({
    if (input$tabs == "flyers" & input$search_box == "") {
      output$cards <-  renderUI({HTML("<br><br><br><center><h1>Select one of the stores above for one-click access to the weekly flyer. <br><br>
                                      Or, search for a particular item or store of your choice.</h1></center>")})
    }
  })
  
  
  observeEvent(input$search_btn, {
    shinyjs::hide('metro_btn')
    shinyjs::hide("wholefoods_btn")
    shinyjs::hide("loblaws_btn")
    shinyjs::hide("bulkbarn_btn")
    shinyjs::hide("sobeys_btn")
    shinyjs::hide("fortinos_btn")
    shinyjs::show("cards")
    
  
    ## Any search term
    search_term <- gsub(" ", "+", input$search_box)
    
    res_json <- get_flyer(search_term)
    
    if (length(res_json$items) ==0) {
      output$cards <- renderText(HTML("<br><br><center><h2>Sorry, your query did not return any results.</h2></center>"))
    } else {
      
      filtered_data <- parse_flyer(res_json)
      
      updateTabItems(session, "tabs", "flyers")
      
      output$cards <- renderUI({
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
  }
})
  
  observeEvent(input$reset_btn, {
    shinyjs::reset("search_box")
    
    #output$cards <- renderText("Check out these stores.")
    
    shinyjs::show('metro_btn')
    shinyjs::show("wholefoods_btn")
    shinyjs::show("loblaws_btn")
    shinyjs::show("bulkbarn_btn")
    shinyjs::show("sobeys_btn")
    shinyjs::show("fortinos_btn")
    
    updateTabItems(session, "tabs", "flyers")
    
  })


  
  ## By store
  observeEvent(input$metro_btn | input$wholefoods_btn | input$fortinos_btn | input$sobeys_btn | input$bulkbarn_btn | input$loblaws_btn,
               {
                  if (input$metro_btn) {
                    search_term = "metro"
                  } else if (input$wholefoods_btn) {
                    search_term = "whole+foods" 
                  }  else if (input$fortinos_btn) {
                    search_term = "fortinos" 
                  } else if (input$sobeys_btn) {
                    search_term = "sobeys" 
                  } else if (input$bulkbarn_btn) {
                    search_term = "bulk+barn"
                  } else {
                    search_term = "loblaws"
                  }
                  
                  res_json <- get_flyer(search_term)
                  
                  if (length(res_json$items) == 0) {
                    output$cards <-  renderText(HTML("<br><br><center><h2>Oops, the flyer is currently unavailable!</h2></center>"))
              
                  } else {
                    filtered_data <- parse_flyer(res_json)
                    
                    updateTabItems(session, "tabs", "flyers")
                    
                    output$cards <- renderUI({
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
                  }
                })
  
  
  observeEvent(input$upload_help, {
    showModal(modalDialog(
      title = HTML("<h3>How to prepare an image of the receipt:</h3>"),
      HTML("<h4>1. Take a picture of the receipt.</h4>"),
      HTML("<h4>2. Crop the picture so that it only contains the purchased items:</h4>"),
      HTML('<center><img src="https://i.imgur.com/oN67aUk.jpg" width=60%></center>'), br(),
      HTML("<h4>3. Save and upload this image on this page, along with the purchase date and store name.</h4>"),
      easyClose = TRUE
    ))
  })
   

}



# Preview the UI in the console
shinyApp(ui = ui, server = server)