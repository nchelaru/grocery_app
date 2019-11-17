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
library(shinyWidgets)
library(icon)
library(data.table)
library(rhandsontable)
library(slickR)
library(svglite)
library(tippy)

source("functions.R")



jscode <- "shinyjs.refresh = function() { history.go(0); }"

options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

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

## Empty dataframe
empty_df = data.frame("Item" = character(3),
                      "UnitPrice"= double(3),
                      "TotalPrice"= double(3),
                      "date" = Sys.Date(),
                      "store" = character(3),
                      stringsAsFactors = FALSE)

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "



## Define UI
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "tabs", 
    menuItem(
      HTML("<font size='4'>Upload receipt</font>"),
      tabName = "upload",
      icon = icon("receipt")
    ),
    menuItem(
      HTML("<font size='4'>View shopping history</font>"),
      tabName = 'history',
      icon=icon("shopping-cart")
    ),
    menuItem(
      HTML("<font size='4'>Browse sales flyers</font>"),
      tabName = 'flyers',
      icon=icon("newspaper")
    )),
  textInput(
    "search_box",
    label = h4("Search the flyers:"),
    width = NULL,
    placeholder = "Enter item or store name"
  ),
  splitLayout(
    actionButton("search_btn", HTML("<font size='4'>Search</font>"), class = "btn-primary"),
    actionButton("reset_btn", HTML("<font size='4'>Clear</font>"), class = "btn-primary")),
  uiOutput('store_list')
)         


body <- dashboardBody(tabItems(
  tabItem(
    tabName = "upload",
    fluidPage(
      shinyjs::useShinyjs(),
      extendShinyjs(text = jscode),
      shinyjs::inlineCSS(appCSS),
      tags$head(tags$style(
        type="text/css",
        "#success_img img {max-width: 100%; width: auto; height: auto}"
      )),
      fluidRow(column(4,   
                      br(),
        flipBox(id = 1, width = 12, 
                front_btn_text = "Or, manually enter purchases",
                back_btn_text = "Upload a receipt",
                main_img = "https://image.flaticon.com/icons/svg/138/138360.svg",
                header_img = "https://images.unsplash.com/photo-1557821552-17105176677c?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=2089&q=80",
                front_title = NULL,
                back_title = NULL,
                div(id = "form",
                    dateInput(
                      width='100%',
                      "date", 
                      HTML("<font size='4'>1. Select date of purchase:</font>"), 
                      value = Sys.time()
                    ),
                    selectInput(
                      width='100%',
                      "store",
                      HTML("<font size='4'>2. Select store</font>"),
                      c("", "Bloor Street Market", "Bulk Barn", "Loblaws", "Galleria", "H-Mart", "Metro", 
                        "Shoppers Drug Mart", "Sobeys", "T&T Supermarket", "Whole Foods")
                    ),
                    fileInput(
                      width='100%',
                      "receipt",
                      HTML("<font size='4'>3. Upload image of receipt</font>"),
                      multiple = FALSE,
                      accept = c("image/jpeg", "image/png", ".jpeg", ".png", ".jpg")
                    ),
                    HTML("<font size='4'><b>4. Confirm that all information is correct</b><br>
                         Edit the table as needed (double click on cell for more options).</font>"),
                    br(), hr(), br(),
                    actionButton("submit", "Upload receipt", class = "btn-warning"),
                    br(), br(), 
                    shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                                    div(id = "error",
                                        div(
                                          br(), tags$b("Error: "), span(id = "error_msg")
                                        ))),

                    style='padding: 20px 50px 20px 50px;'
                    ),
        
        shinyjs::hidden(div(
          id = "thankyou_msg",
          br(),
          actionLink("submit_another", 
                     HTML("<h1><i class='fas fa-clipboard-check'></i><br></h1><h3>Submit another?</h3>")),
          br()
        )),
         
        back_content = div(shinyjs::hidden(div(
                              id = "thankyou_msg_back",
                              actionLink("submit_another_back", 
                                         HTML("<h1><i class='fas fa-clipboard-check'></i><br></h1><h3>Submit another?</h3>")),
                              br())),
                           div(id = 'back_content',
                               HTML('<p style="float:left";><i>Right click the table to insert or delete rows.</i></p>'),
                               br(), br(),
                               rHandsontableOutput("manual_entry"), 
                               br(),
                               actionButton("manual_submit", "Enter purchases", class = "btn-warning")
                             )
                           ))),
        
        column(8, br(), 
               box(
                   br(), br(),
                   slickROutput("slick_output", width='100%', height='550px'),
                   rHandsontableOutput("contents") %>% withSpinner(type = 1),
                   br(),
                   htmlOutput("success_img"),
                   title = NULL,
                   width = 12,
                   height = 700,
                   solidHeader = TRUE,
                   status = 'danger',
                   collapsible = FALSE)
               ))
    )
  ),
  tabItem(tabName = "history",
          fluidPage(
            br(),
            fluidRow(valueBoxOutput("totalBox"), 
                     valueBoxOutput("totalBox2"), 
                     valueBoxOutput("totalBox3")),
            fluidRow(tabBox(tabPanel("By date", "heat map by date"),
                            tabPanel("By store", "knob and "),
                            tabPanel("Detailed history", DTOutput("purchase_history")),
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
        actionButton("citymarket_btn", 
                     label = img(src="https://i.ibb.co/ryJ4TNy/city-market.png", width="95%"),
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
                        skin="purple",
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
      res <- shoppers_parse(v, k, i, receipt_date(), script_type())
    } else if (script_type() == "Whole Foods") {
      res <- wf_parse(v, k, i, receipt_date(), script_type())
    } else if (script_type() == "H-Mart") {
      res <- hmart_parse(v, k, i, receipt_date(), script_type())
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
  

  receipt_data <-  reactive({
    as.data.frame(get_res(imgur_upload(re1())[1]))
  })
    

  observe({
    if (is.null(input$receipt$datapath)) {
      output$contents <- renderRHandsontable({
        data.frame(matrix(ncol=0, nrow=0))
      })
      
      img_list <- c("https://i.ibb.co/SrQYjm9/step-1.png", 
                    "https://i.ibb.co/XSBsRp7/step-2.png",
                    "https://i.ibb.co/gVGdcpS/step-3.png",
                    "https://i.ibb.co/L1HB2Xc/step-4.png")
      
      output$slick_output <- renderSlickR({
        x <- slickR(img_list,
                    slideId = 'myslick',
                    height = 250,
                    width = '90%',
                    slickOpts = list(dots = TRUE, autoplay = TRUE))
      })
      
      # Observe the active slick
      # We will store this information in a new reactive environment
      active_slick <- shiny::reactiveValues()
      
      shiny::observeEvent(input$slick_output_current,{
        
        clicked_slide    <- input$slick_output_current$.clicked
        relative_clicked <- input$slick_output_current$.relative_clicked
        center_slide     <- input$slick_output_current$.center
        total_slide      <- input$slick_output_current$.total
        active_slide     <- input$slick_output_current$.slide
        
        if(!is.null(clicked_slide)){
          active_slick$clicked_slide    <- clicked_slide
          active_slick$center_slide     <- center_slide
          active_slick$relative_clicked <- relative_clicked
          active_slick$total_slide      <- total_slide
          active_slick$active_slide     <- active_slide
        }
      })
    } else {
      shinyjs::hide('success_img')
      shinyjs::hide('slick_output')
      shinyjs::show('contents')
      
      output$contents <- renderRHandsontable({
        rhandsontable(receipt_data(), stretchH = "all", 
                      colHeaders = c('Item', 'Unit Price', 'Total Price', 'Purchase date', 'Store')) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)  %>%
          hot_cols(columnSorting = TRUE,  allowInvalid = TRUE, strict=FALSE)
      })

    }
  })
  

  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    shinyjs::hide("contents")
    shinyjs::hide("slick_output")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      #df <- read.csv('res.csv')
      df <- hot_to_r(input$contents)
      #saveData(formData())
      dbWriteTable(con, "apptest", df, append = TRUE)
      shinyjs::reset("form")
      shinyjs::hide("form")
      output$contents <- renderText({
        ' '
      })
      shinyjs::show("thankyou_msg")
      shinyjs::hide('btn-1')
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
    
    ## Show success image
    src = "https://i.ibb.co/9rgXLVH/pluto-welcome.png"
    output$success_img <- renderText({c('<center><h2>Purchases added!</h2><br><img src="',src,'"></center>')})
    
    shinyjs::show('success_img')
  })
  
  
  
  ## submit another receipt
  observeEvent(input$submit_another, {
    js$refresh()
  })
  
  
  ## Switch to manual entry
  # observeEvent(input$manual_sub_another, {
  #   shinyjs::hide('thankyou_msg')
  #   shinyjs::hide('success_img')
  #   shinyjs::show('slick_output')
  #   shinyjs::show('manual_entry')
  #   shinyjs::show('manual_submit')
  # })
  
  
  ## Manual entry
  tb_data <- reactiveValues(values=empty_df)
  
  output$manual_entry <- renderRHandsontable({
    rhandsontable(tb_data$values, stretchH = "all", rowHeaderWidth = 20,
                  colHeaders = c('Item', 'Unit price', 'Total price', 'Purchased', 'Store')) 
  })
  
  observeEvent(input$manual_entry, {
    tb_data$values <- hot_to_r(input$manual_entry)
  })
  
  
  # When the manual submit button is clicked, submit the response
  observeEvent(input$manual_submit, {
    # User-experience stuff
    shinyjs::disable("manual_submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    shinyjs::hide("contents")
    shinyjs::hide("slick_output")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      #df <- read.csv('res.csv')
      df <- hot_to_r(input$manual_entry)
      #saveData(formData())
      dbWriteTable(con, "apptest", df, append = TRUE)
      shinyjs::reset("manual_entry")
      shinyjs::hide("manual_entry")
      shinyjs::hide('manual_submit')
      output$contents <- renderText({
        ' '
      })
      shinyjs::hide('back_content')
      shinyjs::show("thankyou_msg_back")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("manual_submit")
      shinyjs::hide("submit_msg")
    })
    
    ## Show success image
    src = "https://i.ibb.co/9rgXLVH/pluto-welcome.png"
    output$success_img <- renderText({c('<center><h2>Purchases added!</h2><br><img src="',src,'"></center>')})
  
    shinyjs::show('success_img')
  })
  
  ## Change to receipt upload after manual entry
  observeEvent(input$submit_another_back, {
    js$refresh()
  })
  

  
  ## Purchase history
  pur_hist <- dbGetQuery(con, "SELECT * FROM apptest ORDER BY date DESC")
  
  colnames(pur_hist) <- c("Item", "Unit price", "Total price", "Purchase date", "Store")
  
  output$purchase_history <- DT::renderDataTable(pur_hist)
  
  output$totalBox <- renderValueBox({
    valueBox(
      sum(pur_hist$TotalPrice),
      "Spent this month",
      icon = icon("usd", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$totalBox2 <- renderValueBox({
    valueBox(
      "Products",
      "Relative to last month",
      icon = icon("time", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$totalBox3 <- renderValueBox({
    valueBox(
      "Items",
      "Median purchase size",
      icon = icon("shopping-cart", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  
  
  ## No search term
  observe({
    if (input$tabs == "flyers" & input$search_box == "") {
      output$cards <-
        renderUI({
          HTML(
            "<br><br><br><center><h1>Select one of the stores above for one-click access to the weekly flyer. <br><br>
                                      Or, search for a particular item or store of your choice.</h1></center>"
          )
        })
    }
  })
  
  
  
  ## Any search term
  observeEvent(c(req(input$search_box != "", input$search_btn)), {
    shinyjs::hide('cards')
    shinyjs::hide('metro_btn')
    shinyjs::hide("wholefoods_btn")
    shinyjs::hide("loblaws_btn")
    shinyjs::hide("citymarket_btn")
    shinyjs::hide("sobeys_btn")
    shinyjs::hide("fortinos_btn")
    shinyjs::show("cards")
    
    search_term <- gsub(" ", "+", input$search_box)
    
    res_json <- get_flyer(search_term)
    
    output$store_list <-
      renderUI(
        prettyRadioButtons(
          "select_store",
          h4("See items at:"),
          choices = unique_stores(input$search_box),
          selected = NULL,
          inline = FALSE,
          width = NULL
        )
      )
    
    
    
    if (length(res_json$items) == 0) {
      output$cards <-
        renderText(
          HTML("<br><br><center><h2>Sorry, your query did not return any results.</h2></center>")
        )
      
      shinyjs::hide('store_list')
    } else {
      observeEvent(input$select_store, {
        output$cards <-
          create_grid_item(session, search_term, input$select_store)
      })
      
      shinyjs::show('store_list')
    }
    
    #shinyjs::show('store_list')
  }) 
   
 
  
  ## Reset search box
  observeEvent(input$reset_btn, {
    shinyjs::reset("search_box")
 
    shinyjs::show('metro_btn')
    shinyjs::show("wholefoods_btn")
    shinyjs::show("loblaws_btn")
    shinyjs::show("citymarket_btn")
    shinyjs::show("sobeys_btn")
    shinyjs::show("fortinos_btn")
    shinyjs::hide('store_list')
    
    updateTabItems(session, "tabs", "flyers")
    
  })


  
  ## Display flyer by store
  observeEvent(input$wholefoods_btn, {
    output$cards <- create_grid_store(session, "whole+foods")
  })
  
  observeEvent(input$metro_btn, {
    output$cards <- create_grid_store(session, "metro")
  })
  
  observeEvent(input$fortinos_btn, {
    output$cards <-create_grid_store(session, "fortinos")
  })
  
  observeEvent(input$sobeys_btn, {
    output$cards <- create_grid_store(session, "sobeys")
  })
  
  observeEvent(input$citymarket_btn, {
    output$cards <- create_grid_store(session, "independent+city+market")
  })
  
  observeEvent(input$loblaws_btn, {
    output$cards <- create_grid_store(session, "loblaws")
  })
}



# Preview the UI in the console
shinyApp(ui = ui, server = server)