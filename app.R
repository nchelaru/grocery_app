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
library(openair)
library(shinyhelper)
library(reactable)

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



# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "


about_msg <- HTML('<div style="font-size:20px; margin: 0px 15px 15px 15px;"><p>
                  <br>
                  This little app is a fun programming exercise that combines my love for exploring the ever-expanding Shiny ecosystem
                  with my so-uncool enthusiasm for good grocery sales. <i class="fas fa-grin-squint"></i>
                  
                  <br><br>
                  
                  This project is very much in active development, so please check back regularly to see what other neat features have been added. 
                  If you have suggestions or questions, I would love to hear from you through any of the channels listed below.
                  
                  <br><br>
                  
                  Along with Mihai Chelaru, I am the co-curator of <a href="https://www.intelligencerefinery.io"><b>Intelligence Refinery</b></a>,
                  a knowledge repository for all things data science and software development. We try to distill all that we learn and experience as 
                  working data scientists into useful nuggets that can be easily referenced by our future selves and whoever else may find them helpful. 
                  Flip to the other side of this card to find links to our other works.
                  
                  <br><br>
                  
                  Hope that you have enjoyed your time here!
                  
                  <br>
                  </p></div>')
                  
contact_footer <- HTML('<div style="font-size:20px; margin: 0px 15px 15px 15px;">
                      <br>
                      <center><a href="mailto:nancy.chelaru@gmail.com"><i class="fas fa-envelope" style="padding:10px;"></i></a>
                      <a href="https://www.intelligencerefinery.io/contact/"><i class="fas fa-globe" style="padding:10px;"></i></i></a>
                      <a href="https://github.com/nchelaru?tab=repositories"><i class="fab fa-github-alt" style="padding:10px;"></i></a>
                      <a href="https://twitter.com/n_chelaru"><i class="fab fa-twitter" style="padding:10px;"></i></a></center>
    
                      <br><hr><br><br><br>
                      </div>')

iframe_test <- HTML('<iframe src="https://raindrop.io/collection/8841871" 
                    style="border:0px #ffffff none;" name="myiFrame" scrolling="no" 
                    frameborder="1" marginheight="0px" marginwidth="0px" height="750px" 
                    width="100%" allowfullscreen></iframe>')

## Define UI
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "tabs", 
    menuItem(
      HTML("<font size='4'>Enter purchases</font>"),
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
    ),
    menuItem(
      HTML("<font size='4'>About this app</font>"),
      tabName = 'about',
      icon=icon("question")
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
        "#success_img img {max-width: 100%; width: auto; height: auto}
        #slick_slide img {max-width: 100%; width: auto; height: auto}
       #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
       #modal1 .modal-header {background-color: #DD9AC2; border-top-left-radius: 6px; border-top-right-radius: 6px}
        "
      )),
      fluidRow(column(5,   
        flipBox(id = 1, width = 12,
                front_btn_text = HTML("<font size='4'>Manually enter purchases instead</font>"),
                back_btn_text = HTML("<font size='4'>Upload a receipt instead</font>"),
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
                      div(HTML("<font size='4'>2. Select store  </font>"), 
                          HTML("<font size='2' color='grey'><i>(More coming soon!)</i></font>")),
                      c("", "Loblaws", "H-Mart", 'Shoppers Drug Mart', "Whole Foods"),
                      # c("", "Bloor Street Market", "Bulk Barn", "Loblaws", "Galleria", "H-Mart", "Metro", 
                      #   "Shoppers Drug Mart", "Sobeys", "T&T Supermarket", "Whole Foods")
                    ),
                    fileInput(
                      width='100%',
                      "receipt",
                      div(HTML("<font size='4'>3. Upload image of receipt   </font>"), 
                          actionLink("receipt_modal", HTML("<i class='far fa-question-circle'></i>"))),
                      HTML('<i class="fas fa-receipt"></i>'),
                      multiple = FALSE,
                      accept = c("image/jpeg", "image/png", ".jpeg", ".png", ".jpg")
                    ),
                    HTML("<font size='4'><b>4. Confirm that all information is correct</b><br>
                         Edit the table as needed<br>(right click on table for more editing options)</font>"),
                    br(),
                    HTML("<br><br><font size='4'><b>5. Save to database!</b></font><br><br>"),
                    actionButton("submit", HTML("<font size='4'>Submit</font>"), class = "btn-warning"),
                    br(), br(), hr(), br(), br(),
                    shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                                    div(id = "error",
                                        div(
                                          br(), tags$b("Error: "), span(id = "error_msg")
                                        ))),

                    style='padding: 20px 50px 20px 50px;'
                    ),
        
        shinyjs::hidden(div(
          id = "thankyou_msg",
          br(), br(), br(),
          actionLink("submit_another", 
                     HTML("<h1><i class='fas fa-clipboard-check'></i><br></h1><h3>Submit another</h3>")),
          br(), hr(), br(), br(),
          actionLink("switch_history", 
                     HTML("<h1><i class='fas fa-eye'></i><br></h1><h3>View submitted purchases</h3>")),
          br(), hr(), br(), br()
        )),
         
        back_content = div(shinyjs::hidden(div(
                              id = "thankyou_msg_back",
                              br(), br(),
                              actionLink("submit_another_back", 
                                         HTML("<h1><i class='fas fa-clipboard-check'></i><br></h1><h3>Submit another</h3>")),
                              br(), hr(), br(), br(),
                              actionLink("switch_history_back", 
                                         HTML("<h1><i class='fas fa-eye'></i><br></h1><h3>View submitted purchases</h3>")),
                              br(), hr(), br(), br())),
                           div(id = 'back_content',
                               actionButton("num_rows", HTML("<font size='4'>Select the number of items purchased</font>"), class = "btn-warning"),
                               br(),
                               shinyjs::hidden(div(id = "knob", align='center',
                                                   knobInput(inputId="manual_knob", label="", value = 1, 
                                                   min = 1, max = 15, step = 1,
                                                   angleOffset = 0, angleArc = 360, cursor = FALSE,
                                                   thickness = NULL, lineCap = "round",
                                                   displayInput = TRUE, displayPrevious = TRUE,
                                                   rotation = "clockwise", fgColor =NULL,
                                                   inputColor = "#428BCA", bgColor = NULL, readOnly = FALSE, 
                                                   width = NULL, height = NULL, immediate = TRUE))),
                               shinyjs::hidden(div(id = "manual_table", style='margin:20px; font-size:17px;',
                                                     HTML('<p style="float:left";><i>If needed, right click the table to insert more blank rows.</i></p>'),
                                                     br(), br(), 
                                                     rHandsontableOutput("manual_entry"), 
                                                     br(), 
                                                     splitLayout(
                                                       actionButton("manual_submit", HTML("<font size='4'>Save to database!</font>"), class = "btn-success"), 
                                                       actionButton("manual_clear", HTML("<font size='4'>Cancel</font>"), class = "btn-danger")),
                                                     br()
                                                   )),
                               
                             )
                           ))),
        
        column(7, 
               box(
                   br(),
                   div(slickROutput("slick_output", width='100%', height='550px'),
                       style='margin:30px'),
                   div(rHandsontableOutput("contents") %>% withSpinner(type = 1), 
                       style='margin:30px;font-size:17px;'),
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
            fluidRow(valueBoxOutput("totalBox_thismonth"), 
                     valueBoxOutput("totalBox_topstore"),
                     valueBoxOutput("totalBox_lastmonth")), 
            fluidRow(tabBox(tabPanel("Detailed history", 
                                     #withSpinner(DTOutput("purchase_history"), type=2), 
                                     reactableOutput("purchase_history"),
                                     style='margin: 0px 30px 0px 30px'),
                            tabPanel("Summary by date", 
                                      br(), 
                                     withSpinner(plotlyOutput('dat_heatmap', height='150%'), type=2),
                                     style='margin:30px;'),
                            tabPanel("Summary by store", 
                                     br(),
                                     withSpinner(sankeyNetworkOutput('sankey_plot', height='700px', width='110%'), type=2),
                                     style='margin:30px;'),
                            width=12, height=800),
                     ))),
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
  ),
  tabItem(tabName = "about",
          fluidPage(
            fluidRow(
              column(width=5, 
                     box(
                       title = "Changelog and roadmap",
                       solidHeader = TRUE,
                       status = 'info',
                       width = 12,
                       timelineBlock(
                         timelineEnd(color = "danger"),
                         timelineLabel('2019 November', color = "maroon"),
                         timelineItem(
                           title = HTML("<font size='4'><b>Available store options for receipt OCR</b></font>"),
                           icon = "store",
                           color = 'olive',
                           HTML("<font size='3'>Now you can upload receipts from <b>H-Mart</b>, <b>Loblaws</b>, 
                                <b>Shoppers Drug Mart</b>, and <b>Whole Foods</b> in a snap!</font>"),
                           border = TRUE,
                         ),
                         timelineItem(
                           title = HTML("<font size='4'><b>More goodies!</b></font>"),
                           icon = "chart-line",
                           color = 'teal',
                           HTML("<font size='3'>Now can filter grocery flyer search results by store. 
                           Visualize shopping trends using heatmap and Sankey diagram.</font>"),
                           border = TRUE,
                         ),
                         timelineLabel('2019 October', color = "orange"),
                         timelineItem(
                           title = HTML("<font size='4'><b>New feature!</b></font>"),
                           icon = "keyboard",
                           color = 'purple',
                           HTML("<font size='3'>Add functionality to manually enter purchases and save to database.</font>"),
                           border = TRUE,
                         ),
                         timelineLabel('2019 September', color = "green"),
                         timelineItem(
                           title = HTML("<font size='4'><b>Up and running!</b></font>"),
                           icon = "gears",
                           color = "maroon",
                           HTML("<font size='3'>We are live! A fun app for browsing and searching grocery flyers in Toronto.</font>")
                         ),
                         timelineStart(color = "gray")
                       )
                     )),
              column(
                width=7, 
                flipBox(id=2, width=12,  subtitle="Creator", main_img= 'https://octodex.github.com/images/andycat.jpg',
                    header_img =  "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
                    front_title = "Hi, I'm Nancy Chelaru!", back_title = NULL,
                    front_btn_text = "Other projects at Intelligence Refinery", back_btn_text = "Back",
                    div(about_msg, 
                        contact_footer
                        ),
                    back_content = iframe_test)
              
            )
          )
          )
))
)



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
  
  observeEvent(input$receipt_modal, {
    showModal(tags$div(id="modal1", modalDialog(
      easyClose = TRUE,
      title = HTML("<h3>A friend reminder!  <i class='fas fa-smile-beam'></i></h3>"),
      HTML("<h4>Please crop the image around <b>only</b> the purchased items before uploading:</h4><br>"),
      HTML("<center><img src='receipt_example.jpg' height=400px></center>")
    )))
    
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
                      colHeaders = c('Item', 'Unit price ($/ea or /kg)', 'Total ($)', 'Purchase date', 'Store')) %>%
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
      colnames(df) <- c("item", "unitprice", "totalprice", "date", "store")
      df <-  df[!(is.na(df$item) | df$item==""), ]
      dbWriteTable(con, "grocery", df, append = TRUE)
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
  
  ## View history
  observeEvent(input$switch_history, {
    updateTabsetPanel(session, "tabs",
                      selected = "history")
  })

  
  ## Manual entry
  observeEvent(input$num_rows, {
    shinyjs::hide('btn-10001')
    shinyjs::show('knob')
    shinyjs::show('manual_table')
  })
  
  
  ## Get input from knob
  gen_empty_df <- function(r_num) {
    empty_df = data.frame("store" = character(r_num), 
                          "Item" = character(r_num),
                          "UnitPrice"= double(r_num),
                          "TotalPrice"= double(r_num),
                          "date" = Sys.Date(),
                          stringsAsFactors = FALSE)
    return(empty_df)
  }
  
  
  tb_data <- eventReactive(input$manual_knob, {
    df <- gen_empty_df(input$manual_knob)
    return(df)
  })
  
  
  output$manual_entry <- renderRHandsontable({
    stores_list <- c("Coppa's Fresh Market", 'Food Basics', 'Loblaws', 'Metro', 'Sobeys', "Fortinos", 
                "FreshCo", 'Walmart', 'T&T Supermarket', "Galleria Supermarket", "H-Mart ", "No Frills", 
                "Shoppers Drug Mart", 'BTrust', 'LCBO', 'Independent City Market', "Whole Foods", 'Canadian Tire')
    
    rhandsontable(tb_data(), stretchH = "all", rowHeaderWidth = 35,
                  colHeaders = c('Store', 'Item', 'Unit price ($/ea or /kg)', 'Total ($)', 'Date')) %>%
      hot_col(col = "Store", type = "dropdown", source = stores_list, colWidths = 150)
  })

  
  
  observeEvent(input$manual_entry, {
    tb_data <- hot_to_r(input$manual_entry)
  })
  
  
  # When the manual submit button is clicked, submit the response
  observeEvent(input$manual_submit, {
    # User-experience stuff
    shinyjs::disable("manual_submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    shinyjs::hide("contents")
    shinyjs::hide("slick_output")
    shinyjs::hide('back_content')
    shinyjs::show("thankyou_msg_back")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      #df <- read.csv('res.csv')
      df <- hot_to_r(input$manual_entry)
      colnames(df) <- c("item", "unitprice", "totalprice", "date", "store")
      dbWriteTable(con, "grocery", df, append = TRUE)
      shinyjs::reset("manual_entry")
      shinyjs::hide("manual_entry")
      shinyjs::hide('manual_submit')
      output$contents <- renderText({
        ' '
      })
      
      
      #shinyjs::hide('knob')
      #shinyjs::hide('manual_table')
      
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
  
  
  
  ## Cancel button
  observeEvent(input$manual_clear, {
    js$refresh()
  })
  
  
  
  ## Change to receipt upload after manual entry
  observeEvent(input$submit_another_back, {
    js$refresh()
  })
  

  ## To switch to purchase history tab
  observeEvent(input$switch_history_back, {
    updateTabsetPanel(session, "tabs",
                      selected = "history")
  })
  
  
  ## Purchase history
  observe({
    if (req(input$tabs) == "history") {
      pur_hist <- dbGetQuery(con, "SELECT * FROM grocery ORDER BY Date DESC")
      
      colnames(pur_hist) <- c("item", "unitprice", "totalprice", "date", "store")
      
      pur_hist <- pur_hist[c("date", 'item', 'store', 'unitprice', 'totalprice')]
      
      pur_hist <-  pur_hist[!(is.na(pur_hist$item) | pur_hist$item==""), ]
      
      # output$purchase_history <- DT::renderDataTable(datatable(pur_hist, 
      #                                                          colnames=c("Item", "Unit price ($/ea or /kg)", "Total price ($)", 
      #                                                                     "Purchase date", "Store")))
      
      orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)
    
      
      output$purchase_history <- renderReactable({
                                                      reactable(pur_hist, 
                                                                searchable = TRUE,
                                                                filterable = TRUE,
                                                                showPageSizeOptions = TRUE, 
                                                                pageSizeOptions = c(5, 10, 15),
                                                                defaultPageSize = 15,
                                                                defaultSorted = list(date='desc'),
                                                                defaultColDef = colDef(
                                                                  footerStyle = list(fontWeight = "bold"),
                                                                  style = JS("function(rowInfo, colInfo, state) {
                                                                        // Highlight sorted columns
                                                                        for (var i = 0; i < state.sorted.length; i++) {
                                                                          if (state.sorted[i].id === colInfo.id) {
                                                                            return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                          }
                                                                        }
                                                                      }")),
                                                                columns = list(
                                                                  item = colDef(name='Item'),
                                                                  store = colDef(name='Store'),
                                                                  date = colDef(name='Date', html=TRUE,
                                                                                footer = "<b>Total</b>"),
                                                                  unitprice = colDef(
                                                                    name='Unit price ($)',
                                                                    format = colFormat(currency = "USD", separators = TRUE, locales = "en-US"),
                                                                    html = TRUE, align = "left", header = JS("
                                                                      function(colInfo) {
                                                                        return colInfo.column.name + '<div style=\"color: #999\">/ea or /kg</div>'
                                                                      }
                                                                    ")
                                                                                     ),
                                                                  totalprice = colDef(
                                                                    name='Total price ($)',
                                                                    html=TRUE,
                                                                    format = colFormat(currency = "USD", separators = TRUE, locales = "en-US"),
                                                                    footer = JS("function(colInfo) {
                                                                              var values = colInfo.data.map(function(row) {
                                                                                return row[colInfo.column.id]
                                                                              })
                                                                              var total = values.reduce(function(a, b) { return a + b }, 0)
                                                                              return '<b>$' + total.toFixed(2) + '</b>'
                                                                            }"),
                                                                    style = function(value) {
                                                                      normalized <- (value - min(pur_hist$totalprice)) / (max(pur_hist$totalprice) - min(pur_hist$totalprice))
                                                                      color <- orange_pal(normalized)
                                                                      list(background = color)
                                                                    }
                                                                  )
                                                                  
                                                                ))
                                                    })
      
      
      output$totalBox_thismonth <- renderValueBox({
        pur_hist <- pur_hist %>%
          separate('date', sep="-", into = c("year", "month", "day"))
        
        curr_month <- format(Sys.Date(), "%m")
        
        pur_hist_curr_month <- pur_hist[pur_hist$month == curr_month, ]
        
        valueBox(
          value=paste0("$", sum(pur_hist_curr_month['totalprice'])),
          "Spent this month",
          icon = icon("usd", lib = "glyphicon"),
          color = "red"
        )
      })
      
      output$totalBox_lastmonth <- renderValueBox({
        pur_hist <- pur_hist %>%
          separate('date', sep="-", into = c("year", "month", "day"))
        
        curr_month <- format(Sys.Date(), "%m")
        
        last_month = as.numeric(curr_month) - 1
        
        pur_hist_last_month <- pur_hist[pur_hist$month == last_month, ]
        
        valueBox(
          value=paste0("$", sum(pur_hist_last_month['totalprice'])),
          "Spent last month",
          icon = icon("time", lib = "glyphicon"),
          color = "orange"
        )
      })
      
      output$totalBox_topstore <- renderValueBox({
        pur_hist <- pur_hist %>%
          separate('date', sep="-", into = c("year", "month", "day"))
        
        curr_month <- format(Sys.Date(), "%m")
        
        pur_hist_curr_month <- pur_hist[pur_hist$month == curr_month, ]
        
        df <- pur_hist_curr_month %>% 
          group_by(store) %>%
          summarise(store_total=sum(totalprice))
        
        store_max <- df$store[which.max(df$store_total)]
        
        valueBox(
          store_max,
          "Top store this month",
          icon = icon("shopping-cart", lib = "glyphicon"),
          color = "green"
        )
      })
      
      output$dat_heatmap <- renderPlotly({
        date_heatmap()
      })
      
      output$sankey_plot <- renderSankeyNetwork({
        sankey_diag()
      })
    }
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