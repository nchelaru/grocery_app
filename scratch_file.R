library(shiny)
library(rhandsontable)
library(data.table)
library(hablar)

## load colours
cols <- toupper(c(
  "#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
  "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
  "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
  "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

shinyApp(
  ui=fluidPage(
    pageWithSidebar(
      headerPanel(title="Calendar Planner",windowTitle="Calendar Planner"),
      sidebarPanel(
        h3("Duration"),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 dateInput("in_duration_date_start","From",value=format(Sys.time(),"%Y-%m-%d"))
          ),
          column(6,style=list("padding-left: 5px;"),
                 dateInput("in_duration_date_end","To",value=format(as.Date(Sys.time())+30,"%Y-%m-%d"))
          )
        ),
        h3("Tracks"),
        fluidRow(
          column(3,style=list("padding-right: 3px;"),
                 textInput("in_track_name_1",label="Name",value="Vacation",placeholder="Vacation")
          ),
          column(3,style=list("padding-right: 3px; padding-left: 3px;"),
                 dateInput("in_track_date_start_1",label="From",value=format(Sys.time(),"%Y-%m-%d"))
          ),
          column(3,style=list("padding-right: 3px; padding-left: 3px;"),
                 dateInput("in_track_date_end_1",label="To",value=format(as.Date(Sys.time())+30,"%Y-%m-%d"))
          ),
          column(3,style=list("padding-left: 3px;"),
                 colourpicker::colourInput("in_track_colour_1",label="Colour",
                                           palette="limited",allowedCols=cols,value=cols[1])
          )
        ),
        fluidRow(
          column(3,style=list("padding-right: 3px;"),
                 textInput("in_track_name_2",label="Name",value="Offline",placeholder="Offline")
          ),
          column(3,style=list("padding-right: 3px; padding-left: 3px;"),
                 dateInput("in_track_date_start_2",label="From",value=format(Sys.time(),"%Y-%m-%d"))
          ),
          column(3,style=list("padding-right: 3px; padding-left: 3px;"),
                 dateInput("in_track_date_end_2",label="To",value=format(as.Date(Sys.time())+30,"%Y-%m-%d"))
          ),
          column(3,style=list("padding-left: 3px;"),
                 colourpicker::colourInput("in_track_colour_2",label="Colour",
                                           palette="limited",allowedCols=cols,value=cols[2])
          )
        ),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 colourpicker::colourInput("in_track_colour_available",label="Track colour (Available)",
                                           palette="limited",allowedCols=cols,value=cols[length(cols)-1])
          ),
          column(6,style=list("padding-left: 5px;"),
                 colourpicker::colourInput("in_track_colour_weekend",label="Track colour (Weekend)",
                                           palette="limited",allowedCols=cols,value=cols[length(cols)])
          )
        ),
        tags$br(),
        h3("Settings"),
        selectInput("in_legend_position",label="Legend position",
                    choices=c("top","right","left","bottom"),selected="right",multiple=F),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 selectInput("in_legend_justification",label="Legend justification",
                             choices=c("left","right"),selected="right",multiple=F)
          ),
          column(6,style=list("padding-left: 5px;"),
                 selectInput("in_legend_direction",label="Legend direction",
                             choices=c("vertical","horizontal"),selected="vertical",multiple=F)
          )
        ),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 numericInput("in_themefontsize",label="Theme font size",value=8,step=0.5)
          ),
          column(6,style=list("padding-left: 5px;"),
                 numericInput("in_datefontsize",label="Date font size",value=2.5,step=0.1)
          )
        ),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 numericInput("in_monthfontsize",label="Month font size",value=8,step=0.5)
          ),
          column(6,style=list("padding-left: 5px;"),
                 numericInput("in_legendfontsize",label="Legend font size",value=5,step=0.5)
          )
        ),
        tags$br(),
        h3("Download"),
        helpText("Width is automatically calculated based on the number of weeks. File type is only applicable to download and does not change preview."),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 numericInput("in_height","Height (cm)",step=0.5,value=5.5)
          ),
          column(6,style=list("padding-left: 5px;"),
                 numericInput("in_width","Width (cm)",step=0.5,value=NA)
          )
        ),
        fluidRow(
          column(6,style=list("padding-right: 5px;"),
                 selectInput("in_res","Res/DPI",choices=c("200","300","400","500"),selected="200")
          ),
          column(6,style=list("padding-left: 5px;"),
                 selectInput("in_format","File type",choices=c("png","tiff","jpeg","pdf"),selected="png",multiple=FALSE,selectize=TRUE)
          )
        ),
        downloadButton("btn_downloadplot","Download Plot"),
        tags$hr(),
        helpText("2019 | RaukR")
      ),
      mainPanel(
        sliderInput("in_scale","Image preview scale",min=0.1,max=3,step=0.10,value=1),
        helpText("Scale only controls preview here and does not affect download."),
        tags$br(),
        imageOutput("out_plot")
      )
    )
  ),
  server=function(input,output){}
)