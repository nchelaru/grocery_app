library(shiny)
library(DT)

this_table = data.frame(bins = c(30, 50), cb = c(T, F))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxInput("cb", "T/F"),
      actionButton("add_btn", "Add"),
      actionButton("delete_btn", "Delete")
    ),
    
    mainPanel(
      DTOutput("shiny_table")
    )
  )
)

server <- function(input, output) {
  
  this_table <- reactiveVal(this_table)
  
  observeEvent(input$add_btn, {
    t = rbind(data.frame(bins = input$bins,
                         cb = input$cb), this_table())
    this_table(t)
  })
  
  observeEvent(input$delete_btn, {
    t = this_table()
    print(nrow(t))
    if (!is.null(input$shiny_table_rows_selected)) {
      t <- t[-as.numeric(input$shiny_table_rows_selected),]
    }
    this_table(t)
  })
  
  output$shiny_table <- renderDT({
    datatable(this_table(), selection = 'single', options = list(dom = 't'))
  })
}

shinyApp(ui = ui, server = server)