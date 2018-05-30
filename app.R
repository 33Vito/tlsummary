#
# This is a Shiny web application to demostrate the tlsummary function. 
# You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(shinycssloaders)
source("TL gogogo.R") # utility functions

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("tlsummary demo"),
   h4("Tony Liu, June 2018"), 
   
   # Sidebar with options to update a dataframe (in csv)
   sidebarLayout(
      sidebarPanel(width = 4, 
        # fileInput("input_csv", "upload csv file", width = "360px",
        #           accept = c("text/csv","text/comma-separated-values,text/plain",".csv")), 
        # actionButton("input_demo", "demo"), 
        
        div(style="display: inline-block;vertical-align:top;",
            fileInput("input_csv", "upload csv file", width = "350px",
                      accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))), 
        div(style="display: inline-block;vertical-align:top; padding-top: 26px;",
            actionButton("input_run", "run", width = "70px")), 
        div(style="display: inline-block;vertical-align:top; padding-top: 26px;",
            actionButton("input_demo", "demo", width = "70px")), 
        
        uiOutput("ui_n"), 
        uiOutput("ui_gvar")
      ),
      
      # Show a graph output from tlsummary
      mainPanel(
        uiOutput("plots") %>% withSpinner(type=1, color = DC[2])
      )
   )
)

# Define server logic
# max_plots <- 20
server <- shinyServer(function(input, output) {
  data1 <- reactive({
    if (!input$input_demo) req(input$input_csv)
    
    inFile <- input$input_csv
    
    if (input$input_demo) dd <- diamonds[,1:7]
    else dd <- read_csv(inFile$datapath)
    
    dd[,map_lgl(dd, ~is.numeric(.x) | is.integer(.x) | is.factor(.x) | is.character(.x))]
  })
  
  gvar <- reactive({
    if (input$gvar == "") NULL
    else data1()[[input$gvar]]
  })
  
  get_plot_output_list <- function(max_plots, input_n) {
    # Insert plot output objects the list
    plot_output_list <- lapply(1:input_n, function(i) {
      plotname <- names(data1())[i]
      plot_output_object <- plotOutput(plotname)
      plot_output_object <- renderPlot({
        tlsummary(data1()[, i, drop=F], gvar = gvar(), graph_size = 15, table_size = 15, table_padding = c(4,4))
      })
    })
    
    do.call(tagList, plot_output_list) # needed to display properly.
    
    return(plot_output_list)
  }
  
  output$ui_n <- renderUI({
    if (!input$input_demo) req(input$input_csv)
    sliderInput("n", "Number of plots/variables to show: ", value=1, min=1, max=ncol(data1()), step = 1)
  })
  
  output$ui_gvar <- renderUI({
    selectInput("gvar", "Grouping variable: ", 
                selected = NULL,
                multiple = F, 
                choices = c("", names(data1())[map_lgl(data1(), ~class(.x)[1] %in% c("ordered", "factor", "character"))]))
  })

  observeEvent(input$input_run, { 
    output$plots <- renderUI({get_plot_output_list(ncol(data1()), input$n)})
  })
  })

# Run the application 
# runApp(ui = ui, server = server, launch.browser = "chrome")
shinyApp(ui = ui, server = server)
