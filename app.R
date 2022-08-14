library(shiny)
source("utils.R")
library(plotly)
library(shinycssloaders)
library(dplyr)
library(stringr)
library(shinythemes)
library(ggplot2)
library(bslib)

# display loading settings
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# application UI 
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cosmo"),
  navbarPage(
    "Tree-life",
    tabPanel(
      "Dashboard",
      wellPanel(
        "Input Panel",
        dateRangeInput(
          "daterange",
          "Date Range",
          start = "2013-01-01",
          end = "2021-01-01",
          max = "2021-01-01",
          min = "2013-01-01",
          startview = "decade",
          format = "yyyy-mm-dd"
        ),
        actionButton("updatemap",
                     "update dashboard")
        
      ),
      wellPanel(
        htmlOutput("text")
        
      ),
      
      withSpinner(plotlyOutput("scattermap"),
                  type = 1
                  ),
      withSpinner(plotlyOutput("cumulative"),
                  type = 1
                  ),
      withSpinner(plotlyOutput("histogram"),
                  type = 1
      )
      
      
    ),
    tabPanel(
      "About",
      includeMarkdown("about.Rmd")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Read data-frame
  df = reactive({ 
    treat_dataframe(read.csv("tree-data.csv"), 
                    convert_date(input$daterange)
                    )
  })
  
  # # save tree balance
  # balance() = eventReactive({
  #           compute_balance(treat_dataframe(read.csv("tree-data.csv"), 
  #                             convert_date(input$daterange)),
  #                           convert_date(input$daterange
  #                                        )
  #                           )
  #                           
  #   
  # })
  
  # generate text 
  output$text = renderUI({
    HTML(paste("Tree balance of",
                tags$strong(compute_balance(df(), convert_date(input$daterange))),
                "tree(s) between",
                tags$b(input$daterange[1]),
                "and",
                tags$b(input$daterange[2]))
              )
    
  })

  # generate scatter map
  output$scattermap = renderPlotly({
    if (input$updatemap>=0) { 
      isolate(generate_scattermap(df(),
                                  convert_date(input$daterange)
                                  )
              ) 
    }
  })
  
  # generate cumulative curve
  output$cumulative = renderPlotly({
    if (input$updatemap>=0) { 
      isolate(generate_cumulative(df(),
                                  convert_date(input$daterange)
                                  )[[1]]
              ) 
    }
  })
  
  # generate histogram
  output$histogram = renderPlotly({
    if (input$updatemap>=0) { 
      isolate(generate_histogram(df(),
                                  convert_date(input$daterange)
      )
      ) 
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
