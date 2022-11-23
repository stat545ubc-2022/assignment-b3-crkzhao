library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      checkboxInput("sortPrice", "Sort the table by Price", FALSE), # feature 1: Add an option to sort the bcl data by Price
      uiOutput("typeOutput"), # feature 2: Allow the user to search for multiple types simultaneously
      uiOutput("countryOutput")
    ),
    mainPanel(
      # feature 5: Place the plot and the table in separate tabs
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("coolplot")),
        tabPanel("Table",
                 h4(textOutput("sizeOfTable")), #feature 3: Show the number of results found whenever the filters change
                 downloadButton("downloadTable", "Download the table"), # feature 4: Allow the user to download the table as a .csv file
                 br(), br(),
                 tableOutput("results"))
      )
    )
  )
)

server <- function(input, output) {
  # feature 2
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                selected = "WINE",
                multiple = TRUE) # Allow the user to search for multiple types simultaneously
  })  
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput) || is.null(input$typeInput)) {
      return(NULL)
    } 
    
    # feature 1: I will firstly sort the data, then find the item that you want 
    if(input$sortPrice) {
      bcl <- arrange(bcl, Price) # sort the data by Price
    }
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram(bins = 30)
  })
  
  output$results <- renderTable({
    filtered()
  })
  
  # feature 3
  output$sizeOfTable <- renderText({
    if (is.null(filtered())) {
      table_size <- 0
    } else {
      table_size <- nrow(filtered()) # get how many rows in the filtered()
    }
    paste("We have found", table_size, "choices for you")
  })

  # feature 4
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("bcl-", input$countryInput, "-data.csv", sep = "") # the downloaded file name
    },
    content = function(file) {
      write.csv(filtered(), file) # write csv file
    }
  )
}

shinyApp(ui = ui, server = server)