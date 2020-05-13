library(shiny)
library(readxl)
library(tidyverse)
library(datasets)
library(forecast)
library(highcharter)
library(ggplot2)


setwd("~/Desktop/Msc BISS/Smart Service Project /SSP-project-master")
#prepare the data 

income_PC4 <- read.csv("Income_per_PC4")
income_PC4$mean_income <- income_PC4$mean_income*1000


lifestyles_PC4 <- read.csv("PC4_lifestyles")
income_PC4 <- left_join(income_PC4, lifestyles_PC4, by = "PC4")
income_PC4[is.na(income_PC4)] <- 0 
ui <- fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file", accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv')),
      # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
      sliderInput("forecast_slider", "Periods to forecast",
                  min = 0, max = 30, value = 1),
      dateRangeInput("daterange", label="range", min = "2017-01-01", max = "2019-01-01", start = "2017-01-01", end = "2019-01-01"),
      selectInput("variable", "variable", choices = c("mean_income", "Harmony", "Rest", "Insight", "Pleasure", "Style",
                                                      "Network", "Adventure"))
    ),
    mainPanel(
      uiOutput("tb")
      ,
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  )
)


# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
server <- function(input, output) {
  
  output$startdate <- renderText({
    as.character(input$daterange[1]) # start date selected by user
  })
  
  # returns the end date selected by the user
  output$enddate <- renderText({
    as.character(input$daterange[2]) # End date selected by the user
  })
  
  
  # returns the text showing the date range selected by the user
  output$range <- renderText({
    paste("Selected date range is ", input$daterange[1], "to", input$daterange[2]) 
  })
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- as.data.frame(input$file)
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })

  input_merged <- reactive({
    if(is.null(data()))
      return()
  
    # Matching the email address field in our uploaded file with that in our server data 
    merge(x=data(),y=income_PC4,by ="PC4",all.x=TRUE)
  })
  
  input_merged_by_date <- reactive({
    if(is.null(data()))
      return()
    input_merged() %>%
    group_by(date) %>%
    summarize(mean_income = as.numeric(mean(mean_income)),
              Harmony = mean(Harmony),
              Insight = mean(Insight),
              Pleasure = mean(Pleasure),
              Rest = mean(Rest),
              Style = mean(Style),
              Network = mean(Network),
              Adventure = mean(Adventure)) %>%
    filter(date >= as.character(input$daterange[1]), date <= as.character(input$daterange[2])) %>%
    select(date, input$variable)  
  })
  
  # start working on the ts
  
 
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
 
output$merged_agg <- renderTable({
  
  if(is.null(input_merged())){return ()}
  input_merged_by_date()
})

output$timeseries <- renderPlot({
 if(is.null(input_merged())){return ()}
 ggplot(input_merged_by_date(), aes_string(x = "date", y = input$variable))+geom_bar(stat = "identity")

 })
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")), tabPanel("Merged Data", tableOutput("merged")),
                  tabPanel("Aggregated Data", tableOutput("merged_agg")),
                  tabPanel("timeseries", plotOutput("timeseries")))
  })
}
shinyApp(ui, server)
 
