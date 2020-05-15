Sys.which("make")
install.packages("jsonlite", type = "source")
install.packages("shinydashboard", "tmap", "tmaptools", "leaflet", "devtools")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)

#loading the data
setwd("C:/Users/Cies/Desktop/BISS/Smart Service Project/SSP dashboard")
LeistertData <- read_excel("LeistertData.xlsx")
LeistertData
Data2 <- read_excel("data_prelim2.xlsx")
Data2

#here is a data frame with the tutorial messages that will show up in the task
#ui dynamic how-to video shows how to incorporate using apply function
TaskMessage <- c("Welcome to the Limburg Marketing dashboard! This tutorial will guide you through the use and update of this system to maximise your use of data.",
                 "The tabs to your left will allow you to interact with the data differently. the first tab will lead you to where you are now, Lifestyle Map, Descriptives show you an initial understanding of people attracted to middle Limburg for tourism",
                 "Predictive Analytics will further delve into trends that can be spotted and predictions that can be made",
                 "The last tab will guide you through the procedure of uploading new data and formating procedures",
                 "For user interaction we color coded our visualizations based on their purpose",
                 "blue boxes are outputs which will show you information",
                 "red boxes are inputs that allow you to adjust the variables of relevance",
                 "orange boxes are the core visualizations which will make understanding the trends simple",
                 "To conclude our tutorial read through the information shown in the data interaction tab") 
TaskDF <- data.frame("ButtonValue" = 1:9, "taskMessage" = c(TaskMessage))
TaskDF


#this is the current placeholder vector for regions
placeholder_region <- c("Limburg", "Brabant", "Gelderland")
placeholder_region
placeholder_lifestyle <- c("lifestyle 1", "lifestyle 2", "lifestyle 3")
placeholder_lifestyle

#tutorial dropdown interaction menu
PlaceholderValue <- 10
  Value <- 10 + ActButton

#this code is for the top bar and drop down messages
header <- dashboardHeader(title = "LM Dashboard",
  dropdownMenu(type = "messages",
               messageItem(from = "Group 3 SSP",
                           message = "Thank you for working with BISS Master students"),
  dropdownMenu(type = "tasks",
              taskItem(value = PlaceholderValue,
                       color = "aqua",
                       "Limburg Marketing Tutorial"))))


#------------------------------------------------------------------------------------------------------------------------
#this code is for the sidebar and new pages
#a lot of this code coincides with the body
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tutorial",
             tabName = "Tutorial", icon = icon("location-arrow")),
    menuItem("Dashboard Map",
             tabName = "LifestyleMap", icon = icon("map")),
    menuItem("Dashboard Descriptives",
             tabName = "DescriptiveAnalytics", icon = icon("calculator")),
    menuItem("Dashboard Predictive",
             tabName = "PredictiveAnalytics", icon = icon("chart-line")),
    menuItem("Data interaction",
             tabName = "DataInteraction", icon = icon("database"))
  )
)


#this code is for the main body of data vis such as the mapping feature
body <- dashboardBody(
  tabItems(
#Tutorial
    tabItem(tabName = "Tutorial",
    fluidRow(
      shinydashboard::box(width = 12, title = "Tutorial", height= 150, status = "info", textOutput("TaskMessage")
    ),
      (actionButton("NextButton", "NEXT", icon = NULL, width = NULL)))),
#Lifestyle Mapping feature
    tabItem(tabName = "LifestyleMap",
    fluidRow(
      shinydashboard::box(width = 4, title = "Infobox 1", "showing number of results", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 2", "showing highest lifestyle value", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 3", "showing something else", height = 100, status = "info"),
      shinydashboard::box(width = 12, title = "Lifestyle Map", "This is where the Lifestyle Map will be", height = 400, status = "warning", solidHeader = TRUE),
      shinydashboard::box(width = 6, title = "Location Inputs", textInput("text", "Postal Code:"), selectInput(inputId = "Region", label =  "region",
                         choices = placeholder_region),solidHeader = TRUE, status = "danger"),
      shinydashboard::box(widtch = 6, title = "Variable Inputs", sliderInput("Years of Data", label = "Years of data", min = 2000, max = 2030, 
                          value = c(2008, 2018)),selectInput(inputId = "Lifestyle Search", label = "Lifestyle Search", choices = placeholder_lifestyle),
                          solidHeader = TRUE, status = "danger"))),
#Desctiptive analytics
    tabItem(tabName = "DescriptiveAnalytics",
      fluidRow(
        #these values need to be made interactive later
        infoBox("Mean Duration of Stay", mean(LeistertData$`Duration of Stay`), icon = icon("clock")),
        infoBox("total Households Included", nrow(LeistertData), icon = icon("hashtag")))),
#Predictive analytics
    tabItem(tabName = "PredictiveAnalytics",
            fluidRow(
              shinydashboard::box(width = 12, title = "Predictive visualizations", "click on the different tabs for different predictive visualizations", height = 600)
            ))),
#Data Loading
    tabItem(tabName = "Data Interaction"))

#----------------------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output,session) {
  
output$TaskMessage <- renderText({
  TaskMessage[input$NextButton]
})

}

shiny::shinyApp(ui,server)

