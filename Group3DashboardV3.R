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

#here is a data frame with the tutorial messages that will show up in the task
#ui dynamic how-to video shows how to incorporate using apply function


#this is the current placeholder vector for regions
placeholder_region <- c("Limburg", "Brabant", "Gelderland")
placeholder_region
placeholder_lifestyle <- c("lifestyle 1", "lifestyle 2", "lifestyle 3")
placeholder_lifestyle

#this code is for the top bar and drop down messages
header <- dashboardHeader(title = "LM Dashboard",
  dropdownMenu(type = "messages",
               messageItem(from = "Group 3 SSP",
                           message = "here we can plug in tutorials and explanation"),
              messageItem(from = "Group 3 SSP",
                          message = "have a look at the sidebar options"))
)

#this code is for the sidebar and new pages
#a lot of this code coincides with the body
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard Map",
             tabName = "LifestyleMap", icon = icon("map")),
    menuItem("Dashboard Descriptives",
             tabName = "DescriptiveAnalytics", icon = icon("calculator")),
    menuItem("Dashboard Predictive",
             tabName = "PredictiveAnalytics", icon = icon("chart-line"))
  )
)

#this code is for the main body of data vis such as the mapping feature
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "LifestyleMap",
    fluidRow(
      shinydashboard::box(width = 4, title = "Infobox 1", "showing number of results", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 2", "showing highest lifestyle value", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 3", "showing some other shit", height = 100, status = "info"),
      shinydashboard::box(width = 12, title = "Lifestyle Map", "This is where the Lifestyle Map will be", height = 400, status = "warning", solidHeader = TRUE),
      shinydashboard::box(width = 6, title = "Location Inputs", textInput("text", "Postal Code:"), selectInput(inputId = "Region", label =  "region",
                         choices = placeholder_region),solidHeader = TRUE, status = "danger"),
      shinydashboard::box(widtch = 6, title = "Variable Inputs", sliderInput("Years of Data", label = "Years of data", min = 2000, max = 2030, 
                          value = c(2008, 2018)),selectInput(inputId = "Lifestyle Search", label = "Lifestyle Search", choices = placeholder_lifestyle),
                          solidHeader = TRUE, status = "danger"))),
    tabItem(tabName = "DescriptiveAnalytics",
      fluidRow(
        #these values need to be made interactive later
        infoBox("Mean Duration of Stay", mean(LeistertData$`Duration of Stay`), icon = icon("clock")),
        infoBox("total Households Included", nrow(LeistertData), icon = icon("hashtag")))),
    tabItem(tabName = "PredictiveAnalytics")))


#renderPrint()

#this part regards the server and the upload feature
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output,session) {
  
}

shinyApp(ui,server)

