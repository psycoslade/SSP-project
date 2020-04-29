install.packages("shinydashboard", "tmap", "tmaptools", "leaflet")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)



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
             tabName = "Lifestyle Map", icon = icon("map")),
    menuItem("Dashboard Descriptives",
             tabName = "Descriptive Analytics", icon = icon("calculator")),
    menuItem("Dashboard Predictive",
             tabName = "Predictive Analytics", icon = icon("chart-line"))
  )
)

#this code is for the main body of data vis such as the mapping feature
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Dashboard Map",
          fluidRow(
            box(title = "Lifestyle Map", solidHeader = TRUE, width = 12, height = NULL,),
            box(textInput(inputId = "postalcode", label = "Postal code", value = "0000AA"), width = 6, height = NULL),
            box(sliderInput(inputId = "years", label = "Years of data", min = 2010, max = 2016, value = 2016))
          ))))


#this part regards the server and the upload feature
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output) {}

shinyApp(ui,server)
