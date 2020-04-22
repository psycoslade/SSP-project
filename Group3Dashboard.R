install.packages("shinydashboard", "tmap", "tmaptools", "leaflet")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)



#this code is for the top bar and drop down messages
header <- dashboardHeader(
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
    menuItem("DashboardMap",
             tabName = "Lifestyle Map"),
    menuItem("DashboardDescriptives",
             tabName = "Descriptive Analytics"),
    menuItem("DashboardPredictive",
             tabName = "Predictive Analytics")
  ),
  sliderInput(inputId = "years", label = "Years of data", min = 2010, max = 2016, value = 2016)
)

#this code is for the main body of data vis such as the mapping feature
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Lifestyle Map",
            "This is where the lifestyle map will be"),
    tabItem(tabName = "Descriptive Anlaytics",
            tabBox(title = "Descriptives"),
            tabPanel(title = "Stripper Dave"),
            tabPanel(title = "Sexy Silvester")),
    tabItem(tabName = "Predictive Analytics",
            "If we get far enough here will be the trends that are forseeable"))
  )




#this part regards the server and the upload feature
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output) {}

shinyApp(ui,server)
