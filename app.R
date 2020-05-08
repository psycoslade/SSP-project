# Limburg Marketing Dashboard

# set working directory

setwd("/Users/Daniel/Documents/Master/Studium/1. Semester/Period 4/Smart Service Project/data")


# packages

library(leaflet)
library(leaflet.extras)
library(ggmap)
library(cbsodataR)
library(tidyverse)
library(sf)
library(sp)
library(geojsonio)
library(tigris)
library(ggmap)
library(DT)
library(xlsx)
library(data.table)
library(lubridate)
library(stringr)
library(htmlwidgets)
library(scales)

# preparing the data

# search CBS data

df_cbs_toc <- cbs_get_toc() %>% filter(str_detect(Title, "Kerncijfers"))


# Find out which columns are available
metadata <- cbs_get_meta("83765NED")
print(metadata$DataProperties$Key)

data <- cbs_get_data("83765NED", 
                     select=c("WijkenEnBuurten","Gemeentenaam_1", "SoortRegio_2", 
                              "AantalInwoners_5", "k_0Tot15Jaar_8", "k_15Tot25Jaar_9", 
                              "k_25Tot45Jaar_10", "k_45Tot65Jaar_11", "k_65JaarOfOuder_12", "GeboorteRelatief_25", 
                              "Gescheiden_15", "HuishoudensTotaal_28", "AantalInkomensontvangers_64", "GemiddeldInkomenPerInkomensontvanger_65",
                              "GemiddeldInkomenPerInwoner_66", "k_65JaarOfOuder_12", "k_40PersonenMetLaagsteInkomen_67", 
                              "k_20PersonenMetHoogsteInkomen_68", "k_40HuishoudensMetLaagsteInkomen_70", "k_20HuishoudensMetHoogsteInkomen_71",
                              "HuishoudensMetEenLaagInkomen_72")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         births = GeboorteRelatief_25)

### This part threw an error for some reason
# not needed anymore

# Retrieve data with municipal boundaries from PDOK
#municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json", stringAsFactors = FALSE)

#data <- municipalBoundaries %>%
#  left_join(data, by=c(statcode="WijkenEnBuurten"))

###

boundaries <- geojson_read("https://opendata.arcgis.com/datasets/e1f0dd70abcb4fceabbc43412e43ad4b_0.geojson", what = "sp")

#boundaries_income <- boundaries@data %>% 
#  left_join(data, by = c("Gemeentecode" = "statcode")) results in a dataframe


boundaries_income <- geo_join(boundaries, data, "Gemeentecode", "WijkenEnBuurten", how = "left") # results in a Polygon Object

# include Leistert data

# processed data by Stefan
# Leistert_df <- fread(file = "data_prelim.csv", sep = ";")


# unprocessed Leistert df

Leistert_df <- fread(file = "CASE_1_LEISTERT_Final_with_Lifestyle.csv", sep = ";")
colnames(Leistert_df) <- gsub(" ", "_", colnames(Leistert_df))


### Test ###

# create month and year column 

Leistert_df$Aankomst <- as.Date(Leistert_df$Aankomst) 
Leistert_df$month <- strftime(Leistert_df$Aankomst, "%m")
Leistert_df$year <- strftime(Leistert_df$Aankomst, "%Y")

# group Leistert_df per Gemeente

Leistert_df_gemeente <- Leistert_df %>% 
  group_by(Gemeente_Name) %>% 
  tally() %>% rename("number_of_visitors" = n)

###

# join Leistert_df_gemeente to boundaries_income

boundaries_income <- geo_join(boundaries_income, Leistert_df_gemeente, "Gemeentenaam", "Gemeente_Name", how = "left")


# using shiny

library(shiny)

ui <- fluidPage(
  titlePanel("Limburg Marketing Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "ZIP",
                label = "Select a ZIP Code:",
                choices = NULL,
                options = list(maxItems = 1, maxOptions = 5)),
      selectizeInput(inputId = "Gemeente",
                     label = "Choose a Gemeente",
                     choices = NULL,
                     options = list(maxItems = 1, maxOptions = 5)),
      selectInput(
          inputId = "year", 
          label = "Select time period:", 
          choices = 2017:2019),
      
      checkboxInput(inputId = "shortest_stays",
                    label = "Show shortest average stays",
                    value = FALSE,
                    width = '100%'),
      
      selectInput(inputId = "facility",
                  label = "Choose Facility Type",
                  choices = c("TH", "TP", "BUN", "CH" , "HT", "SP", "JP")),
    p("Made with", a("Shiny.",
                     href = "http://shiny.rstudio.com"
    )),
    img(src="Shiny_Logo.png")
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Map", uiOutput("leaf")),
      tabPanel("Table", 
               fluidRow(column(
                 DT::dataTableOutput("table"), tableOutput("table2"), width = 11)
               )
               ),
      tabPanel("Plot", 
               fluidRow(
                 splitLayout(cellWidths = c("65%", "35%"),
                             plotOutput("plot"), plotOutput("plot4")
                             ),
                 conditionalPanel(condition = "input.shortest_stays == 0",
                                              plotOutput("plot2")
                                              ),
                 conditionalPanel(condition = "input.shortest_stays == 1",
                                              plotOutput("plot2_short")
                                              ),
               splitLayout(cellWidths = c("65%", "35%"),
                           plotOutput("plot3"),  plotOutput("plot5"))
                            
                 ),
               
               )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  #css_fix <- "div.info.legend.leaflet-control br {clear: both;}" not working inside shiny
  #html_fix <- htmltools::tags$style(type = "text/css", css_fix) 
  
  output$leaf <- renderUI({
    leafletOutput("map", width = "100%", height = 900) # option to adjust size of leaflet map
  })
  
  output$map <- renderLeaflet({
    
    NL_pal <- colorNumeric("Blues", domain = boundaries_income@data$GemiddeldInkomenPerInwoner_66)
    NL_pal_2 <- colorNumeric("YlGn", domain = boundaries_income@data$HuishoudensTotaal_28)
    NL_pal_3 <- colorNumeric("Reds", domain = boundaries_income@data$AantalInwoners_5)
    NL_pal_4 <- colorNumeric("Greens", domain = boundaries_income@data$number_of_visitors) # option to filter for year - right now it is aggregated
    
    popup_inkom = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Mean Income: ",
                        boundaries_income$GemiddeldInkomenPerInwoner_66) %>% map(htmltools::HTML)
    popup_huishouden = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Huishouden Total: ",
                             boundaries_income$HuishoudensTotaal_28) %>% map(htmltools::HTML)
    popup_inwoner <- str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Inwoner Total: ",
                           boundaries_income$AantalInwoners_5) %>% map(htmltools::HTML)
    popup_visitors <- str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Cumulated Visitors Total: ",
                            boundaries_income$number_of_visitors) %>% map(htmltools::HTML)
    
    boundaries_income  %>%
      leaflet() %>% # prependContent(html_fix) %>%  not working within Shiny
      addTiles(group = "OSM") %>% 
      addProviderTiles("CartoDB", group = "Carto") %>% 
      addProviderTiles("Esri", group = "Esri") %>%
      addPolygons(weight = 2,fillOpacity = 0.5, color = ~NL_pal(GemiddeldInkomenPerInwoner_66),
                  label = ~popup_inkom,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inkomen") %>% 
      addLegend(pal = NL_pal, values = ~boundaries_income@data$GemiddeldInkomenPerInwoner_66,
                group = "Inkomen", position = "bottomleft", title = "Mean Income in Thousands",
                labFormat = labelFormat(prefix = "€"), opacity = 1) %>% 
      addPolygons(weight = 2,fillOpacity = 0.5, color = ~NL_pal_2(HuishoudensTotaal_28),
                  label = ~popup_huishouden,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Huishouden") %>% 
      addLegend(pal = NL_pal_2, values = ~boundaries_income@data$HuishoudensTotaal_28,
                group = "Huishouden", position = "bottomleft", title = "Aantal Huishouden", opacity = 1) %>% 
      addPolygons(weight = 2,fillOpacity = 0.5, color = ~NL_pal_3(AantalInwoners_5),
                  label = ~popup_inwoner,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inwoner") %>% 
      addLegend(pal = NL_pal_3, values = ~boundaries_income@data$AantalInwoners_5,
                group = "Inwoner", position = "bottomleft", title = "Aantal Inwoners", opacity = 1) %>% 
      addPolygons(weight = 2,fillOpacity = 0.5, color = ~NL_pal_4(number_of_visitors),
                  label = ~popup_visitors,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Visitors") %>% 
      addLegend(pal = NL_pal_4, values = ~boundaries_income@data$number_of_visitors,
                group = "Visitors", position = "bottomleft", title = "Cumulated number of visitors", opacity = 1) %>% 
      addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                       overlayGroups = c("Inkomen", "Huishouden", "Inwoner", "Visitors")) %>% showGroup("Carto") %>%
      hideGroup(c("Huishouden", "Inwoner", "Visitors"))
  })
  
  table_df <- as.data.frame(boundaries_income) %>% select(c("Gemeentenaam", "Gemeentecode","GeboorteRelatief_25", "AantalInwoners_5",
                                                            "Gescheiden_15", "HuishoudensTotaal_28", "GemiddeldInkomenPerInwoner_66",
                                                            "k_20PersonenMetHoogsteInkomen_68", "k_20HuishoudensMetHoogsteInkomen_71",
                                                            "GemiddeldInkomenPerInkomensontvanger_65", "k_40HuishoudensMetLaagsteInkomen_70"))
  
  colnames(table_df) <- c("Gemeentenaam", "Gemeentecode", "Geboorte_Relatief", "Aantal_Inwoner", "Gescheiden", "Huishouden_Totaal",
                          "GemiddeldInkomenPerInwoner", "20PersonenMetHoogsteInkome", "20HuishoudensMetHoogsteInkomen",
                          "GemiddeldInkomenPerInkomensontvanger", "40HuishoudensMetLaagsteInkomen")
  
  output$table <- DT::renderDataTable({table_df
  }, options = list(scrollX = TRUE)) # option to be able to scroll within the table
  
  
  # extract month and year 
  
  Leistert_df$Aankomst <- as.Date(Leistert_df$Aankomst) 
  Leistert_df$month <- strftime(Leistert_df$Aankomst, "%m")
  Leistert_df$year <- strftime(Leistert_df$Aankomst, "%Y")

  
  # show frequency of visits per municipalities per year
  
  
  Leistert_df_grouped <- reactive({
      dplyr::filter(Leistert_df, year == input$year) %>%   
      group_by(Gemeente_Name) %>%
      tally() %>%
      arrange(desc(n)) %>% 
      head(20)
    })
  
  
  output$plot <- renderPlot({
    
    ggplot(Leistert_df_grouped(), mapping = aes(x = reorder(Gemeente_Name, -n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low=muted("red"), mid='white', high=muted("blue"), space = "Lab") +
      ggtitle(paste("Number of Visitors in", input$year)) +
      geom_text(aes(label = n, vjust = 1.5), color = "white") +
      xlab("Gemeente") +
      ylab("Number of Bookings") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # show avg stay per Gemeente per year per facility type (longest/shortest)
  # create two dfs -> one for longest and one for shortest average length of stay
  
  # create new column with facility type
  
  Leistert_df$facility_type_new <- str_extract(Leistert_df$Facility_Type, "[A-Z]+")

  
  Leistert_df_grouped_2 <- reactive({
      dplyr::filter(Leistert_df, year == input$year, facility_type_new == input$facility) %>% 
      group_by(Gemeente_Name) %>% 
      summarise(avg_stay = round(mean(Duration_of_Stay), 2)) %>%
      arrange(desc(avg_stay)) %>% 
      head(20)
    })
  
  Leistert_df_grouped_2_short <- reactive({
    dplyr::filter(Leistert_df, year == input$year, facility_type_new == input$facility) %>% 
      group_by(Gemeente_Name) %>% 
      summarise(avg_stay = round(mean(Duration_of_Stay), 2)) %>%
      dplyr::filter(avg_stay > 1) %>% 
      arrange(avg_stay) %>% 
      head(20)
  })
  
  # plot average longest stays
  
  output$plot2 <- renderPlot({
    
   
    ggplot(Leistert_df_grouped_2(), mapping = aes(x = reorder(Gemeente_Name, -avg_stay), y = avg_stay, fill = avg_stay)) +
      scale_fill_gradient(low="blue", high="red", space="Lab") +
      geom_bar(stat = "identity") +
      ggtitle(paste("Average Length of Stay per Gemeente in", input$year, "for", input$facility)) +
      geom_text(aes(label = avg_stay, vjust = 1.5), color = "white") +
      xlab("Gemeente") +
      ylab("Average Length of Stay") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # plot averagest shortest stays
  
  output$plot2_short <- renderPlot({
    
    
    ggplot(Leistert_df_grouped_2_short(), mapping = aes(x = reorder(Gemeente_Name, -avg_stay), y = avg_stay, fill = avg_stay)) +
      scale_fill_gradient2(low="blue", high="red", space="Lab") +
      geom_bar(stat = "identity") +
      ggtitle(paste("Shortest Average Length of Stay per Gemeente in", input$year, "for", input$facility)) +
      geom_text(aes(label = avg_stay, vjust = 1.5), color = "white") +
      xlab("Gemeente") +
      ylab("Average Length of Stay") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  # filter for Gemeente and Facility Type
  
  # filter for year beforehand?
  
  Leistert_df_grouped_3 <- reactive({
    dplyr::filter(Leistert_df, POSTCODE == input$ZIP, year == input$year) %>% 
      group_by(POSTCODE, Facility_Type) %>% 
      tally() %>% 
      spread(Facility_Type, n) %>% replace(., is.na(.), 0)
  })
    output$table2 <- renderTable({Leistert_df_grouped_3()
  
  })
  

  # serachable input for ZIP Codes on Server-side -> Client-side also possible but handles high in put less well
    
    updateSelectizeInput(session, 'ZIP', choices = Leistert_df$POSTCODE, server = TRUE)
    
    
  # display visitors per month in a given year
  
Leistert_df_grouped_4 <- reactive({
  dplyr::filter(Leistert_df, year == input$year) %>% 
    group_by(year, month) %>%
    tally()
})

# calculate mean for midpoint for better visualistation

midpoint <- Leistert_df %>% group_by(year, month) %>% tally()
mid <- mean(midpoint$n)

output$plot3 <- renderPlot({ggplot(Leistert_df_grouped_4(), mapping = aes(x = month, y = n, fill = n)) +
    scale_fill_gradient2(low="snow3", mid="green", high="darkgreen", space="Lab", midpoint = mid) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n, vjust = 1.5), color = "white") +
    ggtitle(paste("Monthly Visitors in total in", input$year)) +
    xlab("Month") +
    ylab("Number of Visitors")

})

# as.numeric(gsub(",", ".", Percentage)) change commas to periods
# then calculate label positions

Leistert_df_ls <- Leistert_df %>% select(POSTCODE, Buurt_Name, Gemeente_Name, avontuurzoekers:stijlzoekers) %>% 
  gather(key = "Groups", value = "Percentage", -POSTCODE, -Buurt_Name, -Gemeente_Name) %>% unique()

Leistert_df_ls$Percentage <- as.numeric(gsub(",", ".", Leistert_df_ls$Percentage))

#Leistert_df_ls <- Leistert_df_ls %>% 
#  group_by(Groups) %>% 
#  mutate(pos = cumsum(Percentage) - Percentage/2)
  

# group_by(year) %>% mutate(pos = cumsum(quantity)- quantity/2)


Leistert_df_grouped_5 <- reactive({Leistert_df_ls %>% 
    dplyr::filter(POSTCODE == input$ZIP) 
  
    
})

output$plot4 <- renderPlot({
  ggplot(Leistert_df_grouped_5(), mapping = aes(x = "", y = Percentage, fill = Groups)) +
    geom_bar(stat="identity", width = 1, color = "white") +
    coord_polar("y", start=0) + 
    theme_void() + # remove background, grid, numeric labels
    geom_text(data=subset(Leistert_df_grouped_5(), Percentage != 0), aes(label = paste0(round(Percentage), "%")), # only use values unequal to zero
              color = "white", size=5, position = position_stack(vjust = 0.5)) +
    ggtitle(paste("Distribution of Lifestyles in", input$ZIP))
})
   
 
  # plot age distribution per Gemeente 
  
data_age <- data %>% 
  select("Gemeentenaam_1",
         "SoortRegio_2", "AantalInwoners_5", 
         "k_0Tot15Jaar_8", "k_15Tot25Jaar_9", 
         "k_25Tot45Jaar_10", "k_45Tot65Jaar_11",
         "k_65JaarOfOuder_12") 

# alter column names

colnames(data_age) <- gsub(" ", "_", colnames(data_age))

# change type of column SoortRegio

data_age$SoortRegio_2 <- as.character(data_age$SoortRegio_2) %>%
  trimws() # trim whitespace, otherwise
# filtering does not find any matches

data_age <- data_age %>% 
  filter(SoortRegio_2 == "Gemeente")


# join data_age with Leistert_df to get POSTCODE column

#data_age <- data_age %>% 
#  left_join(select(Leistert_df, POSTCODE, Gemeente_Name), by= c("Gemeentenaam_1" = "Gemeente_Name"))
    
# transform dataframe into long format

data_age_long <- data_age %>%
  select(Gemeentenaam_1, k_0Tot15Jaar_8:k_65JaarOfOuder_12) %>% 
  gather(key = "Age_Group", value = "Aantal", -Gemeentenaam_1)


# change names of age groups

# shorter option
data_age_long$Age_Group %<>% #this pipe already assigns dataframe instead of writing it out
  gsub("k_0Tot15Jaar_8", "0-15", .) %>% #like data_age_long$Age_Group <- data_age_long$Age_Group %>% ...
  gsub("k_15Tot25Jaar_9", "15-25", .) %>% 
  gsub("k_25Tot45Jaar_10", "25-45", .) %>% 
  gsub("k_45Tot65Jaar_11", "45-65", .) %>% 
  gsub("k_65JaarOfOuder_12", ">65", .)

# longer option 

#data_age_long$Age_Group <- str_replace_all(data_age_long$Age_Group, "k_0Tot15Jaar_8", "0-15")
#data_age_long$Age_Group <- str_replace_all(data_age_long$Age_Group, "k_15Tot25Jaar_9", "15-25")
#data_age_long$Age_Group <- str_replace_all(data_age_long$Age_Group, "k_25Tot45Jaar_10", "25-45")
#data_age_long$Age_Group <- str_replace_all(data_age_long$Age_Group, "k_45Tot65Jaar_11", "45-65")
#data_age_long$Age_Group <- str_replace_all(data_age_long$Age_Group, "k_65JaarOfOuder_12", ">65")


# plot age distribution on Gemeente Level

data_age_df <- reactive({
  data_age_long %>% 
    dplyr::filter(Gemeentenaam_1 == input$Gemeente)
})

output$plot5 <- renderPlot({
  ggplot(data_age_df(), mapping = aes(x = "", y = Aantal, fill = Age_Group)) +
    geom_bar(stat="identity", width = 1, color = "white") +
    coord_polar("y", start=0) + 
    theme_void() + # remove background, grid, numeric labels
    geom_text(data=subset(data_age_df(), Aantal != 0), aes(label = paste0(round(Aantal / sum(Aantal) * 100), "%")), # only use values unequal to zero
              color = "white", size=5, position = position_stack(vjust = 0.5)) +
    ggtitle(paste("Age Distribution in", input$Gemeente))

  }) 

updateSelectizeInput(session, 'Gemeente', choices = data_age_long$Gemeentenaam_1, server = TRUE)


}

shinyApp(ui, server)

