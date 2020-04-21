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

leaflet() %>%
  addProviderTiles("Stamen") %>% 
  addSearchOSM %>% 
  addReverseSearchOSM() %>% 
  addResetMapButton() 

# Find out which columns are available
metadata <- cbs_get_meta("83765NED")
print(metadata$DataProperties$Key)


data <- cbs_get_data("83765NED", 
                     select=c("WijkenEnBuurten","GeboorteRelatief_25", "AantalInwoners_5",
                              "Gescheiden_15", "HuishoudensTotaal_28", "GemiddeldInkomenPerInwoner_66",
                              "k_20PersonenMetHoogsteInkomen_68", "k_20HuishoudensMetHoogsteInkomen_71",
                              "GemiddeldInkomenPerInkomensontvanger_65", "k_40HuishoudensMetLaagsteInkomen_70")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         births = GeboorteRelatief_25)


# Retrieve data with municipal boundaries from PDOK
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")


data <- municipalBoundaries %>%
  left_join(data, by=c(statcode="WijkenEnBuurten"))


# Create a thematic map
data %>%
  ggplot() +
  geom_sf(aes(fill = births)) +
  scale_fill_viridis_c() +
  labs(title = "Levend geborenen per 1000 inwoners, 2017", fill = "") +
  theme_void()


# reading geoJSON with leaflet

NLgemeentes <- geojson_read("/Users/Daniel/Downloads/townships.geojson", what = "sp")

leaflet::leaflet(NLgemeentes) %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(weight = 1, color = "black")

print(NLgemeentes@data$code)


boundaries <- geojson_read("https://opendata.arcgis.com/datasets/e1f0dd70abcb4fceabbc43412e43ad4b_0.geojson", what = "sp")

boundaries %>% 
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(weight = 1, color = "green", opacity = 0.9)

# look how to join Spatial Object with df

boundaries_income <- boundaries@data %>% 
  left_join(data, by = c("Gemeentecode" = "statcode"))



boundaries_income <- geo_join(boundaries, data, "Gemeentecode", "statcode", how = "left")

class(boundaries_income)

geocode("Netherlands")

# display mean income in Netherlands

NL_pal <- colorNumeric("Blues", domain = log(boundaries_income@data$GemiddeldInkomenPerInwoner_66))
NL_pal_2 <- colorNumeric("YlGn", domain = boundaries_income@data$HuishoudensTotaal_28)
NL_pal_3 <- colorNumeric("Reds", domain = boundaries_income@data$AantalInwoners_5)

popup_inkom = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Mean Income: ",
                    boundaries_income$GemiddeldInkomenPerInwoner_66) %>% map(htmltools::HTML)
popup_huishouden = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Huishouden Total: ",
                         boundaries_income$HuishoudensTotaal_28) %>% map(htmltools::HTML)
popup_inwoner <- str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Inwoner Total: ",
                       boundaries_income$AantalInwoners_5) %>% map(htmltools::HTML)

boundaries_income %>% 
  leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>%
  addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal(log(GemiddeldInkomenPerInwoner_66)),
              label = ~popup_inkom,
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inkomen") %>% 
  addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal_2(HuishoudensTotaal_28),
              label = ~popup_huishouden,
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Huishouden") %>% 
  addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal_3(AantalInwoners_5),
              label = ~popup_inwoner,
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inwoner") %>% 
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Inkomen", "Huishouden", "Inwoner")) %>%  showGroup("Carto") %>% hideGroup(c("Huishouden", "Inwoner"))
 
# using shiny

# multiply income by 1000?

library(shiny)

ui <- fluidPage(
  leafletOutput("map"),
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    NL_pal <- colorNumeric("Blues", domain = boundaries_income@data$GemiddeldInkomenPerInwoner_66)
    NL_pal_2 <- colorNumeric("YlGn", domain = boundaries_income@data$HuishoudensTotaal_28)
    NL_pal_3 <- colorNumeric("Reds", domain = boundaries_income@data$AantalInwoners_5)
    
    popup_inkom = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Mean Income: ",
                        boundaries_income$GemiddeldInkomenPerInwoner_66) %>% map(htmltools::HTML)
    popup_huishouden = str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Huishouden Total: ",
                             boundaries_income$HuishoudensTotaal_28) %>% map(htmltools::HTML)
    popup_inwoner <- str_c("<strong>", boundaries_income$Gemeentenaam, "</strong>", "<br/>", "Inwoner Total: ",
                           boundaries_income$AantalInwoners_5) %>% map(htmltools::HTML)
    
    boundaries_income %>% 
      leaflet() %>% 
      addTiles(group = "OSM") %>% 
      addProviderTiles("CartoDB", group = "Carto") %>% 
      addProviderTiles("Esri", group = "Esri") %>%
      addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal(GemiddeldInkomenPerInwoner_66),
                  label = ~popup_inkom,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inkomen") %>% 
      addLegend(pal = NL_pal, values = ~boundaries_income@data$GemiddeldInkomenPerInwoner_66,
                group = "Inkomen", position = "bottomleft", title = "Gemiddeld Inkomen Per Inwoner",
                labFormat = labelFormat(prefix = "€"), opacity = 1) %>% 
      addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal_2(HuishoudensTotaal_28),
                  label = ~popup_huishouden,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Huishouden") %>% 
      addLegend(pal = NL_pal_2, values = ~boundaries_income@data$HuishoudensTotaal_28,
                  group = "Huishouden", position = "bottomleft", title = "Aantal Huishouden", opacity = 1) %>% 
      addPolygons(weight = 1,fillOpacity = 0.5, color = ~NL_pal_3(AantalInwoners_5),
                  label = ~popup_inwoner,
                  highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = "Inwoner") %>% 
      addLegend(pal = NL_pal_3, values = ~boundaries_income@data$AantalInwoners_5,
                group = "Inwoner", position = "bottomleft", title = "Aantal Inwoners", opacity = 1) %>% 
      addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                       overlayGroups = c("Inkomen", "Huishouden", "Inwoner")) %>%  showGroup("Carto") %>%
      hideGroup(c("Huishouden", "Inwoner")) 
  })
}

shinyApp(ui, server)
