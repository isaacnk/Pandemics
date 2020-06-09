library(sf)
library(tidyverse)
library(lubridate)

covid <- read_csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")

covid$`Week Start` <- mdy(covid$`Week Start`)

covid <- covid %>%
  filter(month(covid$`Week Start`) == 4)

names(covid)

covid.copy <- covid %>%
  group_by(`ZIP Code`) %>%
  summarise(new_cases = sum(`Cases - Weekly`, na.rm = T),
            new_tests = sum(`Tests - Weekly`, na.rm = T),
            new_deaths = sum(`Deaths - Weekly`, na.rm = T),
            population = mean(Population))

covid.copy  <- covid.copy[-which(covid.copy$`ZIP Code` == "Unknown"),]
View(covid.copy)

zips <- st_read("Boundaries - ZIP Codes")
zips$zip <- as.character(zips$zip)

zip.match <- base::match(zips$zip, covid.copy$`ZIP Code`)

zips$new_cases <- 0
zips$new_tests <- 0
zips$new_deaths <- 0
zips$population <- 0

zips[,6:9] <- covid.copy[zip.match,2:5]


tm_shape(zips) +
  tm_polygons(col = "population", style = "jenks")
View(zips)

st_write(zips, "Covid_Zip.shp")



pal <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = crime$A_C_Dff
)


leaflet(crime) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = crime, 
              fillColor = ~pal(A_C_Dff),
              fillOpacity  = 1, 
              stroke = TRUE,
              color = "black",
              opacity = 1,
              weight = 1,
              highlight = highlightOptions(
                weight = 2)) %>%
  leaflet::addLegend(title = "% Change Crime", pal = pal, values = ~A_C_Dff)












