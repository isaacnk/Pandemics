library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(leaflet)
library(rgdal)
library(sf)
library(lubridate)
library(grDevices)
library(plotly)
library(data.table)
library(raster)
library(scales)

#### Data Load Start ####
all.data <- st_read("Dashboarddata.shp")

#### Data Load End ####


#### Pal Def Start ####
vc_pal <- colorNumeric(
    palette = c("green", "yellow", "red"),
    domain = all.data$V_C_Avg
  )

pc_pal <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = all.data$P_C_Avg
)

ac_pal <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = all.data$A_C_Avg
)

change_pal <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = all.data$P_C_Dff
)


#### Pal Def End ####

# Define UI for application
ui <- dashboardPage(
   
    ##### LOGO START #####
    dashboardHeader(title = shinyDashboardLogoDIY(boldText = "Covid and Crime",
                                                  mainText = "Chicago",
                                                  badgeText = "",
                                                  textSize = 16,
                                                  badgeTextColor = "white",
                                                  badgeTextSize = 2,
                                                  badgeBackColor = "",
                                                  badgeBorderRadius = 3)
    ),
    ##### LOGO END #####
    
    dashboardSidebar(sidebarMenu(id = "sidebar",
                                 menuItem("Home", tabName = "home", icon = icon("home")),
                                 menuItem("About", tabName = "about", icon = icon("info"))
                                 #menuItem("Crime", tabName = "crime", icon = icon("shoe-prints")),
                                 #menuItem("Covid-19", tabName = "covid", icon = icon("medkit"))# do as home tab
                                 )
    ),
    
    dashboardBody(
      
      tabItems(
        
        ##### HOME START #####
        tabItem(tabName = "home",
                fluidRow(
                  box(width = 12,
                      h1("Home", align = "center")
                  )),
                fluidRow(
                  box(width = 12,
                      textOutput("hometext"))),
                  fluidRow(
                    box(width = 6,
                        selectInput(
                          inputId = "homeinput1", 
                          label = c("Select 1st Variable to be Mapped:"),
                          choices = c("All Crime April 2020" = "A_C_20", 
                                      "All Crime April Avg." = "A_C_Avg",
                                      "% Change All Crime" = "A_C_Dff",
                                      "Violent Crime April 2020" = "V_C_20",
                                      "Violent Crime April Avg." = "V_C_Avg", 
                                      "% Change Violent Crime" = "V_C_Dff", 
                                      "Property Crime April 2020" = "P_C_20",
                                      "Property Crime April Avg." = "P_C_Avg", 
                                      "% Change Property Crime" = "P_C_Dff",
                                      "April Covid Cases" = "new_cases", 
                                      "April Covid Deaths" = "new_deaths", 
                                      "April Covid Cases per 1000" = "cases_per", 
                                      "April Covid Deaths per 1000" = "deaths_per"
                          ),
                          selected = "A_C_20"
                        )),
                    
                    box(width = 6,
                        selectInput(
                          inputId = "homeinput2", 
                          label = c("Select 2nd Variable to be Mapped:"),
                          choices = c("All Crime April 2020" = "A_C_20", 
                                      "All Crime April Avg." = "A_C_Avg",
                                      "% Change All Crime" = "A_C_Dff",
                                      "Violent Crime April 2020" = "V_C_20",
                                      "Violent Crime April Avg." = "V_C_Avg", 
                                      "% Change Violent Crime" = "V_C_Dff", 
                                      "Property Crime April 2020" = "P_C_20",
                                      "Property Crime April Avg." = "P_C_Avg", 
                                      "% Change Property Crime" = "P_C_Dff",
                                      "April Covid Cases" = "new_cases", 
                                      "April Covid Deaths" = "new_deaths", 
                                      "April Covid Cases per 1000" = "cases_per", 
                                      "April Covid Deaths per 1000" = "deaths_per"
                          ),
                          selected = "A_C_Avg"
                        ))
                  ),
                  fluidRow(
                    box(width = 6,
                        leafletOutput("homemap1", height = 500)),
                    box(width = 6,
                        leafletOutput("homemap2", height = 500)))
        ),
        ##### HOME END #####
        
        ##### ABOUT START #####
        tabItem(tabName = "about",
                fluidRow(
                  box(width = 12,
                      h1("About", align = "center")
                  )),
                fluidRow(
                  box(width = 12,
                      textOutput("abouttext1"))),
                fluidRow(
                  box(width = 12,
                      textOutput("abouttext2")))
                )
        ##### ABOUT END #####
        
        ##### CRIME START #####
        # tabItem(tabName = "crime",
        #         fluidRow(
        #           box(width = 12,
        #               h1("Crime", align = "center")
        #           )),
        #         fluidRow(
        #           box(width = 6,
        #               selectInput(
        #                 inputId = "crimeinput1", 
        #                 label = c("Select 1st Variable to be Mapped:"),
        #                 choices = c("All Crime April 2020" = "A_C_20", 
        #                           "All Crime April Avg." = "A_C_Avg",
        #                           "% Change All Crime" = "A_C_Dff",
        #                           "Violent Crime April 2020" = "V_C_20",
        #                           "Violent Crime April Avg." = "V_C_Avg", 
        #                           "% Change Violent Crime" = "V_C_Dff", 
        #                           "Property Crime April 2020" = "P_C_20",
        #                           "Property Crime April Avg." = "P_C_Avg", 
        #                           "% Change Property Crime" = "P_C_Dff",
        #                           "April Covid Cases" = "new_cases", 
        #                           "April Covid Deaths" = "new_deaths", 
        #                           "April Covid Cases per 1000" = "cases_per", 
        #                           "April Covid Deaths per 1000" = "deaths_per"
        #                 ),
        #                 selected = "A_C_20"
        #               )),
        #           
        #           box(width = 6,
        #               selectInput(
        #                 inputId = "crimeinput2", 
        #                 label = c("Select 2nd Variable to be Mapped:"),
        #                 choices = c("All Crime April 2020" = "A_C_20", 
        #                             "All Crime April Avg." = "A_C_Avg",
        #                             "% Change All Crime" = "A_C_Dff",
        #                             "Violent Crime April 2020" = "V_C_20",
        #                             "Violent Crime April Avg." = "V_C_Avg", 
        #                             "% Change Violent Crime" = "V_C_Dff", 
        #                             "Property Crime April 2020" = "P_C_20",
        #                             "Property Crime April Avg." = "P_C_Avg", 
        #                             "% Change Property Crime" = "P_C_Dff",
        #                             "April Covid Cases" = "new_cases", 
        #                             "April Covid Deaths" = "new_deaths", 
        #                             "April Covid Cases per 1000" = "cases_per", 
        #                             "April Covid Deaths per 1000" = "deaths_per"
        #                 ),
        #                 selected = "A_C_Avg"
        #               ))
        #         ),
        #         fluidRow(
        #           box(width = 6,
        #               leafletOutput("homemap1", height = 500)),
        #           box(width = 6,
        #               leafletOutput("homemap2", height = 500)))
        # ),
        ##### CRIME END #####
        
        ##### COVID END ##### 
        
        
        
   )))



server <- function(input, output) {
   
#### Old Home ####
  # output$homecrimemap <- renderLeaflet({
  #   
  #   pal <- colorNumeric(
  #     palette = c("green", "yellow", "red"),
  #     domain = all.data$A_C_Dff
  #   )
  #   
  #   
  #   leaflet(all.data) %>%
  #     addProviderTiles("Esri.WorldGrayCanvas") %>%
  #     addPolygons(data = all.data, 
  #                 fillColor = ~pal(A_C_Dff),
  #                 fillOpacity  = 1, 
  #                 stroke = TRUE,
  #                 color = "black",
  #                 opacity = 0.5,
  #                 weight = 1,
  #                 popup = paste(paste("Zipcode", all.data$zip, sep = ": "), paste("% Change in Crime", all.data$A_C_Dff, sep = ": "), sep = "<br>"),
  #                 highlight = highlightOptions(
  #                   weight = 2)) %>%
  #     leaflet::addLegend(title = "% Change Crime", 
  #                        pal = pal, 
  #                        values = ~A_C_Dff,
  #                        position = "bottomleft",
  #                        na.label = "Missing")
  # })
  # 
  # output$homecovidmap <- renderLeaflet({
  #   
  #   homecov.pal <- colorNumeric(
  #     palette = c("green", "yellow", "red"),
  #     domain = all.data$cases_per
  #   )
  #   
  #   
  #   leaflet(all.data) %>%
  #     addProviderTiles("Esri.WorldGrayCanvas") %>%
  #     addPolygons(data = all.data, 
  #                 fillColor = ~homecov.pal(cases_per),
  #                 fillOpacity  = 1, 
  #                 stroke = TRUE,
  #                 color = "black",
  #                 opacity = 0.5,
  #                 weight = 1,
  #                 popup = paste(paste("Zipcode", all.data$zip, sep = ": "), paste("April Covid Cases per 1000", all.data$cases_per, sep = ": "), sep = "<br>"),
  #                 highlight = highlightOptions(
  #                   weight = 2)) %>%
  #     leaflet::addLegend(title = "April Covid Cases per 1000", 
  #                        pal = homecov.pal, 
  #                        values = ~cases_per,
  #                        position = "bottomleft",
  #                        na.label = "Missing")
  # })
  # 
#### Old Home ####
  
  output$hometext <- renderText({
    c("Welcome to the Chicago Covid & Crime Dashboard. This R Shiny Application offers
a chance to interactively explore zip-code-level crime and coronavirus data from April 2020. 
All data used to create this dashboard is publicly available on the Chicago Open Data Portal. 
For a more in-depth discussion of the data processing workflow, see the attached process paper. 
Scroll down for simple side-by-side maps to compare a number of stratified crime and Covid-related
variables. View the About page to read the process paper and a discussion of relevant findings in the data.
The R Scripts used to process the data and create this dashboard can be found at https://github.com/isaacnk/Pandemics")
      })
  
  output$homemap1 <- renderLeaflet({
    
    in.crime1 <- input$homeinput1 
    in.crime1.vals <- st_drop_geometry(all.data[,in.crime1])
    in.crime1.vals <- unlist(in.crime1.vals)

    if(grepl("Dff", in.crime1)) {
      crime1.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$P_C_Dff
      )
    } 
    else if (grepl("P_C", in.crime1)) {
      crime1.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$P_C_Avg
      )
    } else if (grepl("A_C", in.crime1)) {
      crime1.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$A_C_Avg
      ) 
    } else if (grepl("V_C", in.crime1)) {
      crime1.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$V_C_Avg
      )
    } else {
      crime1.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = in.crime1.vals
      )
    }
    
    leaflet(all.data,
            options = leafletOptions(zoomControl = F)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(data = all.data, 
                  fillColor = crime1.pal(in.crime1.vals),
                  fillOpacity  = 1, 
                  stroke = TRUE,
                  color = "black",
                  opacity = 0.5,
                  weight = 1,
                  popup = paste(paste("Zipcode", all.data$zip, sep = ": "), paste(in.crime1, in.crime1.vals, sep = ": "), sep = "<br>"),
                  highlight = highlightOptions(
                    weight = 2)) %>%
      leaflet::addLegend(title = in.crime1, 
                         pal = crime1.pal, 
                         values = ~in.crime1.vals,
                         position = "bottomleft",
                         na.label = "Missing") 
      
  })
  
  output$homemap2 <- renderLeaflet({
    
    in.crime2 <- input$homeinput2
    in.crime2.vals <- st_drop_geometry(all.data[,in.crime2])
    in.crime2.vals <- unlist(in.crime2.vals)
    
    if(grepl("Dff", in.crime2)) {
      crime2.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$P_C_Dff
      )
    } 
    else if (grepl("P_C", in.crime2)) {
      crime2.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$P_C_Avg
      )
    } else if (grepl("A_C", in.crime2)) {
      crime2.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$A_C_Avg
      ) 
    } else if (grepl("V_C", in.crime2)) {
      crime2.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = all.data$V_C_Avg
      )
    } else {
      crime2.pal <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = in.crime2.vals
      )
    }
    
    leaflet(all.data,
            options = leafletOptions(zoomControl = F)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(data = all.data, 
                  fillColor = crime2.pal(in.crime2.vals),
                  fillOpacity  = 1, 
                  stroke = TRUE,
                  color = "black",
                  opacity = 0.5,
                  weight = 1,
                  popup = paste(paste("Zipcode", all.data$zip, sep = ": "), paste(in.crime2, in.crime2.vals, sep = ": "), sep = "<br>"),
                  highlight = highlightOptions(
                    weight = 2)) %>%
      leaflet::addLegend(title = in.crime2, 
                         pal = crime2.pal, 
                         values = ~in.crime2.vals,
                         position = "bottomleft",
                         na.label = "Missing") 
    
  })
  
  output$abouttext1 <- renderText({
    c("A Process Paper detailing the data processing and dashboard creation can be found at:
      https://docs.google.com/document/d/1zcYnSIfh03RjO9gy9QxWY4MNaghDnD2GSvZY_1bIr1g/edit?usp=sharing")
  })
  
  output$abouttext2 <- renderText({
    c("A more in-depth analysis of the data and discussion of relevant findings can be found at: 
      https://docs.google.com/document/d/1innbBRj7zucb7BBGQGol_ctBP-ONvAX7zRP10iFBsXA/edit?usp=sharing")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

