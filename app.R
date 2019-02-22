## app.R ##
library(shiny)
library(tidyverse)
library(leaflet)
library(markdown)

citydata <- read_csv("500_Cities__City-level_Data__GIS_Friendly_Format___2018_release.csv")
citydata <- tidyr::separate(data=citydata,
                            col=Geolocation,
                            into=c("Latitude", "Longitude"),
                            sep=",",
                            remove=FALSE)

citydata$Latitude <- stringr::str_replace_all(citydata$Latitude, "[(]", "")
citydata$Longitude <- stringr::str_replace_all(citydata$Longitude, "[)]", "")

citydata$Latitude <- as.numeric(citydata$Latitude)
citydata$Longitude <- as.numeric(citydata$Longitude)

citydata$color <- ""
citydata$rank <- 1 


##########

ui <- bootstrapPage(
  
  tags$style(
    type = "text/css", "html, body {width:100%;height:100%}"
  ),
  
  leafletOutput(
    "mymap", width = "100%", height = "100%"
  ),
  
  absolutePanel(width = 300,top = 5, right = 5,style="padding: 8px; border: 1px solid #CCC; background: #EEE; opacity: .95",draggable=TRUE,
                
                wellPanel(
                  HTML(markdownToHTML(fragment.only=TRUE, text=c(
                    "Welcome! <br> <br> This app lets you view a selection of the 
                    most populous cities in the United States 
                    and how they rank among each other in 
                    several different health conditions. <br><br>
                    Markers are according to percentile."
                  ),style="background: #FFF")),
                    
                  HTML(markdownToHTML(fragment.only=TRUE, text=c(
                    "Green: <75th percentile<br> Orange: >75th percentile<br> Red: >90th percentile"
                  ),style="background: #FFF; float: left; text-align: right"))),
                
                
                selectInput("healthstat","Select a health statistic:"
                            ,choices=c("Arthritis"="ARTHRITIS_AdjPrev","Binge Drinking"="BINGE_AdjPrev","High Blood Pressure"="BPHIGH_AdjPrev"
                                       ,"Cancer"="CANCER_AdjPrev","Current Asthma"="CASTHMA_AdjPrev", "Coronary Heart Disease"="CHD_AdjPrev"
                                       ,"Smoking"="CSMOKING_AdjPrev","Diabetes"="DIABETES_AdjPrev","Obesity"="OBESITY_AdjPrev","Stroke"="STROKE_AdjPrev"
                            )
                            ,multiple=FALSE, selectize=TRUE),
                
                sliderInput("poprange","Population range of visible cities:",40000,3000000,value=c(40000,3000000),step=20000,width=250),
                
                wellPanel(
                  HTML(markdownToHTML(fragment.only=TRUE, text=c(
                  "This project utilizes data from <br> <a href=https://chronicdata.cdc.gov/500-Cities/500-Cities-City-level-Data-GIS-Friendly-Format-201/dxpw-cm5u>the CDC's 500 Cities Project</a>")
                )
              )
            )
          )
)

server <- function(input,output){
  
  
  #render map  
  output$mymap <- renderLeaflet({
    
    
    leaflet(citydata) %>%
      setMaxBounds(lng1 = -162, lat1 = 64, lng2 = -44, lat2 = 17) %>%
      addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(noWrap = TRUE, minZoom = 4, maxZoom = 8)) %>%
      addProviderTiles(providers$Stamen.TonerLines,options = providerTileOptions(noWrap = TRUE, minZoom = 4, maxZoom = 8)) %>%
      addProviderTiles(providers$Stamen.TonerLabels,options = providerTileOptions(noWrap = TRUE, minZoom = 4, maxZoom = 8))
      
  })
  
  #Observer to update map markers when a new statistic is selected 
  observe({
    proxy <- leafletProxy("mymap", data = citydata)
    
    
    #Sort and set marker colors
    citydata <<- arrange(citydata,desc(get(input$healthstat)))
    citydata[1:50,]$color <<- c("red")
    citydata[51:125,]$color <<- c("orange")
    citydata[126:500,]$color <<- c("green")
    citydata$rank <<- 1:nrow(citydata)
    citydata$rank <<- (100 - (round(citydata$rank /5, digits = 0)))
    citydata$Population2010 <<- ifelse((citydata$Population2010 > 2999999), 2999999, citydata$Population2010)
    citydatasub <<- subset(citydata,(Population2010 > input$poprange[1]))
    citydatasub <<- subset(citydatasub,(Population2010 < input$poprange[2]))
    
    #Assign colors to markers    
    icons <- awesomeIcons(
      icon = 'ios-close',
      markerColor = citydatasub$color
    )
    
    #Set popup text        
    popups <- paste(
      "City: ", citydatasub$PlaceName,", ",citydatasub$StateAbbr, "<br>",
      "Prevalence: ", citydatasub[[input$healthstat]],"%","<br>",
      "Percentile: ", citydatasub$rank,"%",sep=""
    )
    
    #Clear existing markers and draw new ones   
    proxy %>% clearMarkers()
    proxy %>% addAwesomeMarkers(citydatasub$Longitude, citydatasub$Latitude, icon=icons, popup=popups)
    
  })
  
}




shinyApp(ui, server)

