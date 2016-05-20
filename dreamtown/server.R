library(shiny)
library(leaflet)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(ggplot2)

locations <- readRDS("data/alldata.Rds")
locations$ids <- rownames(locations)
yeardata <- readRDS("data/yeardata.Rds")
smax <- max(yeardata[,2:13])
ymax <- round_any(smax, 10, f=ceiling)

ylong <- melt(yeardata)

server <- function(input, output, session) {

    # Recalculate data in limits input by the user when they click the
    # recalculate button
    filteredData <- eventReactive(input$recalc, {
        locations[locations$Sunshine >= input$sun_hours[1] & 
                      locations$Sunshine <= input$sun_hours[2] &
                      locations$PM10.Annual >= input$pollution[1] &
                      locations$PM10.Annual <= input$pollution[2],]
    }, ignoreNULL = FALSE)
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically.
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Thunderforest.Landscape") %>%
            setView(lng = 0, lat = 0, zoom=2)
    })
    
    colorpal <- reactive({
        colorNumeric("Oranges", locations$Sunshine)
    })
    
    
    # Incremental changes to the map should be performed in an observer. 
    # Each independent set of things that can change should be managed in its 
    # own observer.
    observe({
        pal <- colorpal()
        if(nrow(filteredData())==0) {leafletProxy("map") %>% clearShapes()} 
        else {
            leafletProxy("map", data=filteredData()) %>%
                clearMarkers() %>%
                addCircleMarkers(~Lng, ~Lat, 
                           color = ~pal(Sunshine), radius=8,
                           stroke = FALSE, fillOpacity = 1, layerId=~ids)
        }
    })
    
    
    # Construct text to show at each clicked popup
    showPopup <- function(tid, lat, lng) {
        selectedPoint <- locations[locations$ids == tid,]
        content <- as.character(tagList(
            tags$h4(paste(selectedPoint$Country, selectedPoint$City, sep=", ")),
            tags$br(),
            sprintf("Annual sunshine hours: %s", selectedPoint$Sunshine),
            tags$br(),
            sprintf("Annual amount of PM10: %s", round(selectedPoint$PM10.Annual, 2)),
            tags$br(),
            sprintf("Annual amount of PM2.5: %s", round(selectedPoint$PM25.Annual, 2))
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content)
    }
    
    
    # When user clicks on point, show popup and draw histogram for that point
    observe({
        leafletProxy("map") %>% clearPopups()
        click <- input$map_marker_click
        if(is.null(click))
            return()
        
        isolate({
            showPopup(click$id, click$lat, click$lng)
        })
        
        output$hist <- renderPlot({
            ggplot(ylong[ylong$ids==click$id,], aes(x=variable, y=value)) + 
                geom_bar(stat="identity") + xlab("Month") + ylab("Hours") + 
                ggtitle("Sunshine hours by month") + ylim(0, ymax)
        })
    })
    

}