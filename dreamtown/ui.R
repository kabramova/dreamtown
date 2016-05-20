library(shiny)
library(leaflet)
library(RColorBrewer)


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(
        # Include custom CSS
        includeCSS("styles.css")
    ),
    titlePanel(
        h1("Dream Town Explorer", style = "padding:0px 0px 0px 10px")),
    helpText(
        p("Play with the sliders to choose a city that",
             "satisfies your constraints.",
             "Click on the city marker to get more information.",
            style = "padding:0px 0px 0px 10px")),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = "auto",
                  
                  h3("Pick your constraints"),
                  br(),
                  sliderInput("sun_hours",
                              label="Annual number of sunshine hours:",
                              min = 0,
                              max = 4100,
                              value = c(0, 4100),
                              step = 100),
                  br(),
                  sliderInput("pollution",
                              label="Pollution (annual amount of PM10):",
                              min = 0,
                              max = 600,
                              value = c(0, 600),
                              step = 100),
                actionButton("recalc", "Refresh Map"),
                br(),
                plotOutput("hist", height = 200)
    )
)


