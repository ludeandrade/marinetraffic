#### --- Server

server <- function(input, output, session) {
  
  # variables
  maxCoordinates <- reactiveVal(data.frame())
  maxDistance <- reactiveVal(numeric())
  dataVesselReactive <- reactiveVal(data.frame())

  # select input
  dropDownServer(id = "vessels", select = c("ship_type","SHIPNAME"))
  
  # data 4 longest distance kpi & map
  observeEvent(input[["vessels-dropDown2"]], {
    
    dataVessel <- data %>% 
      filter(SHIPNAME==input[["vessels-dropDown2"]]) %>% 
      arrange(DATETIME) %>% 
      mutate(prevLAT=c(tail(LAT, -1), NA)) %>% 
      mutate(prevLON=c(tail(LON, -1), NA))
    
    if(nrow(dataVessel)>1) {
      maxDistance(0)
      
      for(i in 1:nrow(dataVessel)){
        distance <- distm(c(dataVessel$prevLON[i], dataVessel$prevLAT[i]), c(dataVessel$LON[i], dataVessel$LAT[i]), fun = distHaversine)
        
        if(!is.na(distance) && distance >= maxDistance()) {
          maxDistance(distance)
          maxPosi <- i
        }
        
      }
      
      prev <- dataVessel[maxPosi,c("prevLON","prevLAT","date")] %>% rename("LON"="prevLON") %>% rename("LAT"="prevLAT") %>% mutate(reference="Beginning of the sailing")
      maxCoordinates(rbind(prev, dataVessel[maxPosi,c("LON","LAT","date")] %>% mutate(reference="End of the sailing")))
      
      dataVesselReactive(dataVessel)
      
    } else {
      
      maxDistance(0)
      maxCoordinates(NULL)
      
    }
  })
  
  # kpi observations
  output$observations <- renderUI({
    
    message_box(
      header =  formatC(nrow(dataVesselReactive()), big.mark = ","),
      content = paste("Number of observations"),
      icon_name = "wifi",
      class = "ui icon message"
    )
    
  })
  
  # kpi total distance
  output$total <- renderUI({
    
    if(nrow(dataVesselReactive())>0) {

      dataVessel <- dataVesselReactive()
      
      distance <- distm(c(dataVessel$LON[dataVessel$DATETIME==min(dataVessel$DATETIME)][1], 
                          dataVessel$LAT[dataVessel$DATETIME==min(dataVessel$DATETIME)][1]), 
                        c(dataVessel$LON[dataVessel$DATETIME==max(dataVessel$DATETIME)][1], 
                          dataVessel$LAT[dataVessel$DATETIME==max(dataVessel$DATETIME)][1]), 
                        fun = distHaversine)
      
      message_box(
        header =  paste(formatC(distance, big.mark = ","), "kilometers"),
        content = paste("Distance between first and last observations"),
        icon_name = "map outline",
        class = "ui icon message"
      )
    }
  })
  
  # map max distance two observations
  output$map <- renderLeaflet({
    
    if(!is.null(maxCoordinates())) {
      
      maxCoord <- maxCoordinates()
      
      # popup
      popUp <- paste0("<b>", maxCoord$reference, ": </b><br>", format(as_date(maxCoord$date),"%m/%d/%Y"),
                      "<br><b>Longitude: </b><br>", maxCoord$LON,
                      "<br><b>Latitude: </b><br>", maxCoord$LAT)
      
      tag.map.title.vazio <- tags$style(HTML("
            .leaflet-control.map-title-vazio {
            font-size: 16px;
            }"))
      
      title <- tags$div(tags$h4(tags$b("LONGEST DISTANCE BETWEEN TWO CONSECUTIVE OBSERVATIONS")))
      max <- tags$div(tags$b(tags$h4(paste(formatC(maxDistance(), big.mark = ","), "meters"))))
      
      # map icon
      mapIcon <- awesomeIcons(
        icon = 'map marker',
        iconColor = "black",
        library = 'ion',
        markerColor = "black"
      )
      
      leaflet(maxCoord) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        clearImages() %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial=FALSE)) %>%
        addMiniMap(
          tiles = providers$CartoDB.Positron,
          toggleDisplay = TRUE) %>% 
        addAwesomeMarkers(lng=~LON, lat=~LAT, icon = mapIcon, popup = popUp) %>% 
        addControl(title, position = "topright", className="map-title-vazio") %>% 
        addControl(max, position = "topright")
      
    } else {
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
  # bar plor
  output$speed <- renderPlotly({
    
    if(nrow(dataVesselReactive())>0) {
      
      dataVessel <- dataVesselReactive()
      
      dataSpeed <- dataVessel %>% group_by(date) %>% dplyr::summarise(SPEED2 = mean(SPEED)) %>% mutate(date = as.character(date))
      
      fig <- plot_ly(data = dataSpeed, 
                     x = ~SPEED2,
                     y = ~date,
                     type = "bar",
                     opacity = 0.6,
                     marker = list(color = "black"),
                     text = paste(formatC(dataSpeed$SPEED2, digits = 2), "kn"),
                     textposition = "outside"
      ) 
      
      fig %>% style(hoverinfo = 'none')
      
      fig %>% layout(
        title = list(text = "AVERAGE SPEED BY DATE (knots)", font = list(size = 12)),
        paper_bgcolor = "rgba(255,255,255,1)",
        plot_bgcolor = "rgba(255,255,255,1)",
        transition = list(duration = 1500),
        xaxis = list(title = "", font = list(size = 10)),
        yaxis = list(title = "", font = list(size = 10), 
                     type = "-", 
                     tickvals = dataSpeed$date, 
                     ticktext = format(as_date(dataSpeed$date), "%m/%d/%Y"),
                     tickcolor = darkGrey)
      )
    }
  })
  
  # description
  output$description <- renderUI({
    
    if(nrow(dataVesselReactive())>0) {
      
      dataVessel <- (dataVesselReactive() %>% 
        filter(as_datetime(DATETIME)==max(as_datetime(DATETIME))))[1,]
      
      lu2<<-dataVessel
      
      tags$div( tags$b(paste0(input[["vessels-dropDown2"]], "'S MOST RECENT INFORMATION:")), tags$br(),
                tags$ul(
                  tags$li(tags$b("ID:  "), dataVessel$SHIP_ID, tags$br()),
                  tags$li(tags$b("WIDTH:  "), dataVessel$WIDTH, tags$br()),
                  tags$li(tags$b("FLAG:  "), dataVessel$FLAG, tags$br()),
                  tags$li(tags$b("REPORTING PORT:  "), toupper(dataVessel$port), tags$br()),
                  tags$li(tags$b("DESTINATION:  "),  toupper(dataVessel$DESTINATION), tags$br()),
                  tags$li(tags$b("SITUATION:  "), if(dataVessel$is_parked==0) "SAILING" else "PARKED")
                )
      )
    }
  })
  
  # modal INFO
  observeEvent(input$infoButton, {
    create_modal(modal(
      id = "info",
      title = "Info",
      tags$div(tags$h4("All the data are provided by Automatic Identification System (AIS) fitted in every 
                 ship and collected by MarineTraffic network of shore based receivers."), 
               tags$h4("Access the link below for more information."),
               tags$br(),
               tags$a(
                 href="https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/", 
                 "Marine Traffic website"
               )
      )
    ))
  })

} # server