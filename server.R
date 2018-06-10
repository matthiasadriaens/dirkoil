library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

vars <- c("id","lon","lat","fueltype","stationtype","name","status")
terminals <- read.csv("data/terminals.csv")
terminals$fueltype <- rep(0,nrow(terminals))
terminals$stationtype <- rep("terminal",nrow(terminals))
terminals$fueltype <- rep(0,nrow(terminals))
terminals$status <- rep("active",nrow(terminals))
term <- terminals[,c(1,3,4,5,6,2,7)]
names(term) <- vars
stations <- term

load("data/lng.Rdata")
lngStations$country <- NULL
lngStations$id <- c(1:nrow(lngStations))
lngStations$stationtype <- rep("lng",nrow(lngStations))
lng <- lngStations[,c(6,5,4,3,7,1,2)]
names(lng) <- vars
stations <- rbind(stations,lng)

load("data/cng.Rdata")
cngStations$country <- NULL
cngStations$stationtype <- rep("cng",nrow(cngStations))
cngStations$id <- c(1:nrow(cngStations))
cng <- cngStations[,c(7,5,4,3,6,1,2)]
names(cng) <- vars
stations <- rbind(stations,cng)

load("data/petrol.Rdata")
petrolStations$stationtype <- rep("petrol",nrow(petrolStations))
petrolStations$status <-  rep("active",nrow(petrolStations))
petrolStations$id <- c(1:nrow(petrolStations))
petrol <- petrolStations[,c(7,3,2,4,5,1,6)]
names(petrol) <- vars
stations <- rbind(stations,petrol)
stations$id <- c(1:nrow(stations))

#ASSIGN MAP COLORS TO THE STATIONS DATAFRAME
stations$mapcolor <- ifelse(stations$stationtype == "terminal","#ff0000",
                            ifelse(stations$stationtype == "lng","#feb236",
                                   ifelse(stations$stationtype == "cng", "#00ffbf",
                                          ifelse(stations$stationtype == "petrol","#622569","null"))))
palcustom <- c("#ff0000","#feb236","#00ffbf","#622569")
labcustom <- c("Termial","#LNG","CNG","Petrol")

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 4.9, lat = 50.45, zoom = 8.2)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  leafletProxy("map",data = stations)%>%
    addLegend("bottomright", colors = palcustom, labels = labcustom,
              title = "Station Legend",
              opacity = 1
    )

  # This observer is responsible for maintaining the circles and legend,
  # Color of dots based on type of station
  # Selection given for requested data
  observe({

    if(input$petrol == TRUE & input$cng == TRUE & input$lng == TRUE){
      leafletProxy("map",data = stations)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    } else if(input$petrol == FALSE & input$cng == FALSE & input$lng == FALSE){
      stationssubset <- stations[stations$stationtype == "terminal",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    } else if(input$petrol == TRUE & input$cng == FALSE & input$lng == FALSE){
      stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }else if(input$petrol == FALSE & input$cng == TRUE & input$lng == FALSE){
      stationssubset <- stations[stations$stationtype == "cng" | stations$stationtype == "terminal",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }else if(input$petrol == FALSE & input$cng == FALSE & input$lng == TRUE){
      stationssubset <- stations[stations$stationtype == "lng" | stations$stationtype == "terminal",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }else if(input$petrol == TRUE & input$cng == TRUE & input$lng == FALSE){
      stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal" | stations$stationtype == "cng",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }else if(input$petrol == TRUE & input$cng == FALSE & input$lng == TRUE){
      stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal" | stations$stationtype == "lng",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }else if(input$petrol == FALSE & input$cng == TRUE & input$lng == TRUE){
      stationssubset <- stations[stations$stationtype == "cng" | stations$stationtype == "terminal" | stations$stationtype == "lng",]
      leafletProxy("map",data = stationssubset)%>%
        clearShapes() %>%
        addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
    }


  })

  # Show a popup at the given location
  showStationPopup <- function(id, lat, lng) {
    station <- stations[stations$id == id,]

    if(station$stationtype == "terminal"){
      content <- as.character(tagList(
        tags$h3("Terminal info"),
        tags$h6("Name: ",station$name),
        tags$h6("Longitude: ",station$lon),
        tags$h6("Latitude: ",station$lat)
      ))
    }else{
      content <- as.character(tagList(
        tags$h3("Station info"),
        tags$h6("Name: ",station$name),
        tags$h6("Fueltype: ", station$fueltype),
        tags$h6("Status: ",station$status),
        tags$h6("Longitude: ",station$lon),
        tags$h6("Latitude: ",station$lat)
      ))
    }
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  }

  # When a dot is clicked show info on station
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showStationPopup(event$id, event$lat, event$lng)
    })
  })




















  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
