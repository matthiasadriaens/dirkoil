if(!require(leaflet, quietly = TRUE)) install.packages('leaflet') ; require(leaflet, quietly = TRUE)
if(!require(RColorBrewer, quietly = TRUE)) install.packages('RColorBrewer') ; require(RColorBrewer, quietly = TRUE)
if(!require(scales, quietly = TRUE)) install.packages('scales') ; require(scales, quietly = TRUE)
if(!require(lattice, quietly = TRUE)) install.packages('lattice') ; require(lattice, quietly = TRUE)
if(!require(dplyr, quietly = TRUE)) install.packages('dplyr') ; require(dplyr, quietly = TRUE)
if(!require(DT, quietly = TRUE)) install.packages('DT') ; require(DT, quietly = TRUE)

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
load("data/Roads.RData")
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

load("data/lngStations_BE.Rdata")
lngStations$country <- NULL
lngStations$id <- c(1:nrow(lngStations))
lngStations$stationtype <- rep("lng",nrow(lngStations))
lng <- lngStations[,c(6,5,4,3,7,1,2)]
names(lng) <- vars
stations <- rbind(stations,lng)

load("data/cngStations_BE.Rdata")
cngStations$country <- NULL
cngStations$stationtype <- rep("cng",nrow(cngStations))
cngStations$id <- c(1:nrow(cngStations))
cng <- cngStations[,c(7,5,4,3,6,1,2)]
names(cng) <- vars
stations <- rbind(stations,cng)

load("data/petrolStations_BE.Rdata")
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
labcustom <- c("Terminal","LNG","CNG","Petrol")


#############
#Intalling and loading packages to calculate distance
if(!require(rgeos, quietly = TRUE)) install.packages('rgeos') ; require(rgeos, quietly = TRUE)
if(!require(sp, quietly = TRUE)) install.packages('sp') ; require(sp, quietly = TRUE)
if(!require(plyr, quietly = TRUE)) install.packages('plyr') ; require(plyr, quietly = TRUE)
library (rgeos)
library (sp)
library(plyr)
sp.BELGIUM <- BELGIUM
sp.stations <- stations
coordinates(sp.stations) <- ~lon+lat
coordinates(sp.BELGIUM) <- ~lon+lat

# Given radius for influencing counters in km
radiusForStationCount <- 15
#create pairwise distance
distance <- gDistance(sp.BELGIUM,sp.stations, byid=T)
# 1 degree equals 110 km 
distance <- distance*110
# replace all distances higher than given radius with zero, the rest with 1
distance <- apply(distance, 2, function(x) ifelse(x > radiusForStationCount, 0, 1))


# Traffic info on trucks
trucks <- as.data.frame(BELGIUM$truck_daily_avg)
colnames(trucks) <- "trucks"
# Create table where 1's are multiplied with corresponding traffic data
for (i in 1:length(trucks$trucks)){
  distance[,i] <- distance[,i]*trucks[i,1]
}
# Obtain rowsums to have all counter data within a 'radiusForStationCount' km radius
# and put in the stations dataframe
stations <- cbind(stations, Truck = rowSums(distance))

## Same for total traffic
distance <- gDistance(sp.BELGIUM,sp.stations, byid=T)
# 1 degree equals 110 km 
distance <- distance*110
# replace all distances higher than given radius with 0, the rest with 1
distance <- apply(distance, 2, function(x) ifelse(x > radiusForStationCount, 0, 1))

# Traffic info on all vehicles
total <- as.data.frame(BELGIUM$total_daily_avg)
colnames(total) <- "total"
# Create table where 1's are multiplied with corresponding traffic data
for (i in 1:length(total$total)){
  distance[,i] <- distance[,i]*total[i,1]
}
# Obtain rowsums to have all counter data within a 'radiusForStationCount' km radius
# and put in the stations dataframe
stations <- cbind(stations, Total = rowSums(distance))
stations$cars <- stations$Total - stations$Truck
clean_basetable <- stations

clean_basetable$Truck[clean_basetable$stationtype == "terminal"] <- "0"
clean_basetable$Total[clean_basetable$stationtype == "terminal"] <- "0"
clean_basetable$cars[clean_basetable$stationtype == "terminal"] <- "0"

clean_basetable <- rename(clean_basetable, c("Truck"="truck_daily_avg", "Total"="total_daily_avg"))



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#OBSERVE FUNCTION FOR INPUT VARS
function(input, output, session) {
  observeEvent(input$solution,{
    
    #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ################ Genetic algorithm to finetune the location of the predetermined stations
    
    if(!require('genalg', character.only = TRUE, quietly = TRUE)) install.packages('genalg'); require('genalg', character.only = TRUE, quietly = TRUE)
    
    #Code voor de keuze of alle traffic gebruikt moet worden, alleen trucks, alleen traffic.
    target <- input$target
    #Dit is een variabele die aangeeft wat de maximale afstand mag zijn tussen 
    #een station en de terminal die het station gaat bevoorraden
    max_truck_distance <- input$kms
    
    #Here we take the required number of stations 
    nr_of_stations <- input$numlng
    
    #Competition variable
    competition <- input$comp
    
    
    #We wont take the first 6 stations, as these are the terminals.
    # stations 7 and 8 are the existing LNG stations, we include these as their location
    # needs to be taken into account. 
    
    #For the starting stations, we provide a list of 10 suitable startpositions (based on gut feeling)
    #The number of stations taken from this list is specified by the user, with a maximum of 10
    
    
    #DC <- clean_basetable[(7+2):(6+2+nr_of_stations),]
    DC <- data.frame("lon" = c(4.255387, 4.288302, 3.843541, 4.389111, 4.674142, 4.301696, 5.474798, 4.011908, 4.43376, 4.8212026), "lat" = c(50.880937, 51.183174, 50.970936, 51.180704, 50.874973, 50.778507, 50.678347, 51.094534, 50.4513, 50.472375))
    DC <- DC[1:nr_of_stations,]
    
    coordinates <- c(DC$lon,DC$lat)
    total_traffic <- sum(as.numeric(DC$total_daily_avg))
    total_truck <- sum(as.numeric(DC$truck_daily_avg))
    total_car <- total_traffic - total_truck
    str(clean_basetable)
    
    df_coordinates <- matrix(coordinates, ncol=2, byrow = FALSE)
    df_coordinates <- data.frame(df_coordinates)
    str(df_coordinates)
    fn <- function(coordinates){
      cat("\n################# BEGIN ITERATION ################\n")
      if (target == "all traffic"){
        cat("############# BEGIN = ",total_traffic , "\n")
      } else if (target == "trucks"){
        cat("############# BEGIN = ",total_truck , "\n")
      } else {
        cat("############# BEGIN = ",total_car , "\n")
      }
      
      df_coordinates <- matrix(coordinates, ncol=2, byrow = FALSE)
      df_coordinates <- data.frame(df_coordinates)
      
      
      sp.df_coordinates <- df_coordinates
      sp.clean_basetable <- clean_basetable
      coordinates(sp.clean_basetable) <- ~lon+lat
      coordinates(sp.df_coordinates) <- ~ X1 + X2
      
      distance_tanks <- gDistance(sp.clean_basetable,sp.df_coordinates, byid=T)
      
      
      # alle distances zijn nu berekend, minimum distance per gasstation naar 'district' nemen
      min.tanks <- apply(distance_tanks, 1, function(x) order(x, decreasing=F)[1])
      
      optimal_tanks <-clean_basetable[min.tanks,]
      
      if (target == "all traffic"){
        opt <- sum(as.numeric(optimal_tanks$total_daily_avg))
      } else if (target == "trucks"){
        opt <- sum(as.numeric(optimal_tanks$truck_daily_avg))
      } else {
        opt <- sum(as.numeric(optimal_tanks$total_daily_avg)) - 
          sum(as.numeric(optimal_tanks$truck_daily_avg))
      }
      ###############################################################
      #Nu we de tankstations hebben, moeten we kijken of deze niet te ver van 
      #de terminals liggen, maw of er een terminal is die binnen de max afstand ligt
      
      #Beginnen met een vector maken die alle afstanden bevat van elk station naar 
      #zijn dichtstbijzijnde terminal
      #als competitie true is, doen we de lng stations bij onze matrix
      
      if (competition == TRUE){
        competition <- clean_basetable[7:8,]
        optimal_tanks  <- rbind(competition,optimal_tanks)
        
      }
      
      sp.df_opt_station_coor <- optimal_tanks
      coordinates(sp.df_opt_station_coor) <- ~ lon + lat
      clean_terminals_alg <- clean_basetable[1:6,]
      sp.clean_terminals_alg <- clean_terminals_alg
      coordinates(sp.clean_terminals_alg) <- ~lon+lat
      
      distance_terminals <- gDistance(sp.clean_terminals_alg,sp.df_opt_station_coor, byid=T)
      #Multiply these distances with 110, our conversion rate
      distance_terminals <- distance_terminals*110
      
      #Turn the values into a "1" if it is smaller than the max distance, "0" otherwise
      distance_terminals <- apply(distance_terminals, 2, function(x) ifelse(x>max_truck_distance, 0, 1))
      
      #The rowsums need to be at least one, indicating that there is at least one terminal 
      #for the station that lies within the boundary
      #we will check with a bool
      rijsom <- rowSums(distance_terminals)
      if (min(rijsom == 0)){
        opt <- 0
      }
      
      ###############################################################
      
      ######### checken of stations ver genoeg van elkaar liggen (adhv variabele radius)
      # Given radius for influencing counters in km
      radius <- input$station
      #create pairwise distance
      distance_stat <- gDistance(sp.df_opt_station_coor, byid=T)
      # 1 degree equals 110 km 
      distance_stat <- distance_stat*110
      
      # replace all distances higher than given radius with zero, the rest with 1
      distance_stat <- apply(distance_stat, 2, function(x) ifelse(x > radius, 0, 1))
      rijsommeke <- rowSums(distance_stat)
      if (max(rijsommeke) > 1){
        opt <- 0
      }
      
      cat("############# End : ",opt, "\n")
      
      return(-opt)
    }
    
    model <- rbga(stringMin = coordinates - 0.2, 
                  stringMax = coordinates + 0.2, 
                  popSize = 50, iters = 10, 
                  mutationChance = 0.01, evalFunc = fn, 
                  verbose   = TRUE)
    
    model
    #Traffic that passes close to the stations that we selected and optimised
    total_cost_OPT <- model$best[length(model$best)]
    total_cost_OPT
    
    #Optimal coordinates, as generated by the algorithm. These are not yet linked to stations
    coordinates_OPT <-  model$population[which.min(model$evaluations),]
    df_coordinates_OPT <- matrix(coordinates_OPT, ncol=2, byrow = FALSE)
    df_coordinates_OPT <- data.frame(df_coordinates_OPT)
    
    sp.df_coordinates_OPT <- df_coordinates_OPT
    sp.clean_basetable <- clean_basetable
    coordinates(sp.clean_basetable) <- ~lon+lat
    coordinates(sp.df_coordinates_OPT) <- ~ X1 + X2
    
    distance_OPT <- distm(sp.df_coordinates_OPT,sp.clean_basetable, fun = distGeo)
    
    
    #create a table that contains the optimal stations, based on the algorithm
    min.dist_OPT <- apply(distance_OPT, 1, function(x) order(x, decreasing=F)[1])
    
    optimal_stations <-clean_basetable[min.dist_OPT,]
    
    
    # link with closest terminals
    clean_terminals <- clean_basetable[1:6,]
    sp.df_optimal_stations <- optimal_stations
    sp.clean_terminals <- clean_terminals
    coordinates(sp.df_optimal_stations) <- ~lon+lat
    coordinates(sp.clean_terminals) <- ~lon+lat
    
    #create pairwise distance
    distance_terminals <- distm(sp.df_optimal_stations,sp.clean_terminals, fun = distGeo)
    
    # alle distances zijn nu berekend, minimum distance van optimaal gasstation naar 'terminal' nemen
    min.terminals <- apply(distance_terminals, 1, function(x) order(x, decreasing=F)[1])
    min.terminals
    min.terminals_join <- matrix(min.terminals, ncol=1, byrow = FALSE)
    min.terminals_join <- data.frame(min.terminals_join)
    min.terminals_join
    optimal_terminals <- cbind(optimal_stations, min.terminals_join)
    
    optimal_terminals
    #now we only need to attach the latitude and longitude of the terminals
    output_df <- merge(x=optimal_terminals, y=clean_basetable, by.x="min.terminals_join", by.y="id", all.x=TRUE)
    #de coordinaten van de stations zijn aangeduid met x, die van de terminals met y
    output_df$fueltype.x <- NULL
    output_df$counter_name.x <- NULL
    output_df$name.x <- NULL
    output_df$fueltype.y <- NULL
    output_df$counter_name.y <- NULL
    output_df$name.y <- NULL
    output_df$truck_daily_avg.y <- NULL
    output_df$total_daily_avg.y <- NULL
    output_df$stationtype.x <- NULL
    output_df$truck_daily_avg.x <- NULL
    output_df$total_daily_avg.x <- NULL
    output_df$status.x <- NULL
    output_df$stationtype.y <- NULL
    output_df$status.y <- NULL
    output_df$cars.x <- output_df$cars.y <- NULL
    
    
    #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    map <- leafletProxy("map",data = output_df)
    
    map %>%
            clearShapes() %>%
            clearMarkers() %>%
            addMarkers(data = output_df,~lon.x, ~lat.x,layerId=~id) %>%
            addAwesomeMarkers(data = output_df,~lon.y, ~lat.y,layerId=~min.terminals_join,icon = gasicon) 
    for(i in 1:nrow(output_df)){
            map %>% addPolylines(data= output_df,
                                 lat = as.numeric(output_df[i, c(4, 7)]),
                                 lng = as.numeric(output_df[i, c(3, 6)]),
                                 weight = 2,
                                 color = "#66ccff",
                                 layerId = ~min.terminals_join+1)
    }
    #addPolylines(data = output_df, lng = ~lon.x, lat = ~lat.x,color = "#66ccff",weight = 1,layerId = ~min.terminals_join+1)
  })
        
        #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        
        ## Interactive Map ###########################################
        #gasicon = makeIcon("gas.png", 50, 50)
        gasicon <- awesomeIcons(
                icon = 'car',
                iconColor = 'black',
                library = 'ion',
                markerColor = '#ff0000'
        )
        
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
                          title = "Fueling stations",
                          opacity = 1
                )%>%
                addMeasure(
                        position = "bottomleft",
                        primaryLengthUnit = "meters",
                        primaryAreaUnit = "sqmeters",
                        activeColor = "#3D535D",
                        completedColor = "#7D4479")
        
        # This observer is responsible for maintaining the circles and legend,
        # Color of dots based on type of station
        # Selection given for requested data
        observe({
                
                if(input$petrol == TRUE & input$cng == TRUE & input$lng == TRUE){
                        leafletProxy("map",data = stations)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                } else if(input$petrol == FALSE & input$cng == FALSE & input$lng == FALSE){
                        stationssubset <- stations[stations$stationtype == "terminal",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                } else if(input$petrol == TRUE & input$cng == FALSE & input$lng == FALSE){
                        stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                }else if(input$petrol == FALSE & input$cng == TRUE & input$lng == FALSE){
                        stationssubset <- stations[stations$stationtype == "cng" | stations$stationtype == "terminal",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                }else if(input$petrol == FALSE & input$cng == FALSE & input$lng == TRUE){
                        stationssubset <- stations[stations$stationtype == "lng" | stations$stationtype == "terminal",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                }else if(input$petrol == TRUE & input$cng == TRUE & input$lng == FALSE){
                        stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal" | stations$stationtype == "cng",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                }else if(input$petrol == TRUE & input$cng == FALSE & input$lng == TRUE){
                        stationssubset <- stations[stations$stationtype == "petrol" | stations$stationtype == "terminal" | stations$stationtype == "lng",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
                                addCircles(~lon, ~lat,layerId=~id,radius = 4,color = ~mapcolor)
                }else if(input$petrol == FALSE & input$cng == TRUE & input$lng == TRUE){
                        stationssubset <- stations[stations$stationtype == "cng" | stations$stationtype == "terminal" | stations$stationtype == "lng",]
                        leafletProxy("map",data = stationssubset)%>%
                                clearShapes() %>%
                                clearMarkers() %>%
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
        
        observe({
                leafletProxy("map") %>% clearPopups()
                event <- input$map_marker_click
                
                if (is.null(event))
                        return()
                isolate({
                        showStationPopup(event$id, event$lat, event$lng)
                })
        })
        
        
        ## Data Explorer ###########################################
        output$ziptable <- DT::renderDataTable({
                DT::datatable(stations)
        })
}