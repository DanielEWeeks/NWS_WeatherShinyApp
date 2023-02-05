library(shiny)
library(leaflet)
library(geoloc)
# library(lutz)
library(patchwork)
library(sf)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(scales)
library(lubridate)
library(pander)
library(shinyWidgets)
library(maptools)
library(rebird)

# Read in the places table containing data on their latitude and longitude
places <- read.table("places.txt",header=TRUE)

RetrieveHotspotsList <- function(lat, lon, back=2, dist = 25) {
  hotspots <- ebirdhotspotlist(lat=lat, lng=lon, back=back, dist=dist)
  if (nrow(hotspots) > 0) {
    ll <- hotspots %>% select(lng,lat)
    ll2 <- bind_rows(data.frame(lng=lon, lat=lat), ll)
    ll2 <- as.matrix(ll2, ncol=2)
    km <- spDistsN1(ll2, ll2[1,], longlat=TRUE)
    hotspots$km <- km[-1]
    hotspots$miles <- round(hotspots$km/1.609344, 2)
    hotspots <- hotspots %>% arrange(km)
    return(hotspots)
  } else {
    hotspots$locName <- "No nearby hotspots"
    return(hotspots) 
  }
}

HotspotBirdList <- function(i, hotspots, back=2) {
  hotspotList <- ebirdregion(loc=hotspots$locId[i], back=back)
  LocName <- sym(paste0(hotspots$locName[i]," (",hotspots$miles[i]," miles)"))
  hotspotList <- hotspotList %>% 
    select(comName,howMany,obsDt) %>% 
    rename(Date=obsDt,!!LocName:=comName,Number=howMany)
}

eBirdNotables <- function(lat, lon, back=7, dist=25, maxN = 25) {
  a <- ebirdnotable(lat=lat, lng=lon,back=7, dist=25)
  if (nrow(a) > 0) {
    a$obsDt <- as.Date(a$obsDt)
    a <- a %>% select(obsDt, comName, locName) %>% 
      distinct() %>% 
      rename(Date=obsDt,Name=comName,Location=locName)
  } else {
    a <- data.frame(Message="No recent notable eBird sightings")
  }
  return(head(a, maxN))
}

eBirdNotablesMap <- function(lat, lon, back=7, dist=25, maxN = 25) {
  a <- ebirdnotable(lat=lat, lng=lon,back=7, dist=25)
  if (nrow(a) > 0) {
    a$obsDt <- as.Date(a$obsDt)
    a <- a %>% select(obsDt, comName, locName, lat, lng) %>% 
      distinct() %>% 
      rename(Date=obsDt,Name=comName,Location=locName)
  } else {
    a <- data.frame(Message="No recent notable eBird sightings")
  }
  return(head(a, maxN))
}


SunRiseSet <- function(db, tz, lat, lon) {
  # Change the p.time into the local time zone
  db$p.time <- with_tz(db$Time,tz=tz)
  
  # Sunrise, sunset
  longlat <- matrix(c(lon, lat), nrow=1)
  Hels <- SpatialPoints(longlat, proj4string=CRS("+proj=longlat +datum=WGS84"))
  start.time <- db$p.time[1]
  time.seq <- seq(from=start.time, length.out=9,by='days')
  up <- sunriset(Hels,time.seq,direction='sunrise', POSIXct.out=TRUE)
  down <- sunriset(Hels,time.seq,direction='sunset', POSIXct.out=TRUE)
  up$time <- with_tz(up$time,tz=tz)
  down$time <- with_tz(down$time,tz=tz)
  
  up.time <- up$time[up$time > db$p.time[1]]
  down.time <- down$time[down$time > db$p.time[1]]
  up.time <- up.time[up.time < max(db$p.time)]
  down.time <- down.time[down.time < max(db$p.time)]
  
  up <- data.frame(Date=format(up.time,"%F"),Sunrise=up.time)
  down <- data.frame(Date=format(down.time,"%F"),Sunset=down.time)
  SunriseSunset <- full_join(up, down, by="Date") %>% arrange(Date)
  SunriseSunset$Day <- format(SunriseSunset$Sunrise,"%A")
  SunriseSunset$Day[is.na(SunriseSunset$Day)] <- format(SunriseSunset$Sunset,"%A")
  SunriseSunset <- SunriseSunset %>% select(Date, Day, Sunrise, Sunset)
  SunriseSunset
}

# Joe Guiness has a YouTube video that shows how to access the NWS API
#
# https://youtu.be/hxl1c8WRNiA
# 
# This code is based on Joe Guiness' YouTube tutorial:

get_NWS_data <- function(lat, lon) {
  myurl <- paste0("https://api.weather.gov/points/",lat,",",lon)
  weather_list <- fromJSON(myurl)
} 

# weather_list <- get_NWS_data(lat,lon)

get_forecast <- function(weather_list) {
  sv <- GET(weather_list$properties$forecast)
  forecast <- fromJSON(content(sv, as="text", encoding = "UTF-8"))
  forecast$properties$city <- weather_list$properties$relativeLocation$properties$city
  forecast$properties$state <- weather_list$properties$relativeLocation$properties$state
  forecast$properties$coordinates <- weather_list$properties$relativeLocation$geometry$coordinates
  forecast$properties$timeZone <- weather_list$properties$timeZone
  class(forecast) <- "weather_forecast"
  return(forecast)
}

# forecast <- get_forecast(weather_list)


print.weather_forecast <- function(forecast, nperiods = NULL) {
  fdb <- forecast$properties$periods
  tz <- forecast$properties$timeZone
  if (is.null( nperiods)) {
    nperiods <- nrow(fdb)
  }  
  
  start_times <- fdb$startTime
  start_dates <- as.POSIXct(substr(start_times, 1, 19), format="%FT%T", tz = tz)
  end_times <- fdb$endTime
  end_dates <- as.POSIXct(substr(end_times, 1, 19), format="%FT%T", tz = tz)
  
  st0 <- paste0("Forecast for ", forecast$properties$city,", ",
                forecast$properties$state," (",
                forecast$properties$coordinates[2], ", ",
                forecast$properties$coordinates[1], ")")
  cat(st0,"\n\n")
  
  for (j in 1:nperiods) {
    st1 <- paste0(fdb$name[j]," ", format(start_dates[j], "%F"), ": ",
                  fdb$shortForecast[j],"\n")
    
    cat(st1)
    if (fdb$isDaytime[j]) {
      prefix <- "High: "
    } else {
      prefix <- "Low: "
    }
    st1b <- paste0(prefix,fdb$temperature[j], " ", fdb$temperatureUnit[j],"; ",
                   fdb$windDirection[j]," winds ", fdb$windSpeed[j] ,"\n")
    cat(st1b)
    st2 <- strsplit( fdb$detailedForecast[j], " ")[[1]]
    cat(st2, fill = TRUE)
    cat("\n")
  }
}

get_hourly_forecast <- function(weather_list) {
  sv <- GET(weather_list$properties$forecastHourly)
  forecast <- fromJSON(content(sv, as="text", encoding = "UTF-8"))
  forecast$properties$city <- weather_list$properties$relativeLocation$properties$city
  forecast$properties$state <- weather_list$properties$relativeLocation$properties$state
  forecast$properties$coordinates <- weather_list$properties$relativeLocation$geometry$coordinates
  class(forecast) <- "hourly_forecast"
  return(forecast)
}

get_grid_forecast <- function(weather_list) {
  sv <- GET(weather_list$properties$forecastGridData)
  forecast <- fromJSON(content(sv, as="text", encoding = "UTF-8"))
  forecast$properties$city <- weather_list$properties$relativeLocation$properties$city
  forecast$properties$state <- weather_list$properties$relativeLocation$properties$state
  forecast$properties$coordinates <- weather_list$properties$relativeLocation$geometry$coordinates
  class(forecast) <- "hourly_forecast"
  return(forecast)
}

coord_y_datetime <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  # From https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot
  if (!is.null(ylim)) {
    ylim <- lubridate::as_datetime(ylim)
  }
  ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = expand)
}

get_current_conditions <- function(weather_list, nrows=6) {
  sv <- GET(weather_list$properties$observationStations)
  stations <- fromJSON(content(sv, as="text", encoding = "UTF-8"))
  
  stations_df <- data.frame(ID=stations$features$properties$stationIdentifier,
                            Name=stations$features$properties$name, URL=stations$features$properties$`@id`)
  
  # head(stations_df)
  stations_df$Conditions <- NA
  stations_df$Temperature <- NA
  stations_df$WindChill <- NA
  stations_df$Wind <- NA
  stations_df$WindGusts <- NA
  
  for (i in 1:nrows) {
    sv <- GET(paste0(stations_df$URL[i],"/observations/latest"))
    current <- fromJSON(content(sv, as="text", encoding = "UTF-8"))
    # current$properties$temperature$value
    # current$properties$textDescription
    
    if (!is.null(current$properties$temperature$value)) {
      stations_df$Temperature[i] <- round(32 + (9/5)*current$properties$temperature$value,1)
    }
    stations_df$Conditions[i] <- current$properties$textDescription
    if (!is.null(current$properties$windChill$value)) {
      stations_df$WindChill[i] <- round(32 + (9/5)*current$properties$windChill$value,1)
    }
    if (!is.null(current$properties$windSpeed$value)) {
      stations_df$Wind[i] <- round(current$properties$windSpeed$value/1.609344,1)
    }
    if (!is.null(current$properties$windGust$value)) {
      stations_df$WindGusts[i] <- round(current$properties$windGust$value/1.609344,1)
    }
    
  }
  
  # stations_df$Conditions[i] <- current$properties$textDescription
  
  return(stations_df[1:nrows,])
}


# R Shiny geoloc geolocation code from 
# https://stackoverflow.com/questions/71850331/vectors-of-latitude-and-longitude-in-geolocation-app-in-shiny

mapUI <- function(id, label = "Location in map"){
  ns <- NS(id)
  
  tagList(
    geoloc::button_geoloc(ns("myBtn"), "Get my Location"),
    selectInput(ns("row"), "Select location:",
                c("Home"=1,"Cabin"=2,"Cape May"=3,"Oradell"=4,"South Hadley"=5,"Reading"=6),
                width="10%", selectize = FALSE),
    tags$hr(),
  # textOutput(ns("coords")),
  # textOutput(ns("col")),
    tags$h3("Location"),
    textOutput(ns("lat_lon")),
   # textOutput(ns("md")), # for median latitude
   tags$h3("Current conditions"),
   addSpinner(verbatimTextOutput(ns("current")), spin="circle"),
   tags$h3("Forecast"),
   verbatimTextOutput(ns("forecast_first_period")),
   plotOutput(ns("TempPlot"), height="auto"),
   plotOutput(ns("ShortTermPrecipProb"), height="auto"),
   plotOutput(ns("PrecipProbPlot"), height="auto"),
   plotOutput(ns("BarometerPlot"), height="auto"),
   verbatimTextOutput(ns("forecast")),
   verbatimTextOutput(ns("sunrise")),
   htmlOutput(ns("NWSPlot")),
   leafletOutput(ns("lf2"))
  )
}

ebirdUI <- function(id, label = "Location in map"){
  ns <- NS(id)
  
  tagList(
    geoloc::button_geoloc(ns("myBtn"), "Get my Location"),
    selectInput(ns("row"), "Select location:",
                c("Home"=1,"Cabin"=2,"Cape May"=3,"Oradell"=4,"South Hadley"=5,"Reading"=6),
                width="10%", selectize = FALSE),
    selectInput(inputId = ns("hotspots"), label = "Hotspots",choices = NULL, selectize = FALSE),
    tags$hr(),
    tags$h3("Recent notable eBird sightings"),
    textOutput(ns("lat_lon")),
    addSpinner(verbatimTextOutput(ns("eBirdTable")), spin="circle"),
    leafletOutput(ns("lf_eBird")),
    tags$h3("Recent hotspot sightings"),
    verbatimTextOutput(ns("hotspotList"))
  )
}

ebirdServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 mydata <- reactive({
                   places
                 })
                 lat <- reactiveVal()
                 lon <- reactiveVal()
                 lat_lon <- reactiveVal(value = c("lat" = 0, "lon" = 0))
                 

                 observeEvent(input$myBtn_lon, {
                   lat <- as.numeric(input$myBtn_lat)
                   lon <- as.numeric(input$myBtn_lon)
                   
                   lat_lon <- c("lat" = lat, "lon" = lon)
                   lat_lon(c("lat" = lat, "lon" = lon))
                   lat(as.numeric(input$myBtn_lat))
                   lon(as.numeric(input$myBtn_lon))
                   output$lat_lon <- renderText({
                     paste0("Current location: ", lat(), ", ", lon())
                   })
                 })
                 
                 observeEvent(input$row, {
                   lon <- mydata()$Lon[as.numeric(input$row)]
                   lat <- mydata()$Lat[as.numeric(input$row)]
                   place <- mydata()$Place[as.numeric(input$row)]
                   
                   lat_lon <- c("lat" = lat, "lon" = lon)
                   lat_lon(c("lat" = lat, "lon" = lon))
                   lon(mydata()$Lon[as.numeric(input$row)])
                   lat(mydata()$Lat[as.numeric(input$row)])
                   output$lat_lon <- renderText({
                     paste0(gsub("_", " ", place), ": ", lat(), ", ", lon())
                   })
                  
                   output$eBirdTable <- renderPrint({
                     req(lat_lon())
                    pander(eBirdNotables(lat(),lon()))
                   })
                   
                   choices_hotspots <- reactive({
                     req(lat_lon())
                     choices_hotspots <- RetrieveHotspotsList(lat=lat(), lon=lon()) 
                   })
                   
                   observe({
                     updateSelectInput(session = session, inputId = "hotspots", 
                                       choices = choices_hotspots()$locName) 
#                                      choices = paste0(choices_hotspots()$locName," (",choices_hotspots()$miles," miles)"))
                   })
                   
                   
                   output$hotspotList <- renderPrint({
                     req(lat_lon())
                     hotspots <- RetrieveHotspotsList(lat=lat(), lon=lon())
                     if (nrow(hotspots)>0) {
                       j <- which(input$hotspots == hotspots$locName) 
                       if (length(j) > 0) {
                       hotspotList <- HotspotBirdList(i=j, hotspots = hotspots)
                       pander(hotspotList)
                       } else {
                         j <- 1
                       }
                     if(j != 1) {
                     i <- 1
                     hotspotList <- HotspotBirdList(i=i, hotspots = hotspots)
                     pander(hotspotList)
                     }
                     i <- 2
                     hotspotList <- HotspotBirdList(i=i, hotspots = hotspots)
                     pander(hotspotList)
                     i <- 3
                     hotspotList <- HotspotBirdList(i=i, hotspots = hotspots)
                     pander(hotspotList)
                     i <- 4
                     hotspotList <- HotspotBirdList(i=i, hotspots = hotspots)
                     pander(hotspotList)
                     } else {
                       pander(data.frame(Message="No nearby hotspots"))
                     }
                   })
                   
                   output$lf_eBird <- renderLeaflet({
                     req(lat_lon())
                     Lon <- lat_lon()["lon"]
                     names(Lon) <- NULL
                     Lat <- lat_lon()["lat"] 
                     names(Lat) <- NULL
                     b <- eBirdNotablesMap(lat(),lon())
                     if (!("Message" %in% names(b))) {
                     leaflet(data=b) %>%
                       addTiles() %>%
                       setView(Lon, Lat, zoom = 10) %>%
                       addMarkers(Lon, Lat, label = "You're here!",labelOptions = labelOptions(noHide = T)) %>% 
                       addMarkers(lng=~lng, lat=~lat,label = ~Name,labelOptions = labelOptions(noHide = T))
                     } else {
                       leaflet() %>%
                         addTiles() %>%
                         setView(Lon, Lat, zoom = 10) %>%
                         addMarkers(Lon, Lat, label = "You're here!",labelOptions = labelOptions(noHide = T))
                     }
                     })
                    
                 })
               })
}

mapServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      mydata <- reactive({places})
      lat <- reactiveVal()
      lon <- reactiveVal()
      lat_lon <- reactiveVal(value = c("lat"=0,"lon"=0))
      
      weather_list_r <- reactive({
        req(lat_lon())
        Lon <- lat_lon()["lon"]
        names(Lon) <- NULL
        Lat <- lat_lon()["lat"] 
        names(Lat) <- NULL
        
        weather_list <- get_NWS_data(Lat, Lon)
      })
      
      forecast_r <- reactive({
        req(lat_lon())
      
        weather_list <- weather_list_r()
        forecast <- get_forecast(weather_list)
      })
      
      output$coords <- renderText(paste(input$myBtn_lat, input$myBtn_lon, sep = ", "))
      
      Lats <- reactiveValues(Lat = NULL)
      
      observeEvent(input$myBtn, {
        Lats$Lat <- c(Lats$Lat, input$myBtn_lat)
      })
      
      
      output$col <- renderText({
        Lats$Lat 
      })
      
      # add median latitude
      output$md <- renderText({
        req(input$myBtn_lat)
        if(length(Lats$Lat) %% 5 == 0){
          paste0("Median latitude is: ", median(Lats$Lat))
        } 
      })
      
      observeEvent(input$myBtn_lon, {
        lat <- as.numeric(input$myBtn_lat)
        lon <- as.numeric(input$myBtn_lon)
        
        lat_lon <- c("lat"=lat,"lon"=lon)
        lat_lon(c("lat"=lat,"lon"=lon))
        lat(as.numeric(input$myBtn_lat))
        lon(as.numeric(input$myBtn_lon))
        output$lat_lon <- renderText({
          paste0("Current location: ",lat(),", ",lon())
        })
      })
      
      observeEvent(input$row, {
        lon <- mydata()$Lon[as.numeric(input$row)]
        lat <- mydata()$Lat[as.numeric(input$row)]
        place <- mydata()$Place[as.numeric(input$row)]
        
        lat_lon <- c("lat"=lat,"lon"=lon)
        lat_lon(c("lat"=lat,"lon"=lon))
        lon(mydata()$Lon[as.numeric(input$row)])
        lat(mydata()$Lat[as.numeric(input$row)])
        output$lat_lon <- renderText({
          paste0(gsub("_"," ",place),": ",lat(),", ",lon())
        })
      })
      
      observeEvent(c(input$myBtn_lon, input$row), {
        # req(lat_lon)
        Lon <- lat_lon()["lon"]
        Lat <- lat_lon()["lat"]
        lat(lat_lon()["lat"])
        lon(lat_lon()["lon"])
        # lat_lon(c("lat"=Lat,"lon"=Lon))
        output$lat_lon_joint <- renderText({
          paste0(lat(),", ",lon())
        })
      })
      
      output$lf2 <- renderLeaflet({
        req(lat_lon())
        Lon <- lat_lon()["lon"]
        names(Lon) <- NULL
        Lat <- lat_lon()["lat"] 
        names(Lat) <- NULL
        leaflet() %>%
          addTiles() %>%
          setView(Lon, Lat, zoom = 17) %>%
          addMarkers(Lon, Lat, label = "You're here!")
      })
      
      
      output$lf <- renderLeaflet({
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        leaflet() %>%
          addTiles() %>%
          setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
          addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
      })
      
      output$current <- renderPrint({
        req(lat_lon())
        weather_list <- weather_list_r()
        current_conditions <- get_current_conditions(weather_list)
        pander(current_conditions[,c("Name","Conditions" , "Temperature",
"WindChill" ,  "Wind", "WindGusts")], split.table=200)
      })
      
      output$sunrise <- renderPrint({
        req(lat_lon())
        weather_list <- weather_list_r()
        hourly_forecast <- get_hourly_forecast(weather_list)
        db <- hourly_forecast$properties$periods
        tz <- weather_list$properties$timeZone
        db$Time <- as.POSIXct(db$startTime, format="%FT%T", tz = tz)
        
        SunriseSunset <- SunRiseSet(db=db, tz = tz, lat= lat(), lon=lon())
        # SunriseSunset$Day <- format(SunriseSunset$Sunrise,"%A")
        SunriseSunset$Sunrise <- format(SunriseSunset$Sunrise, "%H:%M:%S")
        SunriseSunset$Sunset <- format(SunriseSunset$Sunset, "%H:%M:%S")
        # SunriseSunset <- SunriseSunset %>% select(Date, Day, Sunrise, Sunset)
        pander(SunriseSunset)
      })
      
      output$forecast <- renderPrint({
        req(lat_lon())

        weather_list <- weather_list_r()
        forecast <- forecast_r()
        print.weather_forecast(forecast)
      })
      
      output$forecast_first_period <- renderPrint({
        req(lat_lon())
        
        weather_list <- weather_list_r()
        forecast <- forecast_r()
        print.weather_forecast(forecast, nperiods = 1)
      })
      
      output$TempPlot <- renderPlot(height=800,units="px",{ 
        req(lat_lon())
        
        weather_list <- weather_list_r()
        hourly_forecast <- get_hourly_forecast(weather_list)
        
        db <- hourly_forecast$properties$periods
        
        tz <- weather_list$properties$timeZone
        
        db$Time <- as.POSIXct(db$startTime, format="%FT%T", tz = tz)
        
        
        db$NightDay <- as.numeric(db$isDaytime)
        
        db$templab <- as.character(round(db$temperature,0))
        db$templab[1:length(db$templab) %% 6 != 1] <- NA

        
        db$`wind speed` <- as.numeric(gsub(" mph","",db$windSpeed))
        db$windlab <- as.character(db$`wind speed`)
        db$windlab[1:length(db$windlab) %% 6 != 1] <- NA
        
        db$shortLab <- db$shortForecast
        db$shortLab[1:length(db$shortLab) %% 8 != 1] <- NA
        
        SunriseSunset <- SunRiseSet(db=db, tz = tz, lat= lat(), lon=lon())
        
        pTemp <- ggplot(data = db, aes(x=Time, y=temperature, color=NightDay)) + 
          geom_line(linewidth=1.2) + 
          geom_text(aes(y=temperature,label=templab),vjust=-1, size=6) +
          geom_text(aes(y=max(temperature)+30, label=shortLab), color="darkblue", angle=80, size=5) +
          scale_x_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          xlab(paste0("Time (",tz,")")) + 
          theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size = 20)) +
          theme(legend.position="none") +
          scale_colour_gradient(low="black",high="orange") +
          geom_hline(yintercept = 32, color ="darkblue", lty=2) +
          ggtitle(paste0("Temperature forecast")) +
          ylim(min(db$temperature),max(db$temperature)+45)
        
        pTemp <- pTemp +  
          annotate("rect",xmin=SunriseSunset$Sunrise,xmax=SunriseSunset$Sunset,ymin=-Inf,ymax=max(db$temperature)+2,alpha=1/5,fill="sky blue",colour="black")
        
        
        pWind <- ggplot(data = db, aes(x=Time, y=`wind speed`, color=NightDay)) + 
          geom_line(linewidth=1.2) + 
          geom_text(aes(y=`wind speed`,label=windlab),vjust=-1, size=6) +
          scale_x_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          xlab("Time") + 
          theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size = 20)) +
          theme(legend.position="none") +
          scale_colour_gradient(low="black",high="orange") +
          ggtitle("Wind Speed forecast") +
          ylim(min(db$`wind speed`),max(db$`wind speed`)+1)
        
        pWind <- pWind +  
          annotate("rect",xmin=SunriseSunset$Sunrise,xmax=SunriseSunset$Sunset,ymin=-Inf,ymax=Inf,alpha=1/5,fill="sky blue",colour="black")
        
        
        print(pTemp / pWind)
        })
      
      output$ShortTermPrecipProb <- renderPlot(height=300,units="px",{ 
        req(lat_lon())
        
        weather_list <- weather_list_r()
        tz <- weather_list$properties$timeZone
        
        grid_forecast <- get_grid_forecast(weather_list)
        
        db <- grid_forecast$properties$probabilityOfPrecipitation$values
        db$lab <- as.character(db$value)
        db$lab[1:length(db$value) %% 4 != 1] <- NA
        db <- rename(db, Time=validTime, `precipitation probability`=value)
        db$Time <- as.POSIXct(db$Time, format="%FT%T",tz="UTC")
        db$Time <- with_tz(db$Time,tz=tz)
        
        
        hrs <- c(2:13)
        shortTermPrecipProb <- ggplot(data = db[hrs,], aes(x=Time, y=`precipitation probability`)) +
          geom_col(fill="skyblue") +
          geom_text(aes(y=`precipitation probability`,label=`precipitation probability`),vjust=-0.2, size=6) +
          scale_x_datetime(labels = date_format("%H", tz=tz), breaks="1 hours") + 
          ggtitle("12 hour precipitation forecast") +
          theme(text = element_text(size = 20)) + ylim(c(0,100))
        
        print(shortTermPrecipProb)
      })
      
      output$PrecipProbPlot <- renderPlot(height=600,units="px",{ 
        req(lat_lon())
        
        weather_list <- weather_list_r()
        tz <- weather_list$properties$timeZone
        
        grid_forecast <- get_grid_forecast(weather_list)
        
        db <- grid_forecast$properties$probabilityOfPrecipitation$values
        db$lab <- as.character(db$value)
        db$lab[1:length(db$value) %% 4 != 1] <- NA
        db <- rename(db, Time=validTime, `precipitation probability`=value)
        db$Time <- as.POSIXct(db$Time, format="%FT%T",tz="UTC")
        db$Time <- with_tz(db$Time,tz=tz)
        
        tmp <- db
        tmp$y <- "Precip. Prob."
        tmp$hj <- rep(c(0.8,0,-0.8), length.out=nrow(db))
        
        hPrecipProb <- ggplot(data=tmp, aes(x=y,y=Time,fill=`precipitation probability`)) + 
          geom_tile(height=3600*1) +
          geom_text_repel(aes(label=`precipitation probability`), size=5, point.size=NA, direction="x", min.segment.length = 10, na.rm = TRUE, verbose = FALSE) +
          scale_y_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          coord_y_datetime(ylim = c(max(tmp$Time), min(tmp$Time))) +
          scale_fill_gradient2(low="gray", high="blue", limits= c(0,100)) +
          theme_minimal() +
          theme(axis.ticks.x=element_blank(),
                axis.title.x = element_blank(),
                axis.line.y = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                text = element_text(size = 20)) +
          ggtitle("Precipitation Probability")
        
        db1 <- grid_forecast$properties$skyCover$values
        db1$lab <- as.character(db1$value)
        db1$lab[1:length(db1$value) %% 6 != 1] <- NA
        db1 <- rename(db1, Time=validTime, `sky cover`=value)
        db1$Time <- as.POSIXct(db1$Time, format="%FT%T",tz="UTC")
        db1$Time <- with_tz(db1$Time,tz=tz)
        tmp <- db1
        tmp$y <- "Sky Cover"
        
        hSkyCover <- ggplot(data=tmp, aes(x=y,y=Time,fill=`sky cover`)) + 
          geom_tile(height=3600*1) +
          geom_text_repel(aes(label=lab), size=5, point.size=NA, direction="x", min.segment.length = 10, na.rm=TRUE) +
          scale_y_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          coord_y_datetime(ylim = c(max(tmp$Time), min(tmp$Time))) +
          scale_fill_gradient2(low="white", high="darkgray", limits= c(0,100)) +
          theme_minimal() +
          theme(axis.ticks.x=element_blank(),
                axis.title.x = element_blank(),
                axis.line.y = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                text = element_text(size = 20)) +
          ggtitle("Sky Cover")
        
        print(hPrecipProb | hSkyCover)
        
      })
      
      output$BarometerPlot <- renderPlot(height=400,units="px",{ 
        req(lat_lon())
  
        weather_list <- weather_list_r()
        tz <- weather_list$properties$timeZone
        grid_forecast <- get_grid_forecast(weather_list)
        
        if (length(grid_forecast$properties$pressure$values)>0) {
        db <- grid_forecast$properties$pressure[[1]]
        db$lab <- as.character(db$value)
        db$lab[1:length(db$value) %% 6 != 1] <- NA
        db <- dplyr::rename(db, Time=validTime, pressure=value)
        db$Time <- as.POSIXct(db$Time, format="%FT%T",tz="UTC")
        db$Time <- with_tz(db$Time,tz=tz)
        
        pPressure <- ggplot(data = db, aes(x=Time, y=pressure)) + 
          geom_line(linewidth=1.2) + 
          geom_text(aes(y=pressure,label=lab),vjust=-1, size=6) +
          scale_x_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          xlab("Time") + 
          theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size = 20)) +
          theme(legend.position="none") +
          scale_colour_gradient(low="black",high="orange") +
          ggtitle("Barometric Pressure") +
          ylim(min(db$pressure),max(db$pressure)+0.05)
        
  
        } else {
          
          pPressure <- ggplot() +
            annotate("text", x = 2,  y = 2,
                     size = 6,
                     label = "No barometric pressure data available") + theme_void()
          
        }
        
        print(pPressure)
        
      })
      
      output$NWSPlot <- renderUI({
        req(lat_lon())        
  
        # Oradell graph:
        # https://forecast.weather.gov/meteograms/Plotter.php?lat=40.955&lon=-74.031&wfo=OKX&zcode=NJZ104&gset=20&gdiff=10&unit=0&tinfo=EY5&ahour=0&pcmd=11011111111110000000000000000000000000000000000000000000000&lg=en&indu=1!1!1!&dd=&bw=&hrspan=48&pqpfhr=6&psnwhr=6
        # https://forecast.weather.gov/meteograms/Plotter.php?lat=40.955&lon=-74.031&wfo=OKX&zcode=NJZ104&gset=20&gdiff=10&unit=0&tinfo=EY5&ahour=0&pcmd=11011111111110100000000000000000000000000000000000000000000&lg=en&indu=1!1!1!&dd=&bw=&hrspan=48&pqpfhr=6&psnwhr=6
        # https://forecast.weather.gov/meteograms/Plotter.php?lat=40.955&lon=-74.031&wfo=OKX&zcode=NJZ104&gset=20&gdiff=10&unit=0&tinfo=EY5&ahour=0&pcmd=11011111111110111000000000000000000000000000000000000000000&lg=en&indu=1!1!1!&dd=&bw=&hrspan=48&pqpfhr=6&psnwhr=6
        # weather_list <- get_NWS_data(lat, lon)
        weather_list <- weather_list_r()
        html.str <- paste0("https://forecast.weather.gov/meteograms/Plotter.php?lat=", round(lat(),4),"&lon=",round(lon(),4),"&wfo=",basename(weather_list$properties$forecastOffice),"&zcode=", basename(weather_list$properties$forecastZone),"&gset=20&gdiff=10&unit=0&tinfo=EY5&ahour=0&pcmd=11011111111110111000000000000000000000000000000000000000000&lg=en&indu=1!1!1!&dd=&bw=&hrspan=48&pqpfhr=6&psnwhr=6")
        tags$div(
          tags$img(src=html.str, width="80%"),
          # tags$p(),
          # tags$hr(),
          # tags$img(src="https://origin.wpc.ncep.noaa.gov/basicwx/allfcsts_loop_ndfd.gif", width="80%"),
          # tags$hr(),
          # tags$p("12 hour forecast"),
          # tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/92fndfd.gif', width="80%"),
          # tags$p("24 hour forecast"),
          # tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/94fndfd.gif', width="80%"),
          # tags$p("36 hour forecast"),
          # tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/96fndfd.gif', width="80%"),
          # tags$p("48 hour forecast"),
          # tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/98fndfd.gif', width="80%"),
          tags$p(),
          tags$hr(),
          style="text-align: center;"
          )
      })
    }
  )
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Forecast",
  h2("National Weather Service forecast"),
  tags$p("Click the button to get the forecast for your location"),
  mapUI("map1")
    ),
  tabPanel("Maps",
           includeHTML("Maps.html")),
  tabPanel("BirdCast",
           includeHTML("BirdCast.html")),
  tabPanel("eBird",
           ebirdUI("map2"))
  )
)

server <- function(input, output, session) {
  mapServer("map1")
  ebirdServer("map2")
  
}

shinyApp(ui, server)