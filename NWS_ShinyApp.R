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


# R Shiny geoloc geolocation code from 
# https://stackoverflow.com/questions/71850331/vectors-of-latitude-and-longitude-in-geolocation-app-in-shiny

mapUI <- function(id, label = "Location in map"){
  ns <- NS(id)
  
  tagList(
    geoloc::button_geoloc(ns("myBtn"), "Get my Location"),
    tags$br(),
    textOutput(ns("coords")),
    textOutput(ns("col")),
   # textOutput(ns("md")), # for median latitude
   plotOutput(ns("TempPlot"), height="auto"),
   plotOutput(ns("PrecipProbPlot"), height="auto"),
   plotOutput(ns("BarometerPlot"), height="auto"),
   verbatimTextOutput(ns("forecast")),
   htmlOutput(ns("NWSPlot")),
   leafletOutput(ns("lf"))
  )
}

mapServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
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
      
      output$lf <- renderLeaflet({
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        leaflet() %>%
          addTiles() %>%
          setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
          addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
      })
      
      output$forecast <- renderPrint({
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        weather_list <- get_NWS_data(as.numeric(input$myBtn_lat),as.numeric(input$myBtn_lon))
        forecast <- get_forecast(weather_list)
        print.weather_forecast(forecast)
      })
      
      output$TempPlot <- renderPlot(height=800,units="px",{ 
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        
        lat <- as.numeric(input$myBtn_lat)
        lon <- as.numeric(input$myBtn_lon)
        
        weather_list <- get_NWS_data(lat,lon)
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
        
        pTemp <- ggplot(data = db, aes(x=Time, y=temperature, color=NightDay)) + 
          geom_line(lwd=1.2) + 
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
        
        pWind <- ggplot(data = db, aes(x=Time, y=`wind speed`, color=NightDay)) + 
          geom_line(lwd=1.2) + 
          geom_text(aes(y=`wind speed`,label=windlab),vjust=-1, size=6) +
          scale_x_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          xlab("Time") + 
          theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size = 20)) +
          theme(legend.position="none") +
          scale_colour_gradient(low="black",high="orange") +
          ggtitle("Wind Speed forecast") +
          ylim(min(db$`wind speed`),max(db$`wind speed`)+1)
        
        
        print(pTemp / pWind)
        })
      
      output$PrecipProbPlot <- renderPlot(height=600,units="px",{ 
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        
        lat <- as.numeric(input$myBtn_lat)
        lon <- as.numeric(input$myBtn_lon)
        
        weather_list <- get_NWS_data(lat,lon)
        tz <- weather_list$properties$timeZone
        
        grid_forecast <- get_grid_forecast(weather_list)
        
        db <- grid_forecast$properties$probabilityOfPrecipitation$values
        db$lab <- as.character(db$value)
        db$lab[1:length(db$value) %% 4 != 1] <- NA
        db <- rename(db, Time=validTime, `precipitation probability`=value)
        db$Time <- as.POSIXct(db$Time, format="%FT%T",tz=tz)
        
        tmp <- db
        tmp$y <- "Precip. Prob."
        tmp$hj <- rep(c(0.8,0,-0.8), length.out=nrow(db))
        
        hPrecipProb <- ggplot(data=tmp, aes(x=y,y=Time,fill=`precipitation probability`)) + 
          geom_tile(height=3600*6) +
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
          ggtitle("Preciptiation Probability forecast")
        
        db1 <- grid_forecast$properties$skyCover$values
        db1$lab <- as.character(db1$value)
        db1$lab[1:length(db1$value) %% 6 != 1] <- NA
        db1 <- rename(db1, Time=validTime, `sky cover`=value)
        db1$Time <- as.POSIXct(db1$Time, format="%FT%T",tz=tz)
        tmp <- db1
        tmp$y <- "Sky Cover"
        
        hSkyCover <- ggplot(data=tmp, aes(x=y,y=Time,fill=`sky cover`)) + 
          geom_tile(height=3600*6) +
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
          ggtitle("Sky Cover forecast")
        
        print(hPrecipProb | hSkyCover)
        
      })
      
      output$BarometerPlot <- renderPlot(height=400,units="px",{ 
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        
        lat <- as.numeric(input$myBtn_lat)
        lon <- as.numeric(input$myBtn_lon)
        
        
        weather_list <- get_NWS_data(lat, lon)
        tz <- weather_list$properties$timeZone
        grid_forecast <- get_grid_forecast(weather_list)
        
        db <- grid_forecast$properties$pressure[[1]]
        db$lab <- as.character(db$value)
        db$lab[1:length(db$value) %% 6 != 1] <- NA
        db <- dplyr::rename(db, Time=validTime, pressure=value)
        db$Time <- as.POSIXct(db$Time, format="%FT%T",tz=tz)
        
        pPressure <- ggplot(data = db, aes(x=Time, y=pressure)) + 
          geom_line(lwd=1.2) + 
          geom_text(aes(y=pressure,label=lab),vjust=-1, size=6) +
          scale_x_datetime(labels = date_format("%a %H", tz=tz), breaks="6 hours") +
          xlab("Time") + 
          theme(axis.text.x=element_text(angle=45,hjust=1), text = element_text(size = 20)) +
          theme(legend.position="none") +
          scale_colour_gradient(low="black",high="orange") +
          ggtitle("Barometric Pressure") +
          ylim(min(db$pressure),max(db$pressure)+0.05)
        
        print(pPressure)
        
      })
      
      output$NWSPlot <- renderUI({
        req(input$myBtn_lon)
        req(input$myBtn_lat)
        
        lat <- as.numeric(input$myBtn_lat)
        lon <- as.numeric(input$myBtn_lon)
        
        weather_list <- get_NWS_data(lat, lon)
        html.str <- paste0("https://forecast.weather.gov/meteograms/Plotter.php?lat=", round(lat,4),"&lon=",round(lon,4),"&wfo=PBZ&zcode=", basename(weather_list$properties$forecastZone),"&gset=20&gdiff=10&unit=0&tinfo=EY5&ahour=0&pcmd=11101111110000000000000000000000000000000000000000000000000&lg=en&indu=1!1!1!&dd=&bw=&hrspan=48&pqpfhr=6&psnwhr=6")
        tags$div(
          tags$img(src=html.str, width="80%"),
          tags$p(),
          tags$hr(),
          tags$img(src="https://origin.wpc.ncep.noaa.gov/basicwx/allfcsts_loop_ndfd.gif", width="80%"),
          tags$hr(),
          tags$p("12 hour forecast"),
          tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/92fndfd.gif', width="80%"),
          tags$p("24 hour forecast"),
          tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/94fndfd.gif', width="80%"),
          tags$p("36 hour forecast"),
          tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/96fndfd.gif', width="80%"),
          tags$p("48 hour forecast"),
          tags$img(src='https://www.wpc.ncep.noaa.gov/basicwx/98fndfd.gif', width="80%"),
          tags$p(),
          tags$hr()
          )
      })
    }
  )
}

ui <- fluidPage(
  h2("National Weather Service forecast"),
  tags$p("Click the button to get the forecast for your location"),
  mapUI("map1")
)

server <- function(input, output, session) {
  mapServer("map1")
  
}

shinyApp(ui, server)