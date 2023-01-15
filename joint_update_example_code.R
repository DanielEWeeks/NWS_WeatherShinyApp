library(shiny)
library(DT)
library(geoloc)
library(leaflet)

# Read in the places table containing data on their latitude and longitude
places <- read.table("places.txt",header=TRUE)

ui <- basicPage(
  geoloc::button_geoloc("myBtn", "Get my Location"),
  selectInput("row", "Select location:",
              c("Home"=1,"Cabin"=2,"Cape May"=3,"Oradell"=4,"South Hadley"=5),
              width="10%"),
  DT::dataTableOutput('mytable'),
  tags$br(),
  textOutput("selected"),
  tags$br(),
  textOutput("position"),
  textOutput("coords"),
  textOutput("lat_lon"),
  textOutput("lat_lon_joint"),
  leafletOutput("lf2")
)

server <- function(input, output,session) {

  mydata <- reactive({places})
  lat <- reactiveVal()
  lon <- reactiveVal()
  lat_lon <- reactiveVal(value = c("lat"=0,"lon"=0))
  
  
  output$coords <- renderText(
    paste0("Geoloc: ",input$myBtn_lat,", ", input$myBtn_lon)
    )
  
  output$lf <- renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 17) %>%
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), label = "You're here!")
  })
  

  output$mytable = DT::renderDataTable(    
    datatable(mydata())
  ) 

  selectedRow <- eventReactive(input$mytable_rows_selected,{
    mydata()$places[c(input$mytable_rows_selected)]
  })

  output$selected <- renderText({ 
    # selectedRow()
    mydata()$Lon[input$mytable_rows_selected]
  })
  
  output$position <- renderText({
    paste0("Position: ",mydata()$Lat[as.numeric(input$row)],", ",mydata()$Lon[as.numeric(input$row)])
#    input$myBtn_lon <- mydata()$Lon[as.numeric(input$row)]
#    input$myBtn_lat <- mydata()$Lat[as.numeric(input$row)]
  })
  
  num <- reactiveVal()
 
  # Example code 
  observeEvent(input$number, {
    num(input$number)
  })
  
  observeEvent(input$button, {
    num(1)
  })
  
  observeEvent(c(input$button, input$number), {
    output$check <- renderText({
      num()
    })
  })
    
  observeEvent(input$myBtn_lon, {
    lat <- as.numeric(input$myBtn_lat)
    lon <- as.numeric(input$myBtn_lon)
    
    lat_lon <- c("lat"=lat,"lon"=lon)
    lat_lon(c("lat"=lat,"lon"=lon))
    lat(as.numeric(input$myBtn_lat))
    lon(as.numeric(input$myBtn_lon))
    output$lat_lon <- renderText({
      paste0("observeEvent myBtn_lon: ",lat(),", ",lon())
    })
  })
  
  observeEvent(input$row, {
    lon <- mydata()$Lon[as.numeric(input$row)]
    lat <- mydata()$Lat[as.numeric(input$row)]
    
    lat_lon <- c("lat"=lat,"lon"=lon)
    lat_lon(c("lat"=lat,"lon"=lon))
    lon(mydata()$Lon[as.numeric(input$row)])
    lat(mydata()$Lat[as.numeric(input$row)])
    output$lat_lon <- renderText({
      paste0("observeEvent row: ",lat(),", ",lon())
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
      paste0("Joint update: ",lat(),", ",lon())
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
  
}
runApp(list(ui = ui, server = server))
