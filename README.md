# Geolocation-Tool
Enter a CSV with addresses and their corresponding longitudes and latitudes to visualize their location on a map. Once mapped, one is allowed to select certain data points within a radius and those entries from the CSV will be rendered in a data table below. It will show all information included in the CSV. 
library(shiny)
library(readr)
library(leaflet)
library(ggmap)
library(leaflet.extras)
library(data.table)
library(sp)

####Original design is for proximity visuaization. If you only want to use 1 data set, omit the CSV2 option.

data1 = read_csv(###enterCSV1 here)
  data2 = read_csv(###entrCSV2 here)
    Spatial_Data = SpatialPointsDataFrame(data2[,c( "Longitude", "Latitude")],data2)
    
    ui <- fluidPage(
    
      "Patient Address Locater",
      textInput(inputId = "persadd", 
                label = "Your Address"),
      leafletOutput(outputId = "mymap"),
       dataTableOutput(outputId = "mytable")
      
    )
    
    server <- function(input, output) {
      output$mymap = renderLeaflet({
        
        m = leaflet() %>%
          addTiles() %>%
          addDrawToolbar(targetGroup='draw',
                         polylineOptions=FALSE,
                         markerOptions = FALSE,
                         circleOptions = drawCircleOptions(metric = FALSE, feet = FALSE)) %>%
          addLayersControl(overlayGroups = c('draw'), options =
                             layersControlOptions(collapsed=FALSE)) %>%
          setView(lng = -78.949, 
                  lat = 36.0249, 
                  zoom = 10) 
        
        z =  geocode(input$persadd, source = "google", override_limit = TRUE)
        addMarkers(m, data = z) %>%
          addCircleMarkers(m, lng = data2$Longitude, lat = data2$Latitude, radius = 2
                           , popup =data2$'Nurse Name') %>%
          addCircleMarkers(m, lng = data1$Longitude, 
                           lat = data1$Latitude, 
                           radius = 2,
                           color = "red",
                           popup = data1$`Full Name`)
      })
      
      output$mytable = renderDataTable({ 
        req(input$mymap_draw_stop)
        print(input$mymap_draw_new_feature)
        feature_type <- input$mymap_draw_new_feature$properties$feature_type 
        if(feature_type %in% c("rectangle","polygon")) {
          polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
          
          drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(data2){c(data2[[1]][1],data2[[2]][1])})))
          selectedRN <-Spatial_Data %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
          
          return(data.table(data2[which(!is.na(selectedRN)), ]))
        }
       
        
      })
      
    }
    
    
    
    shinyApp(ui = ui, server = server)
    
