library(shiny)
library(shinyjs)
library(leaflet)
library(plyr)
library(dplyr)
#######Functions and reactiveValues######
server <- reactiveValues(items=list(),
                       points=list(),
                       lines=list(),
                       polygons=list(),
                       polylines=list(),
                       taglist=NULL,
                       links=NULL,
                       beacon1=NULL,
                       beacon2=NULL)
#######Functions and reactiveValues######
shinyServer(function(input, output, session) {
  client<-reactiveValues(selected=NULL,
                         current=NULL,
                         buffer=NULL)
####Add marker######  
  observeEvent(input$map_click,{
    if (!input$addMarkerOnClick)
      return()
    event <- input$map_click
    id<-as.character(as.integer(length(server$items))+1)
    server$items[[length(server$items)+1]]<-
      server$points[[length(server$points)+1]]<-
        client$selected<-data.frame(lng=event$lng,
                                    lat=event$lat,
                                    layerId=id,
                                    pointId=id,
                                    user=as.character(input$user),
                                    stringsAsFactors = FALSE)
    leafletProxy("map") %>% addCircleMarkers(lng=event$lng,
                                             lat=event$lat,
                                             radius=10,
                                             layerId = id,
                                             stroke = TRUE,
                                             color = "blue", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue",
                                             fillOpacity = 0.2)
#     saveRDS(server$items,"items.Rds")
  })
####Add marker######  
  
####Select marker####
  observeEvent(input$map_marker_click, {
    if (input$addMarkerOnClick || input$addPolygonOnClick || input$addLineOnClick)
      return()
    event<-input$map_marker_click
    client$selected<-data.frame(lng=as.double(event$lng), 
                              lat=as.double(event$lat),
                              layerId=event$id,
                              pointId=event$id,
                              user=as.character(input$user),
                              stringsAsFactors = FALSE)
    
    leafletProxy("map") %>% removeShape("selected") %>% addCircleMarkers(lng=as.double(event$lng),
                                             lat=as.double(event$lat),
                                             radius=10,
                                             layerId = "selected",
                                             stroke = TRUE,
                                             color = "red", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "red",
                                             fillOpacity = 0.2)
  })
####Select marker####
  
####Add polygon######
  #Creates a new point on the map and adds it to the list of points (buffer) that will later define the line
  observeEvent(input$map_click, {
    if (!input$addPolygonOnClick)
      return()
    if (is.null(client$buffer))
      server$items[[length(server$items)+1]]<-client$current<-length(server$items)+1
    event <- input$map_click
    id<-as.character(as.integer(length(server$items))+1)
    leafletProxy("map") %>% addCircleMarkers(lng=as.double(event$lng),
                                             lat=as.double(event$lat),
                                             radius=10,
                                             layerId = id,
                                             stroke = TRUE,
                                             color = "blue", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue",
                                             fillOpacity = 0.2)
    server$items[[length(server$items)+1]]<-server$points[[length(server$points)+1]]<-client$selected<-data.frame(lng=event$lng,
                                                                                                                  lat=event$lat,
                                                                                                                  layerId=id,
                                                                                                                  pointId=id,
                                                                                                                  user=as.character(input$user),
                                                                                                                  stringsAsFactors = FALSE)
    client$buffer<-rbind(client$buffer,data.frame(lng=event$lng,
                                                  lat=event$lat,
                                                  layerId=id,
                                                  pointId=id,
                                                  user=as.character(input$user),
                                                  stringsAsFactors = FALSE))
    
    leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,lat=client$buffer$lat,layerId="buffer",stroke = TRUE,
                                        color = "blue", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue",
                                        fillOpacity = 0.2)
    saveRDS(server$items,"items.Rds")
  })
  #Adds an existing point to the list of points (buffer) that will later define the polygon
  observeEvent(input$map_marker_click, {
    if (!input$addPolygonOnClick)
      return()
    if (is.null(client$buffer))
      server$items[[length(server$items)+1]]<-client$current<-length(server$items)+1
    #Then we add the point to the list
    event<-input$map_marker_click
    id<-as.character(as.integer(length(server$items))+1)
    client$selected<-data.frame(lng=event$lng,
                                lat=event$lat,
                                layerId=event$id,
                                pointId=event$id,
                                user=as.character(input$user),
                                stringsAsFactors = FALSE)
    
    client$buffer<-rbind(client$buffer,client$selected)
    
    leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,lat=client$buffer$lat,layerId="buffer",stroke = TRUE, 
                                        color = "blue", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue", 
                                        fillOpacity = 0.2)
    
    if (length(client$buffer$pointId)>1 && head(client$buffer$pointId,1)==tail(client$buffer$pointId,1)) {
      leafletProxy("map") %>% removeShape(layerId = "buffer")
      client$buffer[,3]<-client$current
      client$buffer<-rbind(client$buffer,NA)
      server$items[[client$current]]<-server$polygons[[length(server$polygons)+1]]<-client$buffer
      leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,
                                          lat=client$buffer$lat,
                                          layerId=unique(na.omit(client$buffer$layerId)),
                                          stroke = TRUE, 
                                          color = "blue",
                                          weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue", 
                                          fillOpacity = 0.2)
      client$buffer<-NULL
      client$current<-NULL
      
    } else {
      return()
    }
    saveRDS(server$items,"items.Rds")
  })
####Add polygon######

#####Add polyline######
  #Creates a new point on the map and adds it to the list of points (buffer) that will later define the line
  observeEvent(input$map_click, {
    if (!input$addLineOnClick)
      return()
    if (is.null(client$buffer))
      server$items[[length(server$items)+1]]<-client$current<-length(server$items)+1
    event <- input$map_click
    id<-as.character(as.integer(length(server$items))+1)
    leafletProxy("map") %>% addCircleMarkers(lng=as.double(event$lng),
                                             lat=as.double(event$lat),
                                             radius=10,
                                             layerId = id,
                                             stroke = TRUE,
                                             color = "blue", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "blue",
                                             fillOpacity = 0.2)
    server$items[[length(server$items)+1]]<-server$points[[length(server$points)+1]]<-client$selected<-data.frame(lng=event$lng,
                                                                                                                  lat=event$lat,
                                                                                                                  layerId=id,
                                                                                                                  pointId=id,
                                                                                                                  user=as.character(input$user),
                                                                                                                  stringsAsFactors = FALSE)
    client$buffer<-rbind(client$buffer,data.frame(lng=event$lng,
                                                  lat=event$lat,
                                                  layerId=id,
                                                  pointId=id,
                                                  user=as.character(input$user),
                                                  stringsAsFactors = FALSE))
    
    
    
    if (length(client$buffer$pointId)==2) {
      client$buffer[,3]<-as.integer(length(server$items))+1
      client$buffer<-rbind(client$buffer,NA)
      server$items[[length(server$items)+1]]<-server$lines[[length(server$lines)+1]]<-client$buffer
      client$selected<-data.frame(lng=as.double(event$lng), lat=as.double(event$lat),layerId=as.integer(length(server$items)))
      leafletProxy("map") %>% addPolylines(lng=client$buffer$lng,
                                           lat=client$buffer$lat,
                                           layerId = unique(na.omit(client$buffer$layerId)))
      client$buffer<-NULL
    } else {
      return()
      
    }
  })
#Adds an existing point to the list of points (buffer) that will later define the polygon
  observeEvent(input$map_marker_click, {
    if (!input$addLineOnClick)
      return()
    if (is.null(client$buffer))
      server$items[[length(server$items)+1]]<-client$current<-length(server$items)+1
    #Then we add the point to the list
    event<-input$map_marker_click
    id<-as.character(as.integer(length(server$items))+1)
    client$selected<-data.frame(lng=event$lng,
                                lat=event$lat,
                                layerId=event$id,
                                pointId=event$id,
                                user=as.character(input$user),
                                stringsAsFactors = FALSE)
    
    client$buffer<-rbind(client$buffer,client$selected)
    
    
    if (length(client$buffer$pointId)==2) {
      client$buffer[,3]<-as.integer(length(server$items))+1
      client$buffer<-rbind(client$buffer,NA)
      server$items[[length(server$items)+1]]<-server$lines[[length(server$lines)+1]]<-client$buffer
      client$selected<-data.frame(lng=as.double(event$lng), lat=as.double(event$lat),layerId=as.integer(length(server$items)))
      leafletProxy("map") %>% addPolylines(lng=client$buffer$lng,
                                           lat=client$buffer$lat,
                                           layerId = unique(na.omit(client$buffer$layerId)))
      client$buffer<-NULL
    } else {
      return()
    }
  })
####Add polyline###### 
  
####Select shape######
  observeEvent(input$map_shape_click, {
    if (input$addMarkerOnClick || input$addPolygonOnClick || input$addLineOnClick)
      return()
    event<-input$map_shape_click
    polygons <- ldply(server$polygons, data.frame)
    lines <- ldply(server$lines, data.frame)
    shapes<-rbind(polygons,lines)
    shapes<-shapes[shapes$layerId==event$id,]
    if (input$user==1) {
    server$beacon1<-shapes
    } else {
    server$beacon2<-shapes
    }
    if (length(shapes$layerId==2)) {
      leafletProxy("map") %>% removeMarker("selected")  %>% addPolylines(lng=shapes$lng,lat=shapes$lat,layerId="selected",stroke = TRUE, 
                                          color = "red", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "red", 
                                          fillOpacity = 0.2)
    } else {
      leafletProxy("map") %>% removeMarker("selected") %>% addPolygons(lng=shapes$lng,lat=shapes$lat,layerId="selected",stroke = TRUE, 
                                          color = "red", weight = 5, opacity = 0.5, fill = TRUE, fillColor = "red", 
                                          fillOpacity = 0.2)
    }
    client$selected<-data.frame(lng=as.double(event$lng), 
                                lat=as.double(event$lat),
                                layerId=event$id,
                                pointId=event$id, 
                                user=as.character(input$user),
                                stringsAsFactors = FALSE)
  })
####Select shape######
  


######Hide/Show######
  observeEvent(input$hideMarkers, {
    points <- ldply(server$points, data.frame)
    leafletProxy("map") %>% removeMarker(points[,3])
  })
  observeEvent(input$showMarkers, {
    points <- ldply(server$points, data.frame)
#     leafletProxy("map") %>% addCircleMarkers(lng=points[,1],lat=points[,2],layerId=points[,3])
    leafletProxy("map") %>% addCircleMarkers(lng=points[,1],
                                             lat=points[,2],
                                             radius=10,
                                             layerId = points[,3],
                                             stroke = TRUE,
                                             color = input$colour, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$colour,
                                             fillOpacity = 0.2)
  })
  observeEvent(input$hidePolygons, {
    polygons <- ldply(server$polygons, data.frame)
    leafletProxy("map") %>% removeShape(layerId=unique(na.omit(polygons$layerId)))
  })
  observeEvent(input$showPolygons, {
    polygons <- ldply(server$polygons, data.frame)
    leafletProxy("map") %>% addPolygons(lng=polygons$lng,lat=polygons$lat,layerId=unique(na.omit(polygons$layerId)))
  })
  observeEvent(input$hideLines, {
    lines <- ldply(server$lines, data.frame)
    saveRDS(lines,"lines.Rds")
#     saveRDS(map,"map.Rds")
    leafletProxy("map") %>% removeShape(layerId=unique(na.omit(lines$layerId)))
  })
  observeEvent(input$showLines, {
    lines <- ldply(server$lines, data.frame)
    leafletProxy("map") %>% addPolylines(lng=lines$lng,lat=lines$lat,layerId=unique(na.omit(lines$layerId)))
  })
######Hide/Show######

######Tags#####
  observe({
    if(input$tag < 1){return()}
    isolate({
      server$taglist<-rbind(server$taglist,data.frame(layerId=client$selected$layerId,label=input$category, stringsAsFactors = FALSE)) 
    })
    updateTextInput(session, "category", value="")
  })
######Tags#####
######Links#####  
  observe({
    if(input$link < 1){return()}
    isolate({
      server$links<-rbind(server$links,data.frame(layerId=as.character(client$selected$layerId),
                                                 url=paste("<a href= \"",as.character(input$url),"\">Check on ePlanet</a>",sep=""),
                                                 stringsAsFactors = FALSE)) 
      saveRDS(server$links,"links.Rds")
    })
    updateTextInput(session, "url", value="")
  })
######Links#####  
  
  
####Output####
  output$eplanete <- renderText({
    data<-server$links[server$links$layerId==client$selected$layerId,]
    saveRDS(data,"data.Rds")
    data<-HTML(data[,2])
  })
  output$labels <- renderTable({
    data<-server$taglist[,2]
#     updateSelectInput(session, inputId="caption", label="Tag", choices = as.list(data), selected = NULL)
    data
  })
  output$buffer <- renderTable({
    data<-client$buffer
    data
  })
  output$selected <- renderTable({
    data<-client$selected
    data
  })
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", attribution = NULL, layerId = NULL, options = tileOptions()) %>%
      setView(2,46,6)   
    })
})
####Output####


