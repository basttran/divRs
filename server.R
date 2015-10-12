library(shiny)
library(shinyjs)
library(leaflet)
library(plyr)
library(dplyr)
library(DT)
library(shinyAce)


#######Functions and reactiveValues######
server <- reactiveValues(items = list(),
                         points = list(),
                         lines = list(),
                         polygons = list(),
                         polylines = list())
                       
#######Functions and reactiveValues######
shinyServer(function(input, output, session) {
  
  client <- reactiveValues(selected = NULL,
                           current = NULL,
                           buffer = NULL,
                           test = 0)
                         
####Add marker######  
  observeEvent(input$map_click,{
    if (input$elementType != 1) {
      return()
    } else {
      event <- input$map_click
      id <- as.character(as.integer(length(server$items))+1)
      client$selected <- data.frame(lng=event$lng,
                                    lat=event$lat,
                                    layerId=id,
                                    pointId=id,
                                    user=as.character(input$elementName),
                                    stringsAsFactors = FALSE)
      server$points[[length(server$points)+1]] <- client$selected
      server$items[[id]] <- data.frame(layerId=as.character(id),
                                                           type="point",
                                                           author=input$elementName)
      leafletProxy("map") %>% addCircleMarkers(lng=event$lng,
                                               lat=event$lat,
                                               layerId = id,
                                               stroke = input$stroke, 
                                               color = input$strokeColor, 
                                               weight = input$strokeWeight, 
                                               opacity = input$strokeOpacity, 
                                               fill = input$fill, 
                                               fillColor = input$fillColor, 
                                               fillOpacity = input$fillOpacity)
    }
  })
####Add marker######  
####Add polygon######
  observeEvent(input$map_click, { # if no marker is clicked a new one is created
    if (input$elementType != 3) {
      return()      
    } else { 
      if (is.null(client$buffer)) { # we check if a polygon is already underway
        ## if not, store its id in client$current and save spot in server$items
        client$current <- as.character(as.integer(length(server$items)+1))
        server$items[[client$current]] <- client$current
      } # if yes we'll add the new points to the polygon's definition
      event <- input$map_click # we store the event's info
      id <- as.character(as.integer(length(server$items))+1) # new marker's id
      saveRDS(id,"id.Rds")
      saveRDS(client$current,"current.Rds")
      client$selected <- data.frame(lng = event$lng, # the new marker's data
                                    lat = event$lat,
                                    layerId = id,
                                    pointId = id,
                                    user = as.character(input$elementName),
                                    stringsAsFactors = FALSE)
      client$buffer <- rbind(client$buffer,client$selected) # added to poly data
      ## and stored in server$points, where we will later retrieve its data
      server$points[[length(server$points)+1]] <- client$selected
      ## and we store its ref in server$items
      server$items[[id]] <- data.frame(layerId = as.character(id),
                                       type = "point",
                                       author = input$elementName)
      ## we display this new marker as well as a temporary polygon
      leafletProxy("map") %>% addCircleMarkers(lng = as.double(event$lng),
                                               lat = as.double(event$lat),
                                               layerId = id,
                                               color = "grey")
      leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,
                                          lat=client$buffer$lat,
                                          layerId="buffer",
                                          color = "grey")
    }
  })
  
  observeEvent(input$map_marker_click, { # if a marker is clicked add it to poly
    if (input$elementType != 3) {
      return()
    } else {
      if (is.null(client$buffer)) { # we check if a new poly is already underway
        ## if not, store its id in client$current and save spot in server$items
        client$current <- as.character(as.integer(length(server$items)+1))
        server$items[[client$current]] <- as.character(client$current)
      } # if yes we'll add the new points to the polygon's definition
    }
    event <- input$map_marker_click # we store the event's info
    client$selected <- data.frame(lng = event$lng, # the marker's data
                                  lat = event$lat,
                                  layerId = event$id,
                                  pointId = event$id,
                                  user = as.character(input$elementName),
                                  stringsAsFactors = FALSE)
    client$buffer <- rbind(client$buffer,client$selected) # added to poly's data
    ## we update the temporary polygon's display
    leafletProxy("map") %>% addPolygons(lng = client$buffer$lng,
                                        lat = client$buffer$lat,
                                        layerId = "buffer",
                                        stroke = TRUE, 
                                        color = "grey")
    ## we need to 'close' the shape if its first node is clicked
    if (nrow(client$buffer) > 1 && 
        ## provided there are more than one node
        head(client$buffer$pointId,1) == tail(client$buffer$pointId,1)) {
      ## if so we remove the temporary polygon from the map
      leafletProxy("map") %>% removeShape(layerId = "buffer")
      ## we update the buffer/poly's data with the reserved id a
      client$buffer <- rbind(client$buffer,NA) # add a row of NAs as a separator
      ## we store this in server$polgons where we will later retrieve its data
      client$buffer$layerId <- client$current  
      
      server$polygons[[length(server$polygons)+1]] <- client$buffer
      ## and we add these data ref to our items list
      server$items[[client$current]] <- data.frame(layerId = client$current,
                                                   type = "polygon",
                                                   author = input$elementName)
      ## we display our finished polygon on the map                                                 
      leafletProxy("map") %>% addPolygons(lng = client$buffer$lng,
                                          lat = client$buffer$lat,
                                          layerId = unique(
                                            na.omit(client$buffer$layerId)),
                                          stroke = input$stroke, 
                                          color = input$strokeColor, 
                                          weight = input$strokeWeight, 
                                          opacity = input$strokeOpacity, 
                                          fill = input$fill, 
                                          fillColor = input$fillColor, 
                                          fillOpacity = input$fillOpacity)
      ## we clear client$buffer & client$current so we can start new shapes 
      client$buffer<-NULL
      client$current<-NULL
      saveRDS(server$items,"items.Rds")
    } else {
      return()
    }
  })
####Add polygon#####
####Add polyline######
  #Creates a new point on the map and adds it to the list of points (buffer) 
  #that will later define the line
  observeEvent(input$map_click, {
    if (input$elementType != 2) {
      return()      
    } else { 
      if (is.null(client$buffer)) { # we check if a line is already underway
        ## if not, store its id in client$current and save spot in server$items
        client$current <- as.character(as.integer(length(server$items)+1))
        server$items[[client$current]] <- client$current
      } # if yes we'll add the new points to the polygon's definition
      event <- input$map_click # we store the event's info
      id <- as.character(as.integer(length(server$items))+1) # new marker's id
      saveRDS(id,"id.Rds")
      saveRDS(client$current,"current.Rds")
      client$selected <- data.frame(lng = event$lng, # the new marker's data
                                    lat = event$lat,
                                    layerId = id,
                                    pointId = id,
                                    user = as.character(input$elementName),
                                    stringsAsFactors = FALSE)
      client$buffer <- rbind(client$buffer,client$selected) # add to line's data
      ## and stored in server$points, where we will later retrieve its data
      server$points[[length(server$points)+1]] <- client$selected
      ## and we store its ref in server$items
      server$items[[id]] <- data.frame(layerId = as.character(id),
                                       type = "point",
                                       author = input$elementName)
      ## we display this new marker
      leafletProxy("map") %>% addCircleMarkers(lng = as.double(event$lng),
                                               lat = as.double(event$lat),
                                               layerId = id,
                                               color = "grey")
    # if we actually created a second node we must close the line
    if (length(client$buffer$pointId)==2) {
      ## we update the buffer/poly's data with the reserved id a
      client$buffer <- rbind(client$buffer,NA) # add a row of NAs as a separator
      client$buffer$layerId <- client$current  

      ## we store this in server$polgons where we will later retrieve its data
      server$lines[[length(server$lines)+1]] <- client$buffer
      ## and we add these data ref to our items list
      server$items[[client$current]] <- data.frame(layerId = client$current,
                                                   type = "line",
                                                   author = input$elementName)
      leafletProxy("map") %>% addPolylines(lng=client$buffer$lng,
                                           lat=client$buffer$lat,
                                           layerId = unique(
                                             na.omit(client$buffer$layerId)),
                                           stroke = input$stroke, 
                                           color = input$strokeColor, 
                                           weight = input$strokeWeight, 
                                           opacity = input$strokeOpacity, 
                                           fill = input$fill, 
                                           fillColor = input$fillColor, 
                                           fillOpacity = input$fillOpacity)
      client$buffer<-NULL
      client$current<-NULL
      saveRDS(server$items,"items.Rds")
    } else {
      return()
    }
  }
  })
  #Adds an existing point to the list of points (buffer) that will later define 
  #the line
  observeEvent(input$map_marker_click, {
    if (input$elementType != 2) {
      return()
    } else {
      if (is.null(client$buffer)) { # we check if a new poly is already underway
        ## if not, store its id in client$current and save spot in server$items
        client$current <- as.character(as.integer(length(server$items)+1))
        server$items[[client$current]] <- as.character(client$current)
      } # if yes we'll add the new points to the polygon's definition
    }
    event <- input$map_marker_click # we store the event's info
    client$selected <- data.frame(lng = event$lng, # the marker's data
                                  lat = event$lat,
                                  layerId = event$id,
                                  pointId = event$id,
                                  user = as.character(input$elementName),
                                  stringsAsFactors = FALSE)
    client$buffer <- rbind(client$buffer,client$selected) # added to poly's data
    #Closes the shape if its first node is clicked
    if (length(client$buffer$pointId)==2) {
      ## we update the buffer/poly's data with the reserved id a
      client$buffer <- rbind(client$buffer,NA)
      client$buffer$layerId <- client$current  
       # add a row of NAs as a separator
      ## we store this in server$polgons where we will later retrieve its data
      server$lines[[length(server$lines)+1]] <- client$buffer
      ## and we add these data ref to our items list
      server$items[[client$current]] <- data.frame(layerId = client$current,
                                                   type = "line",
                                                   author = input$elementName)
      leafletProxy("map") %>% addPolylines(lng=client$buffer$lng,
                                           lat=client$buffer$lat,
                                           layerId = unique(
                                             na.omit(client$buffer$layerId)),
                                           stroke = input$stroke, 
                                           color = input$strokeColor, 
                                           weight = input$strokeWeight, 
                                           opacity = input$strokeOpacity, 
                                           fill = input$fill, 
                                           fillColor = input$fillColor, 
                                           fillOpacity = input$fillOpacity)
      client$buffer<-NULL
      client$current<-NULL
      saveRDS(server$items,"items.Rds")
    } else {
      return()
    }
  })
####Add polyline###### 
  
####Select marker####
  observeEvent(input$map_marker_click, {
    if (input$elementType != 0) {
      return()
    } else {
      event<-input$map_marker_click
      client$selected<-data.frame(lng=as.double(event$lng), 
                                  lat=as.double(event$lat),
                                  layerId=event$id,
                                  pointId=event$id,
                                  user=as.character(input$elementName),
                                  stringsAsFactors = FALSE)
      
      leafletProxy("map") %>% removeShape("selected") %>% 
        addCircleMarkers(lng=as.double(event$lng),
                         lat=as.double(event$lat),
                         radius=10,
                         layerId = "selected",
                         stroke = TRUE,
                         color = "red", 
                         weight = 5, 
                         opacity = 0.5, 
                         fill = TRUE, 
                         fillColor = "red",
                         fillOpacity = 0.2) 
    }
  })
####Select marker####
####Select shape######
  observeEvent(input$map_shape_click, {
    if (input$elementType != 0) {
      return()
    } else {
      event<-input$map_shape_click
      polygons <- ldply(server$polygons, data.frame)
      lines <- ldply(server$lines, data.frame)
      shapes<-rbind(polygons,lines)
      shapes<-shapes[shapes$layerId==event$id,]
      if (input$elementName==1) {
        server$beacon1<-shapes
      } else {
        server$beacon2<-shapes
      }
      if (length(shapes$layerId==2)) {
        leafletProxy("map") %>% removeMarker("selected")  %>% 
          addPolylines(lng=shapes$lng,
                       lat=shapes$lat,
                       layerId="selected",
                       stroke = TRUE,
                       color = "red", 
                       weight = 5, 
                       opacity = 0.5, 
                       fill = TRUE, 
                       fillColor = "red",
                       fillOpacity = 0.2)
      } else {
        leafletProxy("map") %>% removeMarker("selected") %>% 
          addPolygons(lng=shapes$lng,
                      lat=shapes$lat,
                      layerId="selected",
                      stroke = TRUE, 
                      color = "red", 
                      weight = 5, 
                      opacity = 0.5, 
                      fill = TRUE, 
                      fillColor = "red",
                      fillOpacity = 0.2)
      }
      client$selected<-data.frame(lng=as.double(event$lng), 
                                  lat=as.double(event$lat),
                                  layerId=event$id,
                                  pointId=event$id, 
                                  user=as.character(input$elementName),
                                  stringsAsFactors = FALSE)
    }
  })
####Select shape#####
  


####Hide/Show####
  observeEvent(input$hideMarkers, {
    points <- ldply(server$points, data.frame)
    leafletProxy("map") %>% removeMarker(points[,3])
  })
  observeEvent(input$showMarkers, {
    points <- ldply(server$points, data.frame)
    leafletProxy("map") %>% addCircleMarkers(lng=points[,1],
                                             lat=points[,2],
                                             radius=10,
                                             layerId = points[,3],
                                             stroke = input$stroke, 
                                             color = input$strokeColor, 
                                             weight = input$strokeWeight, 
                                             opacity = input$strokeOpacity, 
                                             fill = input$fill, 
                                             fillColor = input$fillColor, 
                                             fillOpacity = input$fillOpacity)
  })
  observeEvent(input$hidePolygons, {
    polygons <- ldply(server$polygons, data.frame)
    saveRDS(polygons,"polygons.RDS")
    leafletProxy("map") %>% removeShape(layerId=unique(
      na.omit(polygons$layerId)))
  })
  observeEvent(input$showPolygons, {
    polygons <- ldply(server$polygons, data.frame)
    leafletProxy("map") %>% addPolygons(lng=polygons$lng,
                                        lat=polygons$lat,
                                        layerId=unique(
                                          na.omit(polygons$layerId)),
                                        stroke = input$stroke, 
                                        color = input$strokeColor, 
                                        weight = input$strokeWeight, 
                                        opacity = input$strokeOpacity, 
                                        fill = input$fill, 
                                        fillColor = input$fillColor, 
                                        fillOpacity = input$fillOpacity)
  })
  observeEvent(input$hideLines, {
    lines <- ldply(server$lines, data.frame)
    saveRDS(lines,"lines.Rds")
#     saveRDS(map,"map.Rds")
    leafletProxy("map") %>% removeShape(layerId=unique(na.omit(lines$layerId)))
  })
  observeEvent(input$showLines, {
    lines <- ldply(server$lines, data.frame)
    leafletProxy("map") %>% addPolylines(lng=lines$lng,
                                         lat=lines$lat,
                                         layerId=unique(na.omit(lines$layerId)),
                                         stroke = input$stroke, 
                                         color = input$strokeColor, 
                                         weight = input$strokeWeight, 
                                         opacity = input$strokeOpacity, 
                                         fill = input$fill, 
                                         fillColor = input$fillColor, 
                                         fillOpacity = input$fillOpacity)
  })

######Hide/Show######

# ######Tags#####
#   observe({
#     if(input$tag < 1){return()}
#     isolate({
#       server$taglist<-rbind(server$taglist,
#                             data.frame(layerId=client$selected$layerId,
#                                        label=input$category,
#                                        stringsAsFactors = FALSE)) 
#     })
#     updateTextInput(session, "category", value="")
#   })
######Tags#####
# ######Links#####  
#   observe({
#     if(input$link < 1){return()}
#     isolate({
#       server$links<-
#         rbind(server$links,
#               data.frame(layerId=as.character(client$selected$layerId),
#                          url=paste("<a href= \"",
#                                    as.character(input$url),
#                                    "\">Check on ePlanet</a>",
#                                    sep=""),
#                          stringsAsFactors = FALSE)) 
#       saveRDS(server$links,"links.Rds")
#     })
#     updateTextInput(session, "url", value="")
#   })
# ######Links#####  
  
  
####Output####
  output$eplanete <- renderText({
    data<-server$links[server$links$layerId==client$selected$layerId,]
    saveRDS(data,"data.Rds")
    data<-HTML(data[,2])
  })
  output$labels <- renderTable({
    data<-server$taglist[,2]     
    
#     updateSelectInput(session, 
#                       inputId="caption", 
#                       label="Tag", 
#                       choices = as.list(data), 
#                       selected = NULL)
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
      addTiles(urlTemplate="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
               attribution = NULL, 
               layerId = NULL, 
               options = tileOptions()) %>%
      setView(2,46,6)   
  })
  output$elements <- DT::renderDataTable({
    elements <- ldply(server$items, data.frame)
    elements[,c(3,4)]
  }, options = list(pageLength = 5,
                    lengthChange = FALSE,
                    dom = '<"top">rt<"bottom"fp><"clear">'))
  
  output$picked <- DT::renderDataTable({
    picked <- ldply(server$items, data.frame)
    picked[input$elements_rows_selected,c(2,3)]
  }, options = list(pageLength = 10,
                    lengthChange = FALSE,
                    dom = '<"top"i>rt<"bottom"p><"clear">'))
  
  observeEvent(input$elements_rows_selected, {
    items <- ldply(server$items, data.frame)
    picked <- items[input$elements_rows_selected,c(2,3)]
    saveRDS(picked,"picked.RDS")
    
    pickedPoints <- picked[picked$type=="point","layerId"]
    saveRDS(pickedPoints,"pickedPoints.RDS")
    pointsData <- ldply(server$points, data.frame)
    saveRDS(pointsData,"pointsData.RDS")
    pickedPointsData <- pointsData[pointsData$layerId %in% pickedPoints,]
    saveRDS(pickedPointsData,"pickedPointsData.RDS")
    
    pickedLines <- picked[picked$type=="line","layerId"]
    saveRDS(pickedLines,"pickedLines.RDS")
    linesData <- ldply(server$lines, data.frame)
    saveRDS(linesData,"linesData.RDS")
    pickedLinesData <- linesData[linesData$layerId %in% pickedLines,]
    saveRDS(pickedLinesData,"pickedLinesData.RDS")
    
    pickedPolygons <- picked[picked$type=="polygon","layerId"]
    saveRDS(pickedPolygons,"pickedPolygons.RDS")
    polygonsData <- ldply(server$polygons, data.frame)
    saveRDS(polygonsData,"polygonsData.RDS")
    pickedPolygonsData <- polygonsData[polygonsData$layerId %in% pickedPolygons,]
    saveRDS(pickedPolygonsData,"pickedPolygonsData.RDS")
    
    

    leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    if (nrow(pickedPointsData)==0) {
      return
    } else {
      leafletProxy("map") %>% addCircleMarkers(lng = pickedPointsData$lng,
                                               lat = pickedPointsData$lat,
                                               layerId = pickedPointsData$layerId,
                                               stroke = input$stroke, 
                                               color = input$strokeColor, 
                                               weight = input$strokeWeight, 
                                               opacity = input$strokeOpacity, 
                                               fill = input$fill, 
                                               fillColor = input$fillColor, 
                                               fillOpacity = input$fillOpacity) 
    }

    
    if (nrow(pickedLinesData)==0) {
      return
    } else {
      leafletProxy("map") %>% addPolylines(lng=pickedLinesData$lng,
                                           lat=pickedLinesData$lat,
                                           layerId=unique(na.omit(pickedLinesData$layerId)),
                                           stroke = input$stroke, 
                                           color = input$strokeColor, 
                                           weight = input$strokeWeight, 
                                           opacity = input$strokeOpacity, 
                                           fill = input$fill, 
                                           fillColor = input$fillColor, 
                                           fillOpacity = input$fillOpacity)
    }
    if (nrow(pickedPolygonsData)==0) {
      return
    } else {
      leafletProxy("map") %>% addPolygons(lng=pickedPolygonsData$lng,
                                          lat=pickedPolygonsData$lat,
                                          layerId=unique(na.omit(pickedPolygonsData$layerId)),
                                          stroke = input$stroke, 
                                          color = input$strokeColor, 
                                          weight = input$strokeWeight, 
                                          opacity = input$strokeOpacity, 
                                          fill = input$fill, 
                                          fillColor = input$fillColor, 
                                          fillOpacity = input$fillOpacity)
    }
    
    
    saveRDS(client$test,"test.RDS")
  })
    



})
####Output####


