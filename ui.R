library(shiny)
library(leaflet)
library(shinyjs)


shinyUI(fluidPage(mainPanel(h3("divRs"),
                            div(class="outer",
                                tags$head(
                                  # Include superzip's custom CSS
                                  includeCSS("styles.css")
                                  ),
                                leafletOutput("map", width="100%", height="100%"))
),
# absolutePanel(id = "palette", class = "panel panel-default", fixed = TRUE,
#               draggable = TRUE, top = 60, left = 50, right = "auto", 
#               bottom = "auto", width = 330, height = "auto",
#               h2("Elements"),
#               colourInput("colour",
#                           "Pick a colour",
#                           value = "white",
#                           showColour = c("both",
#                                          "text",
#                                          "background"),
#                           palette = "square",
#                           allowTransparent = FALSE),
# #               selectInput("caption","Tag",server$taglist[server$taglist$layerId==client$selected$layerId,server$label],selected=NULL),
# #               actionLink('addLegend', 'Add legend'),
#               
#               tableOutput("labels"),
#               htmlOutput("eplanete")
# 
# ),

absolutePanel(id = "debug", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = "auto", left = 400, right = 400, 
              bottom = 30, width = "auto", height = 300,
              h4("Debug"),
              tableOutput("basket")
),

absolutePanel(id = "datatable", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 50, left = 50, right = "auto", 
              bottom = 30, width = 300, height = "auto",
              h4("Table"),
              dataTableOutput("items")
),
absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 50, left = "auto", right = 50, 
              bottom = 30, width = 300, height = "auto",
              h4("Controls"),
              textInput("user","User",value="user 1"),
              htmlOutput("selected"),
              selectInput('tools',"Tool",c("Select" = 0,
                                           "Draw points" = 1,
                                           "Draw lines" = 2,
                                           "Draw polygons" = 3)
                          ),
              # checkboxInput('addMarkerOnClick', 'Add marker on click', FALSE),
              actionLink('hideMarkers', 'Hide markers'),
              actionLink('showMarkers', 'Show markers'),
              br(),
              # checkboxInput('addLineOnClick', 'Add Line on clicks', FALSE),
              actionLink('hideLines', 'Hide lines'),
              actionLink('showLines', 'Show lines'),
              br(),
              # checkboxInput('addPolygonOnClick', 'Add polygon on clicks', FALSE),
              actionLink('hidePolygons', 'Hide polygons'),
              actionLink('showPolygons', 'Show polygons'), 
              br(),
              textInput("category", "New category"),
              actionButton("tag","Tag"),
              textInput("url", "New link"),
              actionButton("link","Link"),
              htmlOutput("buffer")
),
tags$div(id="cite",
         'Proof of concept for ', tags$em('AMORAD 2015'), ' by Bastien Tran (REEDS-OVSQ, 2015).'
)
)
                   
        )


