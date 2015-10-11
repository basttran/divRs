library(shiny)
library(leaflet)
library(shinyjs)


shinyUI(fluidPage(mainPanel(h3("divRs"),
                            tags$div(id="cite",
                                     'Proof of concept for ', tags$em('AMORAD 2015'), ' by Bastien Tran (REEDS-OVSQ, 2015).'
                            ),
                            div(class="outer",
                                tags$head(
                                  # Include superzip's custom CSS
                                  includeCSS("styles.css")
                                  ),
                                leafletOutput("map", width="100%", height="100%"))
),

# absolutePanel(id = "debug", class = "panel panel-default", fixed = TRUE,
#               draggable = TRUE, top = "auto", left = 400, right = 400, 
#               bottom = 30, width = 500, height = 300,
#               h4("Debug"),
#               DT::dataTableOutput("picked")
# ),

absolutePanel(id = "datatable", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 50, left = 30, right = "auto", 
              bottom = 30, width = 400, height = 750,
              h4("Elements"),
              h4("Tags"),
              fixedRow(column(4, textInput("tagD", label = " ", value = "")),
                       column(4, textInput("tagE",label = " ", value = "")),
                       column(4, textInput("tagF",label = " ", value = ""))),
              fixedRow(column(6, textInput("item","Item",value="")),
                       column(6, selectInput('tools',"Tool",c("Select" = 0,
                                                              "Draw points" = 1,
                                                              "Draw lines" = 2,
                                                              "Draw polygons" = 3)))),
              # actionLink('hideMarkers', 'Hide markers'),
              # actionLink('showMarkers', 'Show markers'),
              # br(),
              # actionLink('hideLines', 'Hide lines'),
              # actionLink('showLines', 'Show lines'),
              # br(),
              # actionLink('hidePolygons', 'Hide polygons'),
              # actionLink('showPolygons', 'Show polygons'),
              DT::dataTableOutput("elements")),

absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 50, left = "auto", right = 10, 
              bottom = 30, width = 400, height = 750,
              h4("Themes"),
              
              # htmlOutput("selected"),

              # br(),
              fixedRow(column(6,checkboxInput("stroke", "Stroke", value = TRUE),
                             colourInput("strokeColor",
                                         "Stroke color",
                                         value = "white",
                                         showColour = c("both",
                                                        "text",
                                                        "background"),
                                         palette = "square",
                                         allowTransparent = FALSE),
                             sliderInput("strokeOpacity", "Stroke opacity", min = 0, 
                                         max = 1, step = 0.1, value = 0.5),
                             sliderInput("strokeWeight", "Stroke weight", min = 0, 
                                         max = 10, step = 1, value = 5)),
              column(6,checkboxInput("fill", "Fill", value = TRUE),
                             colourInput("fillColor",
                                         "Fill color",
                                         value = "white",
                                         showColour = c("both",
                                                        "text",
                                                        "background"),
                                         palette = "square",
                                         allowTransparent = FALSE),
                             sliderInput("fillOpacity", "Fill opacity", min = 0, 
                                         max = 1, step = 0.1, value = 0.5),
                             textInput("theme", "Theme"))),
              h4("Tags"),
              fixedRow(column(4,textInput("tagA", label = NULL, value = "")),
                       column(4,textInput("tagB", label = NULL, value = "")),
                       column(4,textInput("tagC", label = NULL, value = ""))),
              br(),       
              actionButton("addTheme", label = "Save this theme", value = "My theme"),
                     br(),br(),actionLink("copy", label = "Copy for modification"),
                     br(),actionLink("previous", label = "See previous themes"),
                     br(),actionLink("previous", label = "See next themes"),
                     br(),actionLink("new", label = "Create a new theme"),
              # dashArray = NULL, 
              # smoothFactor = 1, 
              # noClip = FALSE,
              htmlOutput("buffer"))

)
                   
        )


