library(shiny)
library(leaflet)
library(shinyjs)
library(shinyAce)


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
              tabsetPanel(
              tabPanel("Elements",
                       fixedRow(column(6, textInput("elementName","Element's name",value="")),
                                column(6, selectInput("elementType","Element's type",c("Select" = 0,
                                                                       "Draw points" = 1,
                                                                       "Draw lines" = 2,
                                                                       "Draw polygons" = 3)))),
                       h5(tags$strong("Tags")),
                       aceEditor("elementTags", value="", mode="html", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       DT::dataTableOutput("elements")),
              tabPanel("Themes",
                       fixedRow(column(6,textInput("themeName", "Theme's name")),
                                column(6,br(),
                                       actionLink("themePrevious", label = "Previous"),
                                       HTML(" | "),
                                       actionLink("themeNext", label = "Next"),
                                       br(),
                                       actionLink("themeCopy", label = "Copy ( to new)"),
                                       HTML(" | "),
                                       actionLink("themeNew", label = "New"))),
                       h5(tags$strong("Tags")),
                       aceEditor("themeTags", value="", mode="html", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       fixedRow(column(6,colourInput("strokeColor",
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
                                column(6,
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
                                       checkboxInput("stroke", "Stroke", value = TRUE),
                                       checkboxInput("fill", "Fill", value = TRUE))),
                       br(),       
                       actionButton("themeAdd", label = "Save theme"),
                       htmlOutput("buffer")),
              tabPanel("Legends",
                       fixedRow(column(6,textInput("legendName", "Legend's name")),
                                column(6, br(),
                                       actionLink("legendPrevious", label = "Previous"),
                                       HTML(" | "),
                                       actionLink("legendNext", label = "Next"),
                                       br(),
                                       actionLink("legendCopy", label = "Copy ( to new)"),
                                       HTML(" | "),
                                       actionLink("legendNew", label = "New"))),
                       h5(tags$strong("Description")),
                       aceEditor("legendDescription", value="", mode="html", theme="ambient", readOnly = FALSE,
                                 height = "200px", fontSize = 12, wordWrap = TRUE),
                       h5(tags$strong("Resources")),
                       aceEditor("legendResources", value="", mode="html", theme="ambient", readOnly = FALSE,
                                 height = "200px", fontSize = 12, wordWrap = TRUE)),
              tabPanel("Maps",
                       h4("To be made"))
              
              )
        )
)


)


