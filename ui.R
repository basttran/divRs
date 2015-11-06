library(shiny)
library(leaflet)
library(shinyjs)
library(shinyAce)

jsCode <- 'shinyjs.focusNextInputField = function() {
  var el = document.getElementById("elementDescription");
  el.textarea.focus();
//    alert("lol jk");
}'






shinyUI(fluidPage(useShinyjs(),
                  extendShinyjs(text = jsCode),
                  mainPanel(h3("divRs"),
                            tags$div(id="cite",
                                     'Collaborative mapping for ', 
                                     tags$em('AMORAD 2015'), 
                                     ' by Bastien Tran (REEDS-OVSQ, 2015).'
                            ),
                            div(class="outer",
                                tags$head(
                                  # Include superzip's custom CSS
                                  includeCSS("styles.css"),
                                  includeCSS("shinychat.css"),
                                  # And custom JavaScript -- just to send a message when a user hits "enter"
                                  # and automatically scroll the chat window for us. Totally optional.
                                  includeScript("sendOnEnter.js")
                                  ),
                                leafletOutput("map", width="100%", height="100%"))
),

absolutePanel(id = "datatable", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 50, left = 30, right = "auto", 
              bottom = 30, width = 450, height = 750,
              tabsetPanel(
              tabPanel("Elements",
                       fixedRow(column(6, textInput("elementName","Element's name",value="")),
                                column(6, br(),
                                       actionLink("elementPrevious", label = "Previous"),
                                       HTML(" | "),
                                       actionLink("elementNext", label = "Next"),
                                       br(),
                                       actionLink("elementCopy", label = "Copy (to new)"),
                                       HTML(" | "),
                                       actionLink("elementNew", label = "New"))),
                       h5(tags$strong("Tags")),
                       aceEditor("elementTags", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12
                                 , hotkeys = list(nextInputKey = list(win="Tab",mac="Tab"))
                                 ),
                       h5(tags$strong("Description")),
                       aceEditor("elementDescription", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       # h5(tags$strong("Resources")),
                       # aceEditor("elementResources", value="", mode="text", theme="ambient", readOnly = FALSE,
                       #           height = "60px", fontSize = 12),
                       conditionalPanel(condition = "input.elementName != '' && input.elementTags != '' && input.elementDescription != ''",
                                        selectInput("elementType","Element's type",c("Select" = 0,
                                                                                     "Draw points" = 1,
                                                                                     "Draw lines" = 2,
                                                                                     "Draw polygons" = 3))
                       )
                       
              ),
              tabPanel("Sets",
                       fixedRow(column(6,textInput("setName", "Set's name")),
                                column(6,br(),
                                       actionLink("setPrevious", label = "Previous"),
                                       HTML(" | "),
                                       actionLink("setNext", label = "Next"),
                                       br(),
                                       actionLink("setCopy", label = "Copy (to new)"),
                                       HTML(" | "),
                                       actionLink("setNew", label = "New"))),
                       h5(tags$strong("Tags")),
                       aceEditor("setTags", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       h5(tags$strong("Description")),
                       aceEditor("setDescription", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       conditionalPanel(condition = "input.setName != '' && input.setTags != '' && input.setDescription != ''",
                                        DT::dataTableOutput("elements")),
                       br(),
                       actionButton("setAdd", label = "Save set")
              ),
              tabPanel("Themes",
                       fixedRow(column(6,textInput("themeName", "Theme's name")),
                                column(6,br(),
                                       actionLink("themePrevious", label = "Previous"),
                                       HTML(" | "),
                                       actionLink("themeNext", label = "Next"),
                                       br(),
                                       actionLink("themeCopy", label = "Copy (to new)"),
                                       HTML(" | "),
                                       actionLink("themeNew", label = "New"))),
                       h5(tags$strong("Tags")),
                       aceEditor("themeTags", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       h5(tags$strong("Description")),
                       aceEditor("themeDescription", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       fixedRow(column(6,colourInput("strokeColor",
                                                     "Stroke color",
                                                     value = "#050505",
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
                                                   value = "#FFFFFF",
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
              # tabPanel("Legends",
              #          fixedRow(column(6,textInput("legendName", "Legend's name")),
              #                   column(6, br(),
              #                          actionLink("legendPrevious", label = "Previous"),
              #                          HTML(" | "),
              #                          actionLink("legendNext", label = "Next"),
              #                          br(),
              #                          actionLink("legendCopy", label = "Copy (to new)"),
              #                          HTML(" | "),
              #                          actionLink("legendNew", label = "New"))),
              #          h5(tags$strong("Tags")),
              #          aceEditor("legendTags", value="", mode="text", theme="ambient", readOnly = FALSE,
              #                    height = "60px", fontSize = 12),
              #          h5(tags$strong("Description")),
              #          aceEditor("legendDescription", value="", mode="text", theme="ambient", readOnly = FALSE,
              #                    height = "60px", fontSize = 12),
              #          h5(tags$strong("Resources")),
              #          aceEditor("legendResources", value="", mode="text", theme="ambient", readOnly = FALSE,
              #                    height = "60px", fontSize = 12),
              #          actionButton("legendAdd", label = "Save legend")
              # ),
              tabPanel("Maps",
                       # radioButtons("mapCurrent",
                       #             "Current map",
                       #             c("Draft" = 0,
                       #               "Map 1" = 1,
                       #               "Map 2" = 2,
                       #               "Map 3" = 3,
                       #               "Map 4" = 4),
                       #             inline = TRUE),
                       h5(tags$strong("Tags")),
                       aceEditor("mapTags", value = "" , mode="text", theme="ambient", readOnly = TRUE,
                                 height = "60px", fontSize = 12),
                       h5(tags$strong("Description")),
                       aceEditor("mapDescription", value="", mode="text", theme="ambient", readOnly = FALSE,
                                 height = "60px", fontSize = 12),
                       actionButton("mapAdd", label = "Add to map")
                       )
              
              )
        ),
absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
              width = 330, height = "auto",
              h2("Chat"),
              uiOutput("chat"),
              # Create the bottom bar to allow users to chat.
              fluidRow(
                div(class="span10",
                    textInput("entry", "")
                ),
                div(class="span2 center",
                    actionButton("send", "Send")
                ),
                textInput("user", "Your User ID:", value=""),
                tags$hr(),
                helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.
                              <p>Source code available <a href =\"https://github.com/basttran/divRs\">on GitHub</a>.
                              <p>Chat based on <a href =\"https://github.com/trestletech/ShinyChat\">ShinyChat</a>.
                              <p>Map based on <a href =\"https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example\">SuperZIP example</a>."))
              )
              
)
)


)


