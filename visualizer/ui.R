# Define UI for data upload app ----

shinyUI(navbarPage("MOV-UP Data Import", id="nav",
                   theme=shinytheme("flatly"),
                   
                   tabPanel(strong("Data Upload"),
                            div(class = "outer",
                                fluidPage(
                                  
                                  # App title ----
                                  titlePanel("MOV-UP Files"),
                                  
                                  # Sidebar layout with input and output definitions ----
                                  sidebarLayout(
                                    
                                    # Sidebar panel for inputs ----
                                    sidebarPanel(
                                      
                                      selectInput("usertimav",label="Data Options",
                                                  choices = c("1 Second" = 1,
                                                              "10 Second" = 	10,
                                                              "30 Second" = 30,
                                                              "1 Minute" = 60,
                                                              "5 Minute" = 60*5,
                                                              "10 Minute" = 60*10,
                                                              "1 Hour" = 60*60), selected = 10),
                                      
                                      checkboxGroupInput("dataoptions",  label = NULL,
                                                    c("Fill in missing values" = "missing",
                                                      "Include KSEA weather" = "ksea")),
                                      tags$hr(),

                                      # Input: Select a file ----
                                      em("Use Ctrl to select mutliple files."),
                                      fileInput("gpsfile", "Select GPS data files",
                                                multiple = TRUE,
                                                accept = c(".csv"), 
                                                placeholder = "No file selected"),
                                      fileInput("LanganCO", "Select Langan CO data files",
                                                multiple = TRUE,
                                                accept = c(".csv")),
                                      fileInput("ptrak", "Select P-TRAK data file (no screen)",
                                                multiple = TRUE,
                                                accept = c(".txt")),
                                      fileInput("ptrakscreen", "Select P-TRAK data file (with screen)",
                                                multiple = TRUE,
                                                accept = c(".txt")),
                                      fileInput("ae51", "Select AE51 BC", accept=c(".dat"), multiple=T),
                                      fileInput("cpc", "Select CPC", accept=c(".csv"), multiple=T),
                                      fileInput("nanoScan", "Select NanoScan Scan Mode", multiple=T, accept=c(".csv")),
                                      fileInput("nanoSingle", "Select NanoScan Single Channel Mode",multiple=T, accept=c(".csv")),
                                      fileInput("Labview", "Select Labview", accept=c(".txt")),
                                      fileInput("filelog", "Insert Operator Log File", accept=c(".csv")),
                                      
                                      actionButton("mergeButton","Merge!"),
                                      downloadButton("downloadData", "Download")
                                      
                                      
                                      
                                      
                                      
                                      # Input: Checkbox if file has header ----
                                      #checkboxInput("header", "Header", TRUE),
                                      
                                      # Input: Select separator ----
                                      # radioButtons("sep", "Separator",
                                      #              choices = c(Comma = ",",
                                      #                          Semicolon = ";",
                                      #                          Tab = "\t"),
                                      #              selected = ","),
                                      
                                      # Input: Select quotes ----
                                      # radioButtons("quote", "Quote",
                                      #              choices = c(None = "",
                                      #                          "Double Quote" = '"',
                                      #                          "Single Quote" = "'"),
                                      #              selected = '"'),
                                      
                                      # Horizontal line ----
                                      
                                    ),
                                    
                                    
                                    # Main panel for displaying outputs ----
                                    mainPanel(
                                      sliderInput("sigs","Significant Digits",  0,10, value=2),
                                      # Output: Data file ----
                                      dataTableOutput("contents")
                                      
                                    )
                                  )
                                )
                            )
                   ),
                   
                   tabPanel(strong("Time Series Plots"),
                            div(class = "outer",
                                fluidPage(
                                  titlePanel("Data Plots"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      radioButtons("tspoll",label=h4("Select Pollutant to Plot"),
                                                  choices = c("CO Langan" = "CO",
                                                              "P-TRAK No Screen" = "pnc_noscreen",
                                                              "P-TRAK Screen" = "pnc_screen",
                                                              "P-TRAK Diff" = "pnc_diff",
                                                              "P-TRAK Background" = "pnc_background",
                                                              "NanoScan 11.5" = "`11.5`",
                                                              "NanoScan 15.4" = "`15.4`",
                                                              "NanoScan 36.5" = "`36.5`"),
                                                  selected=NULL)
                                      
                                      
                                    ),
                                    
                                    mainPanel(
                                      
                                      plotOutput("tsplot", height = 400,
                                                 dblclick = "tsplot_dblclick",
                                                 brush = brushOpts(
                                                   id = "tsplot_brush",
                                                   resetOnNew = TRUE
                                                 ))
                                    )
                                  )
                                )
                            )
                   ),
                   
                   tabPanel(strong("Wind Rose Plots"),
                            div(class = "outer",
                                fluidPage(
                                  titlePanel("Data Plots"),
                                  
                                  mainPanel(plotOutput("windRoseplot", height = 400))
                                  
                                ))),
                   
                   tabPanel(strong("GPS Track"),
                            div(class = "outer",
                                fluidPage(
                                  titlePanel("Map of Activities"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("pollmap",label=h4("Select Pollutant Display"),
                                                  choices = c("CO Langan" = "CO",
                                                              "P-TRAK No Screen" = "pnc_noscreen",
                                                              "P-TRAK Screen" = "pnc_screen",
                                                              "P-TRAK Diff" = "pnc_diff",
                                                              "P-TRAK Background" = "pnc_background"), selected = NULL),
                                      selectInput(inputId="windangle","Select Wind Direction",
                                                  choices = c("All Wind" = "all",
                                                              "Northerly Wind (315 - 45 degrees)"= "north",
                                                              "Easterly Wind (45 - 135 degrees)" = "east",
                                                              "Southerly Wind (135 - 225)" = "south",
                                                              "Westerly Wind (225 - 315)" = "west"))
                                    ),
                                    mainPanel(
                                  leafletOutput("map1")
                                    )
                                  )
                                  
                                  
                                )
                            )
                   )
)
)
