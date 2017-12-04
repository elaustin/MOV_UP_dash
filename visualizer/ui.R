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
                                      
                                      selectInput("usertimav",label=h4("Select Time Averaging"),
                                                  choices = c("1 Second" = 1,
                                                              "10 Second" = 	10,
                                                              "30 Second" = 30,
                                                              "1 Minute" = 60,
                                                              "5 Minute" = 60*5,
                                                              "10 Minute" = 60*10,
                                                              "1 Hour" = 60*60), selected = 10),

                                      # Input: Select a file ----
                                      em("Use Ctrl to select mutliple files."),
                                      fileInput("gpsfile", "Select GPS data files",
                                                multiple = TRUE,
                                                accept = c(".csv")),
                                      fileInput("LanganCO", "Select Langan CO data files",
                                                multiple = TRUE,
                                                accept = c(".csv")),
                                      fileInput("ptrak", "Select P-TRAK data file (no screen)",
                                                multiple = TRUE,
                                                accept = c(".txt")),
                                      fileInput("ptrakscreen", "Select P-TRAK data file (with screen)",
                                                multiple = TRUE,
                                                accept = c(".txt")),
                                      fileInput("ae51", "Select AE51 BC", accept=c(".dat")),
                                      fileInput("nanoScan", "Select NanoScan Scan Mode", accept=c(".csv")),
                                      fileInput("nanoSingle", "Select NanoScan Single Channel Mode", accept=c(".csv")),
                                      fileInput("Labview", "Select Labview", accept=c(".txt")),
                                      fileInput("filelog", "Insert Operator Log File", accept=c(".csv")),
                                      
                                      actionButton("mergeButton","Merge!"),
                                      downloadButton("downloadData", "Download"),
                                      tags$hr(),
                                      sliderInput("rowsn", "Select number of rows to display",min=0,max=150,step=10, value=10)
                                      
                                      
                                      
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
                                      
                                      # Output: Data file ----
                                      tableOutput("contents")
                                      
                                    )
                                  )
                                )
                            )
                   ),
                   
                   tabPanel(strong("Data Visualizer"),
                            div(class = "outer",
                                fluidPage(
                                  titlePanel("Data Plots"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      dateRangeInput("Dates",
                                                     "Date Range:", min="2017-07-01"),
                                      sliderInput("ylimpm", "Select Maximum PNC Diff to plot",min=0,max=50000,step=100, value=10000)
                                      # selectInput("colors", "Color Scheme",
                                      #             rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                                      # checkboxInput("legend", "Show legend", TRUE)
                                      
                                      
                                    ),
                                    
                                    mainPanel(
                                      
                                      plotOutput("tsplot", height = 400)
                                    )
                                  )
                                )
                            )
                   ),
                   
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
                                                              "P-TRAK Diff" = "pnc_diff"), selected = "CO")
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