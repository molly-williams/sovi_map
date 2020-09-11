# Code adapted in part from the COVID-19 mapper app: https://shiny.rstudio.com/gallery/covid19-tracker.html,
# SuperZIP map: https://shiny.rstudio.com/gallery/superzip-example.html, 

library(shiny)
library(leaflet)

# Choices for drop-down
## Could simplify these later to "race" "age" etc if I can have a responsive feature that pops up in sidebar with additional information/metadata
vars <- c(
    "Vulnerability Rating" = "Vulnerability",
    "% of pop. under age 5" = "% pop <5",
    "% of households with 1 or more nonwhite people" = "% nonwhite households",
    "% of single-person households over age 64" = "% single households >64",
    "% of adults >25 without HS diploma or GED" = "% without GED/diploma",
    "% of households that include someone with a disability" = "% disabled households",
    "% of tenant-occupied housing units" = "% rentals",
    "% of households with no vehicle access" = "% no-car households",
    "% of households with limited English proficiency" = "% limited English households",
    "% of households with income <200% of the federal poverty line" = "% of households in poverty",
    "% of babies born at low birth weight" = "% low birth weight",
    "Age-adjusted rate of asthma ER visits per 10,000" = "Asthma ER visits per 10k",
    "Age-adjusted rate of heart-attack ER visits per 10,000" = "Heart-attack ER visits per 10k",
    "% of households without health insurance" = "% uninsured households",
    "Low food access" = "Food desert"
)

inline=function(x) {
    tags$div(style="display:inline-block;", x)
}

navbarPage("Social Vulnerability Index (SOVI)", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                            # Include custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                            ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Explore SOVI Indicators"),
                                      
                                      selectInput("color", "Vulnerability and Indicators", vars),
                                      conditionalPanel("input.color == 'FOODACCESS'"),
                                      
                                      
                                      h3("Pin Location on Map"),
                                      inline(numericInput("long", label = h5("Longitude:"), value = -121.50001)),
                                      inline(numericInput("lat", label = h5("Latitude:"), value = 38.00001))
                                      ,
                                      #             
                                          
                                          
                                    #  fluidRow(
                                    #      column(6, 
                                    #             numericInput("long", label = h5("Longitude:"), value = -121.50001),
                                    #             numericInput("lat", label = h5("Latitude:"), value = 38.00001)
                                    #             )),
                                      actionButton("recalc", "Show point", width="40%")
                                    )
                        
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data derived from', tags$em('Delta Adapts Equity Technical Memo, Delta Stewardship Council, 2020.')
                        )
                    ),
           
           tabPanel("Data explorer",
                    DT::dataTableOutput("table")
           ),
           tabPanel('Methodology and Resources',
                    h3("SOVI Methodology"),
                    p("Information about methodology here"),
                    h3("Resources"),
                    p("Add resources/more info about Delta Adapts here"),
                    
                    
           ),
           
           conditionalPanel("false", icon("crosshair"))
)


