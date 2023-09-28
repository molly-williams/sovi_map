
# load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(rgeos)) install.packages("rgeos", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rgeos", repos = "http://cran.us.r-project.org")



library(rsconnect)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(rgdal)
library(DT)
library(tidyverse)
library(rgeos)


# Data import and wrangling
sovi <- readOGR("data/SOVI_cities_zips.shp")
sovi@data <- sovi@data %>% 
  dplyr::select(GEO, "Census Block Group ID"=GEOID2, UNDER5, UNDER5_70, NONWHI, NONWHI_70, S65ALONE, S65ALONE_7, LESSGED, LESSGED_70, DSABLTY, DSABLTY_70, 
         RENTERS, RENTER_70, NOCAR, NOCAR_70, LINGISO, LINGISO_70, POV200, POV200_70, ASTHMA, ASTHMA_70, LBW, LBW_70, 
         CARDIO, CARDIO_70, HLTHINS, HLTHINS_70, FOODACCESS, TOTAL_SCOR, PCT_MHHI, CES_SCORE, CES_PCTL, VULNBLTY, NAME, County, est_pop, MHHI=HD01_VD01, ZCTA5CE10)

sovi@data$GEO <- as.numeric(sovi@data$GEO)
sovi@data$FOODACCESS <- as.numeric(as.character(sovi@data$FOODACCESS))
sovi@data$TOTAL_SCOR <- as.numeric(as.character(sovi@data$TOTAL_SCOR))
sovi@data$NAME <- as.character(sovi@data$NAME)
sovi@data$County <- as.character(sovi@data$County)
sovi@data$est_pop <- as.numeric(as.character(sovi@data$est_pop))
sovi@data$MHHI <- as.numeric(as.character(sovi@data$MHHI))



# change ordinal ranking of vulnerability scores for graphing, change decimals to percentages, convert 70th+ percentile rankings to categories
sovi@data <- sovi@data %>% 
  mutate(Vulnerability = ifelse(VULNBLTY == "Moderate", 1, 
                           ifelse(VULNBLTY == "High", 2,
                                  ifelse(VULNBLTY == "Highest", 3, NA)))) %>% 
  mutate("% popn. under age 5" = UNDER5*100) %>% 
  mutate("% of households with >0 non-white people" = NONWHI*100) %>% 
  mutate("% single-person households > age 64" = S65ALONE*100) %>% 
  mutate("% of adults 25+ without GED/diploma" = LESSGED*100) %>% 
  mutate("% of households that include someone with a disability" = DSABLTY*100) %>% 
  mutate("% of rented housing units" = RENTERS*100) %>% 
  mutate("% of households with no vehicle access" = NOCAR*100) %>% 
  mutate("% of households with limited English proficiency" = LINGISO*100) %>% 
  mutate("% of households with income <200% of the federal poverty line" = POV200*100) %>% 
  mutate("% of babies born at low birth weight" = LBW) %>%  # already a percentage 
  mutate("Asthma ER visits per 10k" = ASTHMA) %>% #rate  - convert to %?
  mutate("Heart-attack ER visits per 10k" = CARDIO) %>% #rate - convert to %?
  mutate("% uninsured households" = HLTHINS*100) %>% 
  mutate("Region of low food access" = FOODACCESS) %>%
  mutate(PCT_MHHI = PCT_MHHI*100) %>% 
  mutate("Under 5 popn. >70th percentile?" = ifelse(UNDER5_70 == 1, "Yes", "No")) %>% 
  mutate("Nonwhite households >70th percentile?" = ifelse(NONWHI_70 == 1, "Yes", "No")) %>% 
  mutate("Solo >64 popn. >70th percentile?" = ifelse(S65ALONE_7 == 1, "Yes", "No")) %>% 
  mutate("Adults without GED >70th percentile?" = ifelse(LESSGED_70 == 1, "Yes", "No")) %>% 
  mutate("Disabled households >70th percentile?" = ifelse(DSABLTY_70 == 1, "Yes", "No")) %>% 
  mutate("Renters >70th percentile?" = ifelse(RENTER_70 == 1, "Yes", "No")) %>% 
  mutate("Lack of vehicle access >70th percentile?" = ifelse(NOCAR_70 == 1, "Yes", "No")) %>% 
  mutate("Limited English proficiency >70th percentile?" = ifelse(LINGISO_70 == 1, "Yes", "No")) %>% 
  mutate("Poverty >70th percentile?" = ifelse(POV200_70 == 1, "Yes", "No")) %>% 
  mutate("Asthma ER visits >70th percentile?" = ifelse(ASTHMA_70 == 1, "Yes", "No")) %>% 
  mutate("Low birth weights >70th percentile?" = ifelse(LBW_70 == 1, "Yes", "No")) %>% 
  mutate("Heart attack ER visits >70th percentile?" = ifelse(CARDIO_70 == 1, "Yes", "No")) %>% 
  mutate("Uninsured households >70th percentile?" = ifelse(HLTHINS_70 == 1, "Yes", "No"))



library(raster)

# Delta boundary shapefile
delta_sm <- readOGR(dsn="data", layer="LD_SM_Merged", verbose = FALSE)

## Reproject to lat long
delta_sm <- spTransform(delta_sm, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)

# Delta/SM county boundaries shapefile
delta_counties <- readOGR(dsn="data", layer="LDSMcounties", verbose=FALSE)
delta_counties@data$NAME_UCASE <- as.character(delta_counties@data$NAME_UCASE)


## Reproject to lat long
delta_counties <- spTransform(delta_counties, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)


# HU Watershed boundary shapefile 
watersheds <- readOGR(dsn="data", layer="CA_HU12_clip", verbose = FALSE)
watersheds@data$Name <- as.character(watersheds@data$Name)

## Reproject to lat long
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)

# Suisun Marsh VegCAMP shapefile
#sm_veg <- readOGR(dsn="data", layer="suisun_marsh_veg", verbose = FALSE)

## Reproject to lat long
#sm_veg <- spTransform(sm_veg, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)

# Delta VegCAMP shapefile
#delta_veg <- readOGR(dsn="data", layer="delta_veg", verbose = FALSE)
#delta_veg@data <- delta_veg@data %>% 
#  dplyr::select(OBJECTID, "Vegetation Type"=CWHRTYPE)

# Reproject to lat/long
#delta_veg <- spTransform(delta_veg, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)


# Palette for vegetation layer
#v_pal <- colorNumeric(palette="magma", 11, na.color="transparent")


# Text for popup 
popup_text <- paste(
  "<span style='font-size: 120%'><strong>Vulnerability Rating: ", sovi@data$VULNBLTY,"</strong></span><br/>", 
  "<strong>", sovi@data$NAME,", ", sovi@data$County, " County", "</strong><br/>",
  "Census Block Group ID: ", sovi@data$'Census Block Group ID', "<br/>",
  "Est. median household income: ", dollar(sovi@data$MHHI), "<br/>", 
  "Est. population: ", comma(sovi@data$est_pop), "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)




function(input, output, session) {

# Create point data for interactive location pins
points <- eventReactive(input$recalc, {
    cbind(input$long, input$lat)
  }, ignoreNULL = FALSE)
  

# Create basemap
output$map <- renderLeaflet({
    leaflet() %>% 
    addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/moowill/cki3zbj5o4k4b19qlq98amqia/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibW9vd2lsbCIsImEiOiJja2kzejloOHkxdzNtMnhxcTAwY3Zqa25zIn0.VCsBGYnJr6Z7A7XnD157cg",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )  %>% 
    setView(lat=38.1, lng=-121.8, zoom=10) %>% 
    addMarkers(data = points(), group="Location Pin")
    
})


# Responsive description text and data citations
observe({
  textBy <- input$color
  if (textBy == "Vulnerability") {
    output$selected_var <- renderUI({
      tags$div(
       "Overall vulnerability rating for census blocks based on summed indicators >70th percentile.",
               tags$br(),
               tags$br(),
               "A score of 11 reflects the highest level of social vulnerability, meaning a block group has characteristics
               in the 70th percentile for 11 out of 14 total indicators. 'Highest' vulnerability indicates scores of 8-11,
               'High' indicates scores of 4-7, and 'Moderate' indicates scores of 0-3. See methodology tab for more details."
      )
    })
  }
  
  else if (textBy == "Select Indicator") {
    output$selected_var <- renderUI({
      tags$div(
        " ")
    })
  } 
  
  else if (textBy == "% popn. under age 5") {
    output$selected_var <- renderUI({
      tags$div(
      "Young children are more vulnerable to flooding, extreme heat, and wildfire because they depend 
            on others to take protective actions such as evacuation. They are also more sensitive 
            to heat and air quality impacts because their lungs are still developing.",
      tags$a("(US Global Change Research Program, 2016)", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
      tags$br(),
      tags$br(),
      "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B01001&tid=ACSDT5Y2017.B01001&hidePreview=false", target="_blank")
      )
    })
  }   
  
  else if (textBy == "% of households with >0 non-white people") {
    output$selected_var <- renderUI({
      tags$div(
        "People of color have higher baseline rates of chronic medical conditions 
        that increase their sensitivity to environmental impacts. It is important 
        to note that these population health disparities are the result of long-term, 
        cumulative, social and economic factors - not intrinsic differences based on race",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        "Further, individual households' ability to access resources to recover from natural disasters varies based on race",
        tags$a("(Elliott et al., 2020).", href="https://journals.sagepub.com/doi/full/10.1177/2378023120905439", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B03002&tid=ACSDT5Y2017.B03002&hidePreview=false", target="_blank")
        )
    })
  }
  
  else if (textBy == "% single-person households > age 64") {
    output$selected_var <- renderUI({
      tags$div(
        "Older adults are more likely to have exisiting, chronic medical conditions which make them
        more vulnerable to environmental hazards. They are more sensitive to extreme heat events", 
        tags$a("(Knowlton et al. 2009)", href="https://pubmed.ncbi.nlm.nih.gov/19165388/", target="_blank"),
        "and wildfire smoke",
        tags$a("(Stone et al. 2019).", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
        "Further, older adults living alone are less likely to be able to evacuate on their own.",
        tags$a("(US Global Change Research Program, 2016)", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B11007&tid=ACSDT5Y2017.B11007&hidePreview=false", target="_blank")
        )
    })
  }
  
  else if (textBy == "% of adults 25+ without GED/diploma") {
    output$selected_var <- renderUI({
      tags$div(
        "Low educational attainment is a driver of increased vulnerability 
        and decreased resilience to climate change",
        tags$a("(Raval et al. 2019).", href="https://apen4ej.org/wp-content/uploads/2019/10/APEN-Mapping_Resilience-Report.pdf", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B15003&tid=ACSDT5Y2017.B15003&hidePreview=false", target="_blank"))
    })
  }
  
  else if (textBy == "% of households that include someone with a disability") {
    output$selected_var <- renderUI({
      tags$div(
        "People with disabilities are less likely to be able to evacuate on their own, and are therefore more vulnerable
        to impacts from climate change",
        tags$a("(US Global Change Research Program, 2016).", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B22010&tid=ACSDT5Y2017.B22010&hidePreview=false", target="_blank"))
    })
  }
  
  else if (textBy == "% of rented housing units") {
    output$selected_var <- renderUI({
      tags$div(
        "Renters have fewer resources to repair damage or procure temporary and replacement housing
        in the event of a natural disaster",
        tags$a("(Cutter et al. 2003),", href="https://onlinelibrary.wiley.com/doi/abs/10.1111/1540-6237.8402002", target="_blank"),
        "and face increased exposure and reduced adaptive capacity to hazards like wildfire smoke",
        tags$a("(Stone et al. 2019).", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
        "In the case of extreme heat events, renters without air conditioning may not have the option of installing it,
        or may not be able to afford higher energy costs associated with using AC during peak demand",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B25003&tid=ACSDT5Y2017.B25003&hidePreview=false", target="_blank"))
    })
  }
  
  else if (textBy == "% of households with no vehicle access") {
    output$selected_var <- renderUI({
      tags$div(
        "Households without access to a vehicle are less able to evacuate and are therefore more vulnerable.",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B25044&tid=ACSDT1Y2018.B25044&hidePreview=false", target="_blank"))
    })
  }
  
  
  else if (textBy == "% of households with limited English proficiency") {
    output$selected_var <- renderUI({
      tags$div(
        "Linguistically isolated households are more vulnerable to climate change impacts, such as flooding and extreme 
        heat events, because they have more limited access to or understanding 
        of emergency alerts, health warnings, and safety information than the general population",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=C16002&tid=ACSDT5Y2017.C16002&hidePreview=false", target="_blank")
        )
    })
  }
  
  else if (textBy == "% of households with income <200% of the federal poverty line") {
    output$selected_var <- renderUI({
      tags$div(
        "Low-income communities face a host of disadvantages that are compounded by climate change impacts,
        such as higher baseline rates of chronic medical conditions that increase their sensitivity to environmental hazards
        and fewer resources with which to recover from natural disasers",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=C17002&tid=ACSDT5Y2017.C17002&hidePreview=false", target="_blank")
        )
    })
  }
  
  else if (textBy == "% of babies born at low birth weight") {
    output$selected_var <- renderUI({
      tags$div(
        "Low birth weights are indicative of increased stressed levels in pregnant people, and are often an impact 
        of hazards such as flooding, extreme heat, and wildfire smoke. It is also a useful proxy for overall
        community health and as a predictor of future health conditions",
        tags$a("(US Global Change Research Program, 2016).", href="https://health2016.globalchange.gov/extreme-events", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
        )
    })
  }
  
  else if (textBy == "Asthma ER visits per 10k") {
    output$selected_var <- renderUI({
      tags$div(
        "Individuals suffering from asthma are more sensitive to air pollution caused by climate change 
        hazards such as extreme heat and wildfire smoke",
        tags$a("(Stone et al. 2019),", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
        )
    })
  }
  
  else if (textBy == "Heart-attack ER visits per 10k") {
    output$selected_var <- renderUI({
      tags$div(
        "Individuals suffering from cardiovascular disease are more sensitive to air pollution caused by climate change 
        hazards such as extreme heat and wildfire smoke",
        tags$a("(Stone et al. 2019),", href="https://ww3.arb.ca.gov/smp/progdev/pubeduc/wfgv8.pdf", target="_blank"),
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("CalEnviroScreen 3.0", href="https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30", target="_blank")
        )
    })
  }
  
  else if (textBy == "% uninsured households") {
    output$selected_var <- renderUI({
      tags$div(
        "Individuals that lack health insurance may face more difficulty 
        accessing care for conditions caused or exacerbated by climate change impacts,
        such as extreme heat, exposure to floodwaters, or wildfire smoke",
        tags$a("(CA Office of Planning and Research, 2017).", href="https://www.opr.ca.gov/planning/icarp/resilient-ca.html", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", tags$a("US Census Bureau", href="https://data.census.gov/cedsci/table?q=B27001&tid=ACSDT1Y2018.B27001&hidePreview=false", target="_blank")
        )
    })
  }
  
  else { #(textBy == "Region of low food access") {
    output$selected_var <- renderUI({
      tags$div(
        "A 'food desert' is defined as a region where at least 100 households are more than a half mile
        from the nearest supermarket and have no access to a vehicle, or where at least 500 people or 33% 
        of the population live more than 20 miles from the nearest supermarket. Food deserts are just one 
        measure of food insecurity, which has been identified as a significant contributor to
        health outcomes.",
        tags$a("(CA Healthy Places Index)", href="https://healthyplacesindex.org/data-reports/", target="_blank"),
        tags$br(),
        tags$br(),
        "Data Source: ", 
        tags$a("USDA Food Access Research Atlas", href="https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data.aspx", target="_blank")
        )
    })
  }
  
})




# Draw interactive polygons
observe({
  colorBy <- input$color

  if (colorBy == "Region of low food access") {
  # Color and palette are treated specially in the "food access" case, because the values are categorical instead of continuous.
    colorData <- ifelse(sovi$'Region of low food access' == 1, "yes", "no") ## double check whether 1 means Region of low food access/edit viz to make it more clear
    pal <- colorFactor("viridis", colorData) 
  }
  
     else if (colorBy == "Vulnerability") {
      # Color and palette are treated specially in this case, because the values are categorical instead of continuous.
      colorData <- ifelse(sovi$Vulnerability == 1, "1: Moderate", ifelse(sovi$Vulnerability == 2, "2: High", "3: Highest"))  
     pal <- colorFactor("viridis", colorData)
  }
  
  else if (colorBy == "Select Indicator") {
    # remove indicator data from map 
    colorData <- ifelse(sovi$Vulnerability == 1, " ", ifelse(sovi$Vulnerability == 2, " ", " "))  
    pal <- colorFactor("transparent", colorData)
  } 
  
  
  else { 
    colorData <- sovi@data[[colorBy]]
    pal <- colorBin("viridis", colorData, 10, pretty = FALSE) # 7 = number of bins for each continuous indicator
  }

  
# add indicator data and legend    
  leafletProxy("map", data = sovi) %>%
    clearShapes() %>% # removes appearance of previously selected indicator polygons
    addPolygons(data=sovi, 
                fillColor=pal(colorData), 
                stroke=TRUE, 
                fillOpacity = 0.6, 
                color="black", # polygon border color
                weight=0.8, # polygon border weight
                popup = popup_text,
                group = "Indicator Data") %>% 
    addLegend("bottomright", 
              pal=pal, 
              values=colorData, 
              title=colorBy,
              layerId="colorLegend", 
              labFormat = labelFormat(prefix = "", 
                                      suffix = "", 
                                      between = " - ", 
                                      digits = 0)
              ) %>%
    
  # add location markers
    addMarkers(data = points(), group="Location Pin") %>% 
  
  # add feature shapefiles
    ## Delta boundary
    addPolygons(data=delta_sm, 
                fill=F, 
                stroke=T, 
                color="black", # polygon border color
                weight=3, # polygon border weight
                group = "Delta + Suisun Marsh Boundary") %>% 
    ## County boundaries 
    addPolygons(data=delta_counties, 
                fillColor = "transparent", 
                stroke=T, 
                color="yellow", # polygon border color
                weight=3, # polygon border weight
                label=paste(delta_counties@data$NAME_UCASE),
                group = "County Boundaries") %>% 
  
    ## Watersheds 
    addPolygons(data=watersheds, 
                fillColor = "transparent", 
                stroke=T, 
                color="#08306B", # polygon border color
                weight=3, # polygon border weight
                label=paste(watersheds@data$Name),
                group = "Watersheds") %>% 
    ## Vegetation
#    addPolygons(data=delta_veg, 
#                fillColor = v_pal, 
#                stroke=T, 
#                color="black", # polygon border color
#                weight=3, # polygon border weight
#                label=paste(delta_veg@data$`Vegetation Type`),
#                group = "Vegetation Cover") %>% 
    
  # add layer control panel 
    addLayersControl(
      #    baseGroups = c("Basemap"),
      overlayGroups = c("Indicator Data","Delta + Suisun Marsh Boundary", "Location Pin", "Watersheds", "County Boundaries"), #, "Vegetation Cover"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Watersheds") %>% 
    hideGroup("County Boundaries")
 #     hideGroup("Vegetation Cover")
    

})




## Data Explorer ###########################################

SVI_clean <- sovi@data %>%
  dplyr::select(
    "Census Block Group ID",
    "Zip Code" = ZCTA5CE10,
    "City" = NAME,
    "County" = County,
    "Estimated Population" = est_pop,
    "Median household income" = MHHI,
    "Median household income as % of state household income" = PCT_MHHI,
    "Total Score" = TOTAL_SCOR,
    "Vulnerability Rating" = VULNBLTY,
    "% popn. <5" = "% popn. under age 5",
    "Under 5 popn. >70th percentile?",
    "% of households with one or more nonwhite person" = "% of households with >0 non-white people",
    "Nonwhite households >70th percentile?",
    "% of single-person households >64" = '% single-person households > age 64',
    "Solo >64 popn. >70th percentile?",
    "% of adults >25 without HS diploma or GED" = '% of adults 25+ without GED/diploma',
    "Adults without GED >70th percentile?",
    "% of households that include someone with a disability" = "% of households that include someone with a disability", 
    "Disabled households >70th percentile?",
    "% of tenant-occupied housing units" = "% of rented housing units",
    "Renters >70th percentile?",
    "% of households with no vehicle access" = "% of households with no vehicle access",
    "Lack of vehicle access >70th percentile?",
    "% of households with limited English proficiency" = "% of households with limited English proficiency",
    "Limited English proficiency >70th percentile?",
    "% of households with income <200% of the federal poverty line" = "% of households with income <200% of the federal poverty line",
    "Poverty >70th percentile?",
    "% of babies born at low birth weight" = "% of babies born at low birth weight",
    "Low birth weights >70th percentile?",
    "Age-adjusted rate of asthma ER visits per 10,000" = "Asthma ER visits per 10k",
    "Asthma ER visits >70th percentile?",
    "Age-adjusted rate of heart-attack ER visits per 10,000" = "Heart-attack ER visits per 10k",
    "Heart attack ER visits >70th percentile?",
    "% of households without health insurance" = "% uninsured households",
    "Uninsured households >70th percentile?",
    "Low food access (y/n)" = "Region of low food access")

output$table <- DT::renderDataTable(DT::datatable(SVI_clean, options = list(paging=FALSE)))


## Indicator data for methodology/resources section #########################

indicators <- read_csv("data/indicator_metadata.csv")

output$indicators <- renderTable(indicators)

}


