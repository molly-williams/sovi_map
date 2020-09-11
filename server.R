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
sovi <- readOGR("data/SOVI_cities.shp")
sovi@data <- sovi@data %>% 
  dplyr::select(GEO, "Census Block Group ID"=GEOID2, UNDER5, UNDER5_70, NONWHI, NONWHI_70, S65ALONE, S65ALONE_7, LESSGED, LESSGED_70, DSABLTY, DSABLTY_70, 
         RENTERS, RENTER_70, NOCAR, NOCAR_70, LINGISO, LINGISO_70, POV200, POV200_70, ASTHMA, ASTHMA_70, LBW, LBW_70, 
         CARDIO, CARDIO_70, HLTHINS, HLTHINS_70, FOODACCESS, TOTAL_SCOR, PCT_MHHI, CES_SCORE, CES_PCTL, VULNBLTY, NAME, County, est_pop, MHHI=HD01_VD01)

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
  mutate("% pop <5" = UNDER5*100) %>% 
  mutate("% nonwhite households" = NONWHI*100) %>% 
  mutate("% single households >64" = S65ALONE*100) %>% 
  mutate("% without GED/diploma" = LESSGED*100) %>% 
  mutate("% disabled households" = DSABLTY*100) %>% 
  mutate("% rentals" = RENTERS*100) %>% 
  mutate("% no-car households" = NOCAR) %>% 
  mutate("% limited English households" = LINGISO*100) %>% 
  mutate("% of households in poverty" = POV200*100) %>% 
  mutate("% low birth weight" = LBW) %>%  # already a percentage 
  mutate("Asthma ER visits per 10k" = ASTHMA) %>% #rate  - convert to %?
  mutate("Heart-attack ER visits per 10k" = CARDIO) %>% #rate - convert to %?
  mutate("% uninsured households" = HLTHINS*100) %>% 
  mutate("Food desert" = FOODACCESS) %>%
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
delta_veg <- readOGR(dsn="data", layer="delta_veg", verbose = FALSE)

#delta_veg <- spTransform(delta_veg, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) #used crs(sovi)


#########################
# Palette for vegetation
#w_palette <- colorNumeric(palette="viridis", domain=watersheds@data$Name,, na.color="transparent")

#w_pal <- colorNumeric(
#  palette = "Blues",
#  domain = watersheds@data$Name)

#binpal <- colorBin("Blues", watersheds@data$Name, 67, pretty = TRUE)

######################


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
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )  %>% 
    setView(lat=38.1, lng=-121.8, zoom=10) %>% 
    addMarkers(data = points(), group="Location Pin") 
})

# Draw interactive polygons
observe({
  colorBy <- input$color

  if (colorBy == "Food desert") {
  # Color and palette are treated specially in the "food access" case, because the values are categorical instead of continuous.
    colorData <- ifelse(sovi$'Food desert' == 1, "yes", "no") ## double check whether 1 means food desert/edit viz to make it more clear
    pal <- colorFactor("viridis", colorData) 
  }
  
     else if (colorBy == "Vulnerability") {
      # Color and palette are treated specially in the "food access" case, because the values are categorical instead of continuous.
      colorData <- ifelse(sovi$Vulnerability == 1, "1: Moderate", ifelse(sovi$Vulnerability == 2, "2: High", "3: Highest"))  
     pal <- colorFactor("viridis", colorData)
  } 
  
  else { 
    colorData <- sovi@data[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  }

# add indicator data and legend    
  leafletProxy("map", data = sovi) %>%
    clearShapes() %>% # removes appearance of previously selected indicator polygons
    addPolygons(data=sovi, 
                fillColor=pal(colorData), 
                stroke=TRUE, 
                fillOpacity = 0.6, 
                color="black", # polygon border color
                weight=0.5, # polygon border weight
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
    addPolygons(data=delta_sm, 
                fill=F, 
                stroke=T, 
                color="black", # polygon border color
                weight=2.5, # polygon border weight
                group = "Delta + Suisun Marsh Boundary") %>% 
    addPolygons(data=watersheds, 
                fillColor = "transparent", 
                stroke=T, 
                color="#08306B", # polygon border color
                weight=3, # polygon border weight
                label=paste(watersheds@data$Name),
                group = "Watersheds") %>% 
    
  # add layer control panel 
    addLayersControl(
      #    baseGroups = c("Basemap"),
      overlayGroups = c('Indicator Data',"Delta + Suisun Marsh Boundary", "Location Pin", "Watersheds"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
      hideGroup("Watersheds")
    
})





## Data Explorer ###########################################

SVI_clean <- sovi@data %>%
  dplyr::select(
    "Census Block Group ID",
    "City" = NAME,
    "County" = County,
    "Estimated Population" = est_pop,
    "Median household income" = MHHI,
    "Median household income as % of state household income" = PCT_MHHI,
    "Total Score" = TOTAL_SCOR,
    "Vulnerability Rating" = VULNBLTY,
    "% popn. <5" = "% pop <5",
    "Under 5 popn. >70th percentile?",
    "% of households with one or more nonwhite person" = "% nonwhite households",
    "Nonwhite households >70th percentile?",
    "% of single-person households >64" = '% single households >64',
    "Solo >64 popn. >70th percentile?",
    "% of adults >25 without HS diploma or GED" = '% without GED/diploma',
    "Adults without GED >70th percentile?",
    "% of households that include someone with a disability" = "% disabled households", 
    "Disabled households >70th percentile?",
    "% of tenant-occupied housing units" = "% rentals",
    "Renters >70th percentile?",
    "% of households with no vehicle access" = "% no-car households",
    "Lack of vehicle access >70th percentile?",
    "% of households with limited English proficiency" = "% limited English households",
    "Limited English proficiency >70th percentile?",
    "% of households with income <200% of the federal poverty line" = "% of households in poverty",
    "Poverty >70th percentile?",
    "% of babies born at low birth weight" = "% low birth weight",
    "Low birth weights >70th percentile?",
    "Age-adjusted rate of asthma ER visits per 10,000" = "Asthma ER visits per 10k",
    "Asthma ER visits >70th percentile?",
    "Age-adjusted rate of heart-attack ER visits per 10,000" = "Heart-attack ER visits per 10k",
    "Heart attack ER visits >70th percentile?",
    "% of households without health insurance" = "% uninsured households",
    "Uninsured households >70th percentile?",
    "Low food access (y/n)" = "Food desert")

output$table <- DT::renderDataTable(DT::datatable(SVI_clean, options = list(paging=FALSE)))


}


