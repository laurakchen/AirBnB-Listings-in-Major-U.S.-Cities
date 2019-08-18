# author: Laura Chen
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(pander)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(rsconnect)
library(RColorBrewer)

# read in AirBnB dataset
airbnb <- read.csv("airbnb.csv")

airbnb <- airbnb %>% select(city, log_price, room_type, cleaning_fee, 
host_identity_verified, cancellation_policy, host_response_rate, 
review_scores_rating, number_of_reviews, first_review, last_review, 
host_since, latitude, longitude, description)

airbnb$price <- exp(airbnb$log_price)
airbnb <- airbnb %>% select(-log_price)

# get rid of rows with missing values
airbnb <- airbnb[complete.cases(airbnb), ]

# get rid of % signs
airbnb$host_response_rate <- as.numeric(sub("%", "", airbnb$host_response_rate))

airbnb$host_identity_verified <- fct_recode(airbnb$host_identity_verified,
                                            "True" = "t", "False" = "f")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  # App title
  titlePanel("AirBnB Data in Major Cities"),
  
  leafletOutput("mymap", width = 900, height = 750)
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%
      addCircleMarkers(lng = airbnb$longitude, lat = airbnb$latitude, 
                       clusterOptions = markerClusterOptions(),
                       popup = paste("Price: $", airbnb$price, "<br>",
                                     "Review Score: ", 
                                     airbnb$review_scores_rating, "%"))
  })
}

shinyApp(ui = ui, server = server)

