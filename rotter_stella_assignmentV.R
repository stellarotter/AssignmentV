
# initial setup

rm(list = ls())

if (!require("httr")) install.packages("httr")
if (!require("devtools")) install.packages("devtools")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("rlist")) install.packages("rlist")

library(httr)
library(devtools)
library(tidyverse)
library(jsonlite)
library(rlist)
library(dplyr)

# set apikey
source("apikey.R")

countrycode <- "FR"


#******************************************************************************

# exercise 3: extract (first) venues

# retrieve data
venueGE_raw <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                   query = list(apikey = apikey,
                   countryCode = countrycode,
                   locale = "*"))

# extract content (for overview)
content <- content(venueGE_raw)

# extract json
json_content_page <- content(venueGE_raw, as = 'text', encoding = "UTF-8")

# write to data frame & subset
venueGE <- data.frame(fromJSON(json_content_page, flatten=TRUE)[["_embedded"]][["venues"]]) %>%
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")


# exercise 4: extract all venues

# get json to get totalElements
venue_json <- fromJSON(json_content_page, flatten = TRUE)

n <- as.numeric(venue_json[["page"]][["totalElements"]])
print(n)

# set size up to increase speed
size = 200

# Number of complete pages:
maxpage <- floor(n/size)
print(maxpage)

# Number of entries on the last incomplete page:
remainder <- n-size*floor(n/size)
print(remainder)

# We initiate a dataframe in the correct dimensions to speed up our loop:
allvenues_short <- data.frame(
  name  = character(n),
  city   = character(n),
  postalCode = character(n),
  address   = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n),
  stringsAsFactors = FALSE)

# loop over pages and paste data into predefined dataframe
for (i in 1:(maxpage)) {
  all_venues <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                   query = list(apikey = apikey,
                                countryCode = countrycode, 
                                locale = "*",
                                page   = (i-1),
                                size = 200))
  
  json_content <- content(all_venues, as = "text", encoding = "UTF-8")
  
  # parse content to json 
  venue_json <- fromJSON(json_content, flatten = TRUE)[["_embedded"]][["venues"]]
  
  # if column doesn't exist put NA
  venue_json$name[is.null(venue_json$name)] <- NA
  venue_json$city.name[is.null(venue_json$city.name)] <- NA
  venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
  venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
  venue_json$url[is.null(venue_json$url)] <- NA
  venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
  venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA
  
  
  allvenues_short[(size * i - (size-1)):(size * i),] <- data.frame(venue_json) %>%
    #select colums
    select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")
  
  # pause in loop
  Sys.sleep(0.5)
}


# The last page is incomplete, hence we add it manually outside 
# the loop, as in this particular case, the loop would cause problems otherwise:
all_venues <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues/?",
                 query = list(apikey = apikey,
                              countryCode = countrycode, 
                              locale = "*",
                              # start at page 0 as current page number counted from 0
                              page   = i,
                              size = remainder))

json_content <- content(all_venues, as = "text", encoding = "UTF-8")

# parse content to json 
venue_json <- fromJSON(json_content, flatten = TRUE)[["_embedded"]][["venues"]]

# Replace column by "NA" if it does not exists on each page
venue_json$name[is.null(venue_json$name)] <- NA
venue_json$city.name[is.null(venue_json$city.name)] <- NA
venue_json$postalCode[is.null(venue_json$postalCode)] <- NA
venue_json$address.line1[is.null(venue_json$address.line1)] <- NA
venue_json$url[is.null(venue_json$url)] <- NA
venue_json$location.longitude[is.null(venue_json$location.longitude)] <- NA
venue_json$location.latitude[is.null(venue_json$location.latitude)] <- NA

last_page <- data.frame(venue_json) %>%
  #select colums
  select("name", "city.name", "postalCode", "address.line1", "url", "location.longitude", "location.latitude")

allvenues_short[(size * (i+1) - (size-1)):n,] <- last_page
allvenues_short["country"] <- countrycode




# save dataframe with German data
#allvenues_shortDE <- allvenues_short

# save dataframe with French data
#allvenues_shortFR <- allvenues_short




# PLOT - GERMANY

# get class of columns & transform coordinates to numeric
# prepare data for plot
plotdataDE <- allvenues_shortDE

sapply(plotdataDE, class) 
plotdataDE[c("longitude", "latitude")] <- sapply(plotdataDE[c("longitude", "latitude")],as.numeric)

# drop outlier observations
plotdataDE <- subset(plotdataDE, latitude > 47.271679 & latitude < 55.0846)
plotdataDE <- subset(plotdataDE, longitude > 5.866944 & longitude < 15.043611)

# plot venues
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), data = map_data("world", region = "Germany"),
    fill = "grey90",color = "black") +
  theme_void() + coord_quickmap() +
  labs(title = "Event locations across Germany", caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic")) +
  geom_point(aes(x = longitude, y = latitude),
           data = plotdataDE,
           color = "darkblue",
           alpha = 0.8,
           size = 1,
           shape = 18)


# PLOT - FRANCE

# get class of columns & transform coordinates to numeric
plotdataFR <- allvenues_shortFR

sapply(plotdataFR, class) 
plotdataFR[c("longitude", "latitude")] <- sapply(plotdataFR[c("longitude", "latitude")],as.numeric)

# drop outlier observations
plotdataFR <- subset(plotdataFR, latitude > 42.283333 & latitude < 51.083333)
plotdataFR <- subset(plotdataFR, longitude > -4.783333 & longitude < 9.25)

# plot venues
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), data = map_data("world", region = "France"),
    fill = "grey90",color = "black") +
  theme_void() + coord_quickmap() +
  labs(title = "Event locations across France", caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic")) +
  geom_point(aes(x = longitude, y = latitude),
             data = plotdataFR,
             color = "#1F9804",
             alpha = 0.8,
             size = 1,
             shape = 18)
