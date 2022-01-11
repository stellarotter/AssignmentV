
# initial setup

rm(list = ls())

if (!require("httr")) install.packages("httr")
if (!require("devtools")) install.packages("devtools")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("jsonlite")) install.packages("jsonlite")

library(httr)
library(devtools)
library(tidyverse)
library(jsonlite)

# set apikey
source("apikey.R")


# exercise 3: extract (first) venues Germany

# retrieve data
venueGE_raw <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                   query = list(apikey = apikey,
                   countryCode = "DE"))

# extract content (for overview)
content <- content(venueGE_raw)

# extract json
json_content <- content(venueGE_raw, as = 'text')
# write to data frame & subset
venueGE <- data.frame(fromJSON(json_content)[[1]][[1]]) %>%
  select('name', 'city', 'postalCode', 'address', 'url', 'location')

