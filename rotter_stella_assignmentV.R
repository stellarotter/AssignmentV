
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
venueGE <- data.frame(fromJSON(json_content, flatten=TRUE)[[1]][[1]]) %>%
  select('name', 'city.name', 'postalCode', 'address.line1', 'url', 'location.longitude', 'location.latitude')



# exercise 4: extract all venues

#n <- as.numeric(venueGE[["totalElements"]])
n <- 100
print(n)

# Number of complete pages:
pages <- floor(n/20)
print(pages)

# Number of entries on the last incomplete page:
remainder <- n-20*floor(n/20)
print(remainder)

# We initiate a dataframe in the correct dimensions to speed up our loop:
allvenues_short <- data.frame(
  name  = character(n),
  city.name   = character(n),
  postalCode = character(n),
  address.line1   = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n))

# We loop over the complete pages with 10 entries each:
for (i in 1:pages) {
  allvenues <- GET("https://app.ticketmaster.com/discovery/v2/venues?", 
                    query = list(apikey = apikey,
                                 countryCode = "DE",
                                 pages   = i))
  
  # extract json
  json_content <- content(allvenues, as = 'text')

  # We gradually fill our dataframe page by page (lines 1-10 in the first 
  # iteration, lines 11-20 in the second iteration, ...):
  allvenues_short[(20 * i - 19):(20 * i),] <- data.frame(fromJSON(json_content, flatten=TRUE)[[1]][[1]]) %>%
    select('name', 'city.name', 'postalCode', 'address.line1', 'url', 'location.longitude', 'location.latitude')
  
  
  # pause in loop
  Sys.sleep(0.2)
}




















# The last page is incomplete, hence we add it manually outside 
# the loop, as in this particular case, the loop would cause problems otherwise:
i <- i + 1
res_movies <- GET("http://www.omdbapi.com/?", 
                  query = list(apikey = omdb_key,
                               s      = "batman",
                               type   = "movie", 
                               page   = i))

movies_content <- content(res_movies)

movies_df_short[(10 * i - 9):(n),] <- list.stack(movies_content$Search)
