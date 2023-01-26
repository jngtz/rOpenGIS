# FOR GETTING GEOTAB GPS LOG DATA USING THE MYGEOTAB API
# By Jason Goetz (jasonngoetz@gmail.com)

library(httr)
library(jsonlite)

# Define API URL
geotab_api <- "https://YOURSERVERNUMBER.geotab.com/apiv1/Get?typeName=LogRecord"

# Define query "credential" and "search" paremeters as a JSON

my_credentials <- list(
  userName = "YOURUSERNAME",
  database = "",
  password = "YOURPASSWORD"
)

my_search <- list(
  deviceSearch = list(id = "b49E"),
  fromDate = "2022-11-01T05:00:00Z",
  toDate = "2022-12-01T05:00:00Z",
  resultsLimit = "50000"
)

my_credentials <- toJSON(my_credentials, auto_unbox = TRUE, pretty = TRUE)
my_search <- toJSON(my_search, auto_unbox = TRUE, pretty = TRUE)

my_query <- list(
  credentials = my_credentials,
  search = my_search
)

# Get response from API
api_response <- GET(url = geotab_api, query = my_query)

# Extract data from response
data <- content(api_response)$result

# Convert data to a table
require(data.table)
d <- rbindlist(data)

# Clean data if structure of column is a list
d$device <- unlist(d$device)

# Format date-times
d$dateTime_UTC <- d$dateTime
date_tmp <- as.Date(d$dateTime)
time_tmp <- substr(d$dateTime, 12,19)

dateTime_UTC <- as.POSIXct(paste(date_tmp, time_tmp), tz = "UTC")
dateTime_EST <- format(dateTime_UTC, tz="EST", usetz=TRUE)

# Convert to local time (e.g. EST)
d$time_EST <- format(strptime(dateTime_EST,format='%Y-%m-%d %H:%M:%S'), '%H:%M:%S')
d$date_EST <- format(strptime(dateTime_EST,format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')

# Convert to a spatial object
require(sf)
d_sf <- st_as_sf(d, coords = c("longitude", "latitude"))
st_crs(d_sf) <- 4326 # WGS 84

# View GPS points
require(mapview)
mapviewOptions(platform = "leafgl") # for fast rendering of many points
mapview(d_sf)
