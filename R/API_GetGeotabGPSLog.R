library(httr)
library(jsonlite)

# Define API URL
api_url <- "https://my52.geotab.com/apiv1/Get?typeName=LogRecord"

# Define query paremeters "credentials" and "search"
query_params <- list(
  credentials = noquote('{"userName":"YOURUSERNAME","database":"","password":"YOURPASSWORD"}'),
  search = noquote('{"deviceSearch":{"id":"YOURDEVICEID"},"fromDate":"2022-11-01T05:00:00Z","toDate": "2022-12-01T05:00:00Z","resultsLimit":50000}')
)

# Get response from API
res <- GET(url = api_url, query = query_params)

# Extract data from response
data <- content(res)$result

# Convert data to a table
require(data.table)
d <- rbindlist(data) 

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
