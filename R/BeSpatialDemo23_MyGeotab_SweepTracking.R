# ABOUT ########################################################################

# BeSpatial'23 Vehicle Tracking Demo
# Activity Classification and Map Matching

# "Where and when are we sweeping?"

# Jason Goetz, PhD (jason.goetz@kitchener.ca)
# GIS Application Specialist
# Geospatial Data and Analytics, City of Kitchener

# May 2, 2023

# TASKS
# 1. Pull MyGeotab API log data and create tracks
# 2. Classify sweeping activity (on/off)
# 3. Explore summary statistics and metrics
# 4. Map matching tracks to roads


# LOAD LIBRARIES ###############################################################

library(httr) # Tools for working with URLs and HTTP
library(jsonlite) # A JSON parser and generator
library(sf) # Handling spatial vector data (binds GDAL, GEOS & PROJ tools)
library(dplyr) # Grammar to simplify data manipulation
library(mapview) # Quick interactive viewing of spatial data


# CUSTOM FUNCTIONS #############################################################

## Track builder ####
st_track <- function(x){
  # Fast way to build track with multiple line segments
  track <- as_Spatial(x)
  cc <- as.matrix(sp::coordinates(track))
  cc_comb <- cbind(cc[1:(nrow(cc)-1),], cc[2:(nrow(cc)),])
  list_result <- lapply(split(cc_comb,seq(nrow(cc_comb))), function(x){matrix(x, ncol = 2, byrow = TRUE)})
  line_segments <- lapply(list_result, st_linestring)
  final_sf <- st_sfc(line_segments ) %>% 
    st_sf('ID' = 1:length(.)) 
  
  st_crs(final_sf) <- st_crs(x)
  return(final_sf)
  
}

## Euclidean distance ####

eucliDist <- function(a, b){
  ed <- sqrt(sum((a - b)^2))
  return(ed)
} 

## Flip coordinates ####

reverseCoords <- function(x){
  
  #geom <- as_Spatial(st_geometry(x))
  geom <- x
  
  for (i in 1:length(geom)){
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    geom@lines[[i]]@Lines[[1]]@coords <- geom_crd[nrow(geom_crd):1,]
    
  }
  
  return(geom)
}

## Line trimmer ####

lineTrim <- function(x, dist = 10){
  
  if(class(x)[1] == "sf"){
    geom <- sf::as_Spatial(st_geometry(x))
  } else if(class(x)[1] == "SpatialLinesDataFrame"){
    geom <- x
  } else if(class(x)[1] == "SpatialLines"){
    geom <- x
  }
  
  
  for(i in 1:length(geom)){
    
    #for(i in 1:100){
    
    dist_0 <- dist
    skip <- FALSE
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    
    for(k in 1:nrow(geom_crd)){
      
      if(k == nrow(geom_crd)){
        skip <- TRUE
        break
      }
      
      start_pnt <- geom_crd[k,]
      end_pnt <- geom_crd[k + 1,]
      
      dist_se <- eucliDist(start_pnt, end_pnt) #start end
      
      if (dist_se < dist_0) {
        dist_0 <- dist_0 - dist_se
      } else if (dist_se > dist_0){
        t <- dist_0 / dist_se # ratio of distances
        x_t <- (1 - t)*start_pnt[1] + t*end_pnt[1]
        y_t <- (1 - t)*start_pnt[2] + t*end_pnt[2]
        xy_t <- matrix(c(x_t, y_t), ncol = 2)
        #geom@lines[[i]]@Lines[[1]]@coords[1,] <- xy_t
        break # stop loop
      }
      
      
    }
    
    if(skip == FALSE){
      #points(xy_t, col = "Green")
      # add trim coordinate
      new_crds <- rbind(xy_t, geom_crd[(k+1):nrow(geom_crd),])
      
      #lines(geom_crd[(k+1):nrow(geom_crd), col = "Red")
      
      # assign trimmed line to prev. coordinates
      geom@lines[[i]]@Lines[[1]]@coords <- new_crds
    }
    
    
  }
  
  return(geom)
  
}


# PULLING MYGEOTAB API DATA ####################################################

## Define credentials and create search query ####

# Define API URL
geotab_api <- "https://YOURSERVERNUMBER.geotab.com/apiv1/Get?typeName=LogRecord"

# Define query "credential" and "search" parameters as a JSON
my_credentials <- list(
  userName = "YOURUSERNAME",
  database = "",
  password = "YOURPASSWORD"
)

# Define search query - what device/vehicle and for which time period
my_search <- list(
  deviceSearch = list(id = "b49E"),
  fromDate = "2022-11-01T05:00:00Z",
  toDate = "2022-12-01T05:00:00Z",
  resultsLimit = "50000"
)

# Format query parameters to a json structure
my_credentials <- toJSON(my_credentials, auto_unbox = TRUE, pretty = TRUE)
my_search <- toJSON(my_search, auto_unbox = TRUE, pretty = TRUE)

my_credentials
my_search

# Combine in a list
my_query <- list(
  credentials = my_credentials,
  search = my_search
)

## Get and format json to a table ####
res <- GET(url = api_url, query = my_query)

# Convert data to a table
d <- fromJSON(rawToChar(res$content), flatten = TRUE)[['result']]


## Format date-times  ####
# logged in MyGeotab as UTC timezone

d$dateTime_UTC <- d$dateTime
date_tmp <- as.Date(d$dateTime)
time_tmp <- substr(d$dateTime, 12,19)

dateTime_UTC <- as.POSIXct(paste(date_tmp, time_tmp), tz = "UTC")

d$dateTime_Local <- as.POSIXct(as.character(dateTime_UTC, tz="America/New_York"))
d$date_local <- format(strptime(d$dateTime_Local,format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')
d$time_local <- format(strptime(d$dateTime_Local,format='%Y-%m-%d %H:%M:%S'), '%H:%M:%S')


# Check if working hours makes sense
hours_local <-  format(strptime(d$dateTime_Local,format='%Y-%m-%d %H:%M:%S'), '%H')
hist(as.numeric(hours_local), xlab = "Hour of the day")

# Get days
days <- levels(as.factor(d$date_local))

## Create spatial features - points and lines (tracks) ####

d_sf <- st_as_sf(d, coords = c("longitude", "latitude"))

# define CRS
st_crs(d_sf) <- 4326 # WGS84 (ESPG code)

d_sf$speed <- as.numeric(d_sf$speed)
d_sf$date_local <- as.factor(d_sf$date_local)


# 'Quickly' create tracks with individual segments
track_list <- list()
for(i in 1:length(days)){
  d_sf_sel <- d_sf[d_sf$date_local == days[i],]
  if(nrow(d_sf_sel) > 1){
    track_i <- st_track(d_sf_sel)
    track_i$device <- d_sf_sel$device[1]
    track_i$date <- d_sf_sel$date_local[1]
    track_list[[i]] <- track_i
  }
}
tracks <- do.call(rbind, track_list)

# Visualize the results
mapview(tracks, zcol = "date") + mapview(d_sf, zcol = "date_local")


# ACTIVITY CLASSIFICATION ######################################################

# Let's select/filter for one day for this vehicle
track <- filter(tracks, date == "2022-11-05")
log <- filter(d_sf, date_local == "2022-11-05")

# Can store units, but can make calculations tricky, let's make it just numeric
track$length_m <- as.numeric(st_length(track))

## Calculate velocity for each track segment ####
log$dateTime_UTC <- as.POSIXct(log$dateTime_UTC, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
log$time_s <- as.numeric(difftime(log$dateTime_UTC, "1970-01-01 00:00:00 UTC", units = "secs"))

# V = D/T
track$vel_kph <- NA
track$time_hrs <- NA
for(i in 1:nrow(track)){
  track$vel_kph[i] <- (track$length_m[i] / (log$time_s[i+1] - log$time_s[i])) * 3.6
  track$time_hrs[i] <- (track$length_m[i]/1000) / track$vel_kph[i] 
  track$time_local[i] <- log$time_local[i+1] # time when track completed.
}

# Clean NaN's when length and time = 0
track <- track[!is.na(track$time_hrs),]

sum(track$time_hrs)
mapview(track, zcol = "vel_kph", layer.name = "Velocity [kph]")


## Pre-process with median moving/rolling window (size = 3) filter ####
# To smooth out stop/go locations
track$filter_vel_kph <- track$vel_kph
for(i in 2:(nrow(track)-1)){
  
  vels <- c(track$vel_kph[i-1], track$vel_kph[i+1]) # 1 ahead and 1 behind
  track$filter_vel_kph[i] <- median(vels)
  
}

# Remove tracks with 0 length
track <- filter(track, length_m > 0)

mapview(track, zcol = "vel_kph", layer.name = "Velocity [kph]") +
  mapview(track, zcol = "filter_vel_kph", layer.name = "Filtered Velocity [kph]") 

## Apply rule based classification ####
track$status <- "Off"
track$status[track$vel_kph <= 12 & track$filter_vel_kph  <= 12] <- "On" 
track$status[track$filter_vel_kph  < 2 & track$vel_kph <2] <- "Off" 
track$status[track$vel_kph  == 0] <- "Off"

mapview(track, zcol = "status", layer.name = "Sweeping Status", color =c("#7F8C8D", "#1ABC9C")) +
  mapview(track, zcol = "vel_kph", layer.name = "Velocity [kph]")


# EXPLORE SUMMARY STATISTICS/METRICS ##########################################
## Driving behaviour ####
hist(track$vel_kph[track$status == "On"],
     xlab = "Sweeper Speed (km/h)",
     main = "Histogram of speed when sweeping")

# Average speed when sweeping
avg_sweep_speed <- round(mean(track$vel_kph[track$status == "On"]))
print(paste("The average sweeping speed was", avg_sweep_speed, "km/h "))


## Sweeping efficiency measures ####

# % of service km's gone to sweeping
sweepkm_per <- round(sum(track$length_m[track$status == "On"]) / sum(track$length_m) *100, 1)
print(paste0(sweepkm_per, "% of service km's used for sweeping"))

# % of service time gone to sweeping
sweephrs_per <- round(sum(track$time_hrs[track$status == "On"]) / sum(track$time_hrs) *100, 1)
print(paste0(sweephrs_per, "% of service time used for sweeping"))

# km's sweept per hour of service
sweepkm_per_hrs <- round(sum(track$length_m[track$status == "On"]/1000) / sum(track$time_hrs), 1)
print(paste0(sweepkm_per_hrs, " km of roads were swept per hour"))

# Rough GHG emission est. for diesel engine
fuel_efficiency <- 1.75 # km/liter
fuel_consumption <- (sum(track$length_m)/1000)/fuel_efficiency
co2_emission <- fuel_consumption * 2.68 # kg of CO2/liter fuel burned
print(paste("Roughly,", round(co2_emission), "kg of C02 emissions were produced"))
#!Note: not always the best metric, because GHG tends to be lower when not sweeping...


# MAP MATCHING #################################################################

#### Pre-processing road data ####

# Pull data from our open geo-data portal
roads_url <- "https://services1.arcgis.com/qAo1OsXi67t7XgmS/arcgis/rest/services/Roads/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# Read the data from the URL
roads <- st_read(roads_url)
mapview(roads, color = "#7F8C8D") + mapview(track)

# Our sweepers only do regional and city roads, so we can remove provincial roads
roads <- filter(roads, OWNERSHIP != "MTO")

# Project our data to the local CRS NAD83 UTM 17N
roads <- st_transform(roads, crs = 26917)
track <- st_transform(track, crs = 26917)

# Trim road segments to mitigate false positives at intersections
trim_road <- lineTrim(roads, dist = 10)

# Need to reverse coordinates to trim other end of segments
rev_test <- reverseCoords(trim_road)
trim_road <- lineTrim(rev_test, dist = 10) # 10 Meters

# Coerce to sf spatial object and add fields from road_segments
trim_road <- st_as_sf(trim_road)
trim_road$roadsegmentid <- roads$ROADSEGMENTID
trim_road$ownership <- roads$OWNERSHIP
trim_road$length_m <- roads$Shape__Length
trim_road$category <- roads$CATEGORY
trim_road$lanes <- roads$LANES
trim_road$trim_length_m <- as.numeric(st_length(trim_road))

# Buffer the trimmed roads
buffer_roads <- st_buffer(trim_road, dist = roads$LANES * 3.3 + 6, endCapStyle = "FLAT")
buffer_roads$length_m <- as.numeric(st_length(roads))

# Find intersections of track with roads
buffer_intersect <- st_intersects(st_as_sf(track), buffer_roads)
sweep_path <- buffer_roads[unlist(buffer_intersect),]
sweep_path <- sweep_path[!duplicated(sweep_path$roadsegmentid),]

# Calculate number of passes in general and for sweeper
track_on <- filter(track, status == "On") # only sweep (on) tracks

for(i in 1:nrow(sweep_path)){
  road_seg <- sweep_path[i,]
  
  # Vehicle passes
  intersect_lng <- as.numeric(sum(st_length(st_intersection(track, road_seg))))
  sweep_path$n_passes[i] <- as.integer(round(as.numeric(intersect_lng) / road_seg$trim_length_m,0))
  
  # Sweep (on) passes
  intersect_sweep_lng <- as.numeric(sum(st_length(st_intersection(track_on, road_seg))))
  sweep_path$on_passes[i] <- as.integer(round(as.numeric(intersect_sweep_lng) / road_seg$trim_length_m,0))
  
}

# Remove road segments with 0 passes
sweep_path <- sweep_path[sweep_path$n_passes > 0,]

mapview(sweep_path, zcol = "n_passes") + mapview(track)


## Post-processing clean-up ####

# Remove tracks w low % overlap
track_buffer <- st_union(sf::st_buffer(track, 10)) # 10 Meters
intersect_path <- st_intersection(sweep_path, track_buffer)

# Calculate the area of the intersection
intersection_area <- st_area(intersect_path)
sweep_area <- st_area(sweep_path)

# Proportion sweep_path overlapping with road segments
intersection_prop <- intersection_area / sweep_area

sweep_path$prop_intersect <- as.numeric(intersection_area / sweep_area)
mapview(sweep_path, zcol = "prop_intersect")

# Select sweep paths that have intersection prop > 0.36
sweep_path <- filter(sweep_path, prop_intersect > 0.36)
mapview(sweep_path, zcol = "n_passes") + mapview(track)

## Assign sweeping activity to roads ####

sweep_path$sweep_status <- "Off"
sweep_path$sweep_status[sweep_path$on_passes > 0] <- "On"

mapview(sweep_path, zcol = "sweep_status") + mapview(track, zcol = "status")

                        

## Match to road segment data ####

roads$sweep_status <- sweep_path$sweep_status[match(roads$ROADSEGMENTID, sweep_path$roadsegmentid)]
roads$n_passes <- sweep_path$n_passes[match(roads$ROADSEGMENTID, sweep_path$roadsegmentid)]

# Determine number of street sweeping passes
roads$sweep_passes <- sweep_path$on_passes[match(roads$ROADSEGMENTID, sweep_path$roadsegmentid)]
roads$sweep_passes[roads$sweep_status == "Off"] <- NA

mapview(roads, zcol = "sweep_passes") + 
  mapview(sweep_path, zcol = "sweep_status", hide = TRUE) + 
  mapview(track, zcol = "status", hide = TRUE)
