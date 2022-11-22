library(sf)
library(sfheaders)
library(mapview)

# FUNCTIONS #####################################################

splitLoops <- function(x, dist_tolerance = 0.01){
  # For use only with local UTM CRS (Easting, Northing)
  
  #x: an sf linestring with multiple features
  #dist_tolerance: 'snap' distance for un-closed loops
  
  x$order <- 1:nrow(x)
  newdata <- x
  
  for(i in 1:nrow(x)){
    
    # Select a line segment
    si <- x[i,]
    si_coords <- st_coordinates(si)[,1:2]
    
    si_sf <- st_multipoint(si_coords)              
    si_sf = st_sfc(si_sf, crs = st_crs(x))
    si_sf <- st_as_sf(st_cast(si_sf, "POINT"))
    
    # Find if vertex is duplicated (i.e., closed line string)
    duplicated_coord <- duplicated(si_coords)
    
    if(any(duplicated_coord)){
      
      # Split segment at duplicate coordinate
      dupliciated_ind <- which(duplicated_coord)
      
      split1_coords <- si_coords[1:dupliciated_ind,]
      split2_coords <- si_coords[dupliciated_ind:nrow(si_coords),] 
      
      split1_line <- st_sf(st_sfc(st_linestring(split1_coords), crs = st_crs(x)))
      st_geometry(split1_line) <- "geometery"
      split2_line <- st_sf(st_sfc(st_linestring(split2_coords), crs = st_crs(x)))
      st_geometry(split2_line) <- "geometery"
      
      # Add back fields
      split1_line <- as_Spatial(split1_line)
      split2_line <- as_Spatial(split2_line)
      split1_line@data <- st_drop_geometry(si)
      split2_line@data <- st_drop_geometry(si)
      
      split_si <- st_as_sf(rbind(split1_line, split2_line))
      
      newdata <- newdata[-which(newdata$order == i),] 
      newdata <- rbind(newdata, split_si)
      
    } else {
      
      # Search for intersections
      
      si_buff <- st_buffer(si_sf, dist = dist_tolerance)
      
      # Explode line segment
      
      n_expl <- nrow(si_coords)-1
      expl_coords <- data.frame(X = rep(NA, n_expl*2), 
                                Y = rep(NA, n_expl*2))
      expl_coords$L1 <- NA
      
      mod_row <- (1:n_expl* 2) -1
      
      l = 0
      for(j in mod_row){
        l = l + 1
        expl_coords[j, 1:2] <- si_coords[l, 1:2]
        expl_coords[j+1, 1:2] <- si_coords[l+1, 1:2]
        
        expl_coords[j, 3] <- l
        expl_coords[j+1, 3] <- l
      }
      
      expl_line <- sfheaders::sf_multilinestring(expl_coords, x = "X", y = "Y", multilinestring_id = "L1")
      st_crs(expl_line) <- st_crs(x)
      mapview(expl_line) + mapview(si_sf)
      
      expl_int <- st_intersects(si_buff, expl_line)
      
      # Look for out of sequence intersects
      unclosed_int <- sapply(expl_int, diff) > 1
      unclosed_int[is.na(unclosed_int)] <- FALSE

      if(any(unclosed_int)){
        
        point_ind <- which(unclosed_int == TRUE)
        line_ind <- expl_int[[point_ind]][1]
        
        split_point <- si_sf[unclosed_int,]
        mapview(expl_line) + mapview(split_point)
        
        # Insert new point into coordinates
        split1_coords <- rbind(si_coords[1:line_ind,], st_coordinates(split_point))
        split2_coords <- rbind(st_coordinates(split_point), si_coords[(line_ind+1):nrow(si_coords),] )
        
        split1_line <- st_sf(st_sfc(st_linestring(split1_coords), crs = st_crs(x)))
        st_geometry(split1_line) <- "geometery"
        split2_line <- st_sf(st_sfc(st_linestring(split2_coords), crs = st_crs(x)))
        st_geometry(split2_line) <- "geometery"
        
        # Add back fields
        split1_line <- as_Spatial(split1_line)
        split2_line <- as_Spatial(split2_line)
        split1_line@data <- st_drop_geometry(si)
        split2_line@data <- st_drop_geometry(si)
        
        split_si <- st_as_sf(rbind(split1_line, split2_line))
        
        # Update new line feature
        newdata <- newdata[-which(newdata$order == i),] 
        newdata <- rbind(newdata, split_si)
        
      }
    }
  }
  
  # Make order of features the same as the input feature layer
  newdata[order(newdata$order),]
  newdata$oder <- NULL
  
  return(newdata)
}

# RUN SPLIT FUNCTION ###############################

setwd("Data")

# Load shapefile using sf package
# *Note the projected CRS needs to be UTM (easting, northing)
roads <- st_read("GIS_DATA_ROADSEGMENT.shp")
mapview(road)

# Apply split loop function
new_roads <- splitLoops(roads, dist_tolerance = 0.001)

# Visualize results
new_roads$new_id <- 1:nrow(new_roads)
mapview(new_roads, zcol = "new_id")
