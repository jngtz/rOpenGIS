# MOVE LOOP/CUL-DE-SAC START/END POINT TO INCOMING LINE SEGEMENT
# By Jason Goetz (jasonngoetz@gmail.com)

# These functions are used to change the position of the start and end points
# of any bulb features to match entrance roads 

library(sf)
library(sp)

eucliDist <- function(a, b) sqrt(sum((a - b)^2))

moveBulbStartEnd <- function(roads, bulbs, only.bulbs = FALSE,
                             near.dist = 2){
  
  # roads: road segments without cul-de-sacs / bulbs; 
  #        object of class sf, of SpatialLinesDataFrame
  # bulbs: ony cul-de-sac / bulb road segments
  #        object of class sf, of SpatialLinesDataFrame
  # near.dist: distance threshold for finding entrance road to cul-de-sac
  # only.bulbs: logical; if TRUE return only cul-de-sac/bulb objects, otherwise
  #             return roads and cul-de-sac objects in one feature

  # This function works by:
  #   1.  Checking all input parameters in the correct object class
  #   2.  Checking if the cul-de-sac / bulb objects are closed 
  #       (start/end = same point)
  #   3.  Finds the cul-de-sac vertex nearest to the cul-de-sac entrance
  #   4.  Changes order of vertices to start/end with (2.^above)
  #   5.  Returns a sf class object with main roads and bulbs as one feature
  #       or just bulbs if only.bulbs = TRUE.
  
  # Data preparation - data types
  
  # Coerce bulbs to SpatialLines class {sp}
  if(class(bulbs)[1] == "sf"){
    geom <- sf::as_Spatial(st_geometry(bulbs))
  } else if(class(bulbs)[1] == "SpatialLinesDataFrame"){
    geom <- bulbs
  }
  
  # Make sure roads is sf class {sf}
  if(class(roads)[1] == "SpatialLinesDataFrame"){
    roads <- sf::st_as_sf(roads)
  }
  
  # Check which BULBs are closed: start and end point should be the same
  is_closed <- rep(NA, length(geom))
  
  for(i in 1:length(geom)){
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    first_point <- geom_crd[1,]
    last_point <- geom_crd[nrow(geom_crd),]
    is_same <- all(first_point == last_point)
    
    is_closed[i] <- is_same
    
  }
  

  # Find cul-de-sac/bulb vertex nearest to entrance/intersecting road
  
  near_crds <- data.frame(x = rep(NA, nrow(bulbs)), y = NA, index = NA)

  for(j in 1:nrow(bulbs)){
    
    # Find closest road segment to each bulb 
    
    bulb <- bulbs[j,]
    int_road <- roads[unlist(st_is_within_distance(bulb, roads, dist = near.dist)),] # closest road segment
    
    # Coerce to SpatialLines
    geom_bulb <- sf::as_Spatial(st_geometry(bulb))
    bulb_crd <- geom_bulb@lines[[1]]@Lines[[1]]@coords
    
    geom_road <- sf::as_Spatial(st_geometry(int_road))
    road_crd <- geom_road@lines[[1]]@Lines[[1]]@coords
    
    
    # Find distance of bulb vertices to nearest road segment vertices
    dist_l <- list()
    for(k in 1:nrow(bulb_crd)){
      
      bulb_test <- bulb_crd[k,]
      
      distance <- rep(NA, nrow(road_crd))
      for(i in 1:nrow(road_crd)){
        road_test <- road_crd[i,]
        distance[i] <- eucliDist(bulb_test, road_test)
      }
      
      dist_l[[k]] <- distance
    }
    
    # Determine closet vertex to road segment
    min_dist <- min(unlist(dist_l), na.rm=FALSE)
    mat_dist <- matrix(unlist(dist_l), nrow=length(dist_l), byrow=TRUE)
    min_ind <- which(mat_dist == min_dist, arr.ind = TRUE)
    nearest_bulb_crd <- min_ind[1]
    nearest_crd <- bulb_crd[nearest_bulb_crd,]
    near_crds$x[j] <- nearest_crd[1]
    near_crds$y[j] <- nearest_crd[2]
    near_crds$index[j] <- nearest_bulb_crd
    
  }
  

  # Re-order line verticies to start/end with bulb vertex nearest to roads
  
  geom_bulbs <- as_Spatial(bulbs)
  
  for(i in 1:nrow(bulbs)){
    
    vertex_ind <- near_crds$index[i]
    
    if(vertex_ind > 1 && is_closed[i] == TRUE){
      
      # find location of nearest coordinate
      crd <- near_crds[i,]
      bulb <- bulbs[i,]
      
      geom_bulb <- as_Spatial(st_geometry(bulb))
      bulb_crd <- geom_bulb@lines[[1]]@Lines[[1]]@coords
      
      # remove last bulb coordinate (vertex)
      bulb_crd <- bulb_crd[1:nrow(bulb_crd)-1,]
      
      # reorder vertex coordinate list
      split_above <- bulb_crd[1:vertex_ind -1,]
      split_below <- bulb_crd[vertex_ind:nrow(bulb_crd),]
      
      new_order <- rbind(split_below, split_above)
      
      # close bulb by making first and last coordinate the same
      new_order <- rbind(new_order, new_order[1,])
      
      # assign new vertex coordinates to SpatialLines object
      geom_bulbs@lines[[i]]@Lines[[1]]@coords <- new_order

    }
    
  }
  
  # Remove unwanted attributes/fields/columns
  geom_bulbs$is_closed <- NULL
  
  # Coerce to sf class object
  sf_geombulbs <- st_as_sf(geom_bulbs)
  
  # Fix when geom field names does match (e.g. geometry vs geom)
  geom_inputname <- names(roads)[length(names(roads))]
  geom_outputname <- names(sf_geombulbs)[length(names(sf_geombulbs))]
  
  if(geom_inputname != geom_outputname){
    st_geometry(sf_geombulbs) <- geom_inputname
  }
  
  # Return roads + bulbs or only bulbs
  if(only.bulbs == TRUE){
    export <- sf_geombulbs
  } else {
    export <- rbind(roads,sf_geombulbs)
  }
  
  return(export)
  
}



getStartVertex <- function(x){
  
  # Coerce bulbs to SpatialLines class {sp}
  if(class(x)[1] == "sf"){
    geom <- sf::as_Spatial(st_geometry(x))
  } else if(class(x)[1] == "SpatialLinesDataFrame"){
    geom <- x
  }
  
  is_closed <- rep(NA, length(geom))
  start_points <- data.frame(x = rep(NA, length(geom)), y = NA)
  
  for(i in 1:length(geom)){
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    first_point <- geom_crd[1,]
    last_point <- geom_crd[nrow(geom_crd),]
    is_same <- all(first_point == last_point)
    
    is_closed[i] <- is_same
    
    start_points$x[i] <- first_point[1]
    start_points$y[i] <- first_point[2]
    
  }

  # Explore position of start points
  coordinates(start_points) = ~x+y
  proj4string(start_points) <- proj4string(geom)
  
  return(start_points)
  
}

