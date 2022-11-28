# FOR TRIMMING LENGHTS OF LINE SEGEMENTS
# By Jason Goetz (jasonngoetz@gmail.com)

library(sf)
library(sp)


# Functions # 
eucliDist <- function(a, b) sqrt(sum((a - b)^2))

# Flip coordinates

reverseCoords <- function(x){
  
  #geom <- as_Spatial(st_geometry(x))
  geom <- x
  
  for (i in 1:length(geom)){
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    geom@lines[[i]]@Lines[[1]]@coords <- geom_crd[nrow(geom_crd):1,]
    
  }
  
  return(geom)
}

#

lineTrim <- function(x, dist = 10){
  
  if(class(x)[1] == "sf"){
    geom <- sf::as_Spatial(st_geometry(x))
  } else if(class(x)[1] == "SpatialLinesDataFrame"){
    geom <- x
  } else if(class(x)[1] == "SpatialLines"){
    geom <- x
  }
  
  
  for(i in 1:length(geom)){
  
    dist_0 <- dist
    skip <- FALSE
    
    geom_crd  <- geom@lines[[i]]@Lines[[1]]@coords
    
    # Trim from start point
    
    # plot(geom_crd, type = "l")
    # points(geom_crd)
    # 
    # start_pnt <- geom_crd[1,]
    # end_pnt <- geom_crd[2,]
    # 
    # points(matrix(end_pnt, ncol = 2), col = "blue")
    # points(matrix(start_pnt, ncol = 2), col = "red")

    # Find x,y coordinates along line for given distance (dist)
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


# test <- lineTrim(road, dist = 5) 
# rev_test <- reverseCoords(test)
# trim_test <- lineTrim(rev_test, dist = 5)
# mapview(trim_test)





