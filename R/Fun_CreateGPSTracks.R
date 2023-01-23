# A FUNCTION TO CREATE TRACKS FROM POINTS
# By Jason Goetz (jasonngoetz@gmail.com)

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
