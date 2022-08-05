#' @title Measure safety using inquality indicators
#' @description
#' This function was built to make sure refugee camps are set in safe places.
#' The indicator chosen for this task was the inequality indicator. The higher this indicator
#' is, the more dangerous a place is considered. The output of this function is a dataframe
#' containing longitude, latitude and a binary value. This binary value will be 1 if the
#' inequality not too high (lower than the average). Otherwise, this value will be a 0.
#' @param lng is the longitude of the point
#' @param lat is the latitude of the point
#' @return A dataframe containing longitude, latitude and a binary value (a=1 if the area is suitable, a=0 otherwise)
#' @export safety
#' @examples
#' lat= 10
#' lng=25
#' safety(lng,lat)
#'

safety <- function(lng, lat) {

df <- data.frame(lon = lng, lat = lat)
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+init=epsg:4326")
data("World")
df_sf <-st_as_sf(df)
inter_result <-st_intersection(World, df_sf)
  world_ineq <- World %>%drop_na(inequality)
  ineq <- as.numeric(inter_result["inequality"])
  ineq <- ineq[1]
  if (ineq < mean(world_ineq$inequality) || is.na(ineq)){
    c=1} else {c=0}
mydf <- data.frame(long=lng, lat=lat)
mydf$safety <-  c
return(mydf)
}



