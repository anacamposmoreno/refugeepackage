#' @title Search for water around point
#' @description
#' As established in the SPHERE manual on Minimum Standards for Camp Management, the camp should
#' have access to water no more than 500m away. This function detects the presence of streams in
#' such area.
#' The output of this function is a dataframe containing longitude, latitude and a binary
#' value. This binary value will be 1 is there is a stream within reasonable distance.
#' Otherwise, this value will be a 0.
#' Note that the buffer can be set to a different value.
#' Note that NAs are set to 1 in order to avoid excluding any suitable areas.
#' @param lng is the longitude of the point
#' @param lat is the latitude of the point
#' @param buffer is the radius of the distance. It's default value is set to 500m.
#' @return A dataframe containing longitude, latitude and a binary value (a=1 if the area is suitable, a=0 otherwise)
#' @import tidyverse
#' @import osmdata
#' @import sf
#' @import ggmap
#' @import tmap
#' @import raster
#' @import sp
#' @import rgeos
#' @import rgdal
#' @import leaflet
#' @export WaterAround
#' @examples
#' lat= 10
#' lng=25
#' WaterAround(lng,lat)
#'
#'

WaterAround <- function(lng, lat, buffer = 500) {
df <- data.frame(lon = lng, lat = lat)
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+init=epsg:4326")

q <- df %>%
  st_as_sf() %>%
  st_buffer(buffer) %>%
  opq() %>%
  add_osm_feature("waterway", "stream")

result <- osmdata_sf(q)

if(nrow(result$osm_points) == 0) {
  a = 0} else {a = 1} #counts
mydf <- data.frame(long=lng, lat=lat)
mydf$water <-  a
return(mydf)
}



