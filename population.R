#' @title Check that the population density is low enough
#' @description
#' This function was built to avoid building refugee camps in over populated areas.
#' The output of this function is a dataframe containing longitude, latitude and a binary value.
#' This binary value will be 1 is the population density is not too high and, therefore, the resources of the area are adequate for the camp. Otherwise, this value will be a 0.}
#' @param lng is the longitude of the point
#' @param lat is the latitude of the point
#' @return A dataframe containing longitude, latitude and a binary value (a=1 if the area is suitable, a=0 otherwise)
#' @export population
#' @examples
#' lat= 10
#' lng=25
#' population(lng,lat)
#'

population <- function(lng, lat) {
df <- data.frame(lon = lng, lat = lat)
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+init=epsg:4326")
data("World")
df_sf <-st_as_sf(df)
  inter_result <-st_intersection(World, df_sf)
  pop <- as.numeric(inter_result["pop_est_dens"])
  pop <- pop[1]
if (pop < mean(World$pop_est_dens) || is.na(pop)){
    b=1} else {b=0}
  mydf <- data.frame(long=lng, lat=lat)
  mydf$population <-  b
  return(mydf)
}





