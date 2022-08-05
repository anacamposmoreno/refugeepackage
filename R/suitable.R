#' @title Check whether an area is a suitable location for a Refugee Camp based on water, population density and safety indicators.
#' @description
#' This function combines the WaterAround, population and safety functions, generating a clear
#' output stating whether an area is a suitable location or not. Check the description of
#' WaterAround, population and safety functions for more information on individual conditions.
#' @param lng is the longitude of the point
#' @param lat is the latitude of the point
#' @param buffer is the radius of the distance. It's default value is set to 500m.
#' @return A statement on whether the area is suitable or not
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
#' @export suitable
#' @examples
#' lat= 10
#' lng=25
#' suitable(lng,lat)
#'

suitable <- function(lng, lat, buffer=500){
  df <- data.frame(lon = lng, lat = lat)
  coordinates(df) <- c("lon", "lat")
  proj4string(df) <- CRS("+init=epsg:4326")
  final_list <- list()

  q <- df %>%
    st_as_sf() %>%
    st_buffer(buffer) %>%
    opq() %>%
    add_osm_feature("waterway", "stream")
  result <- osmdata_sf(q)

  if(nrow(result$osm_points) == 0) {
    a = 0} else {a = 1} #counts
  final_list <- append(final_list, a)

  data("World")
  df_sf <-st_as_sf(df)
  inter_result <-st_intersection(World, df_sf)
  pop <- as.numeric(inter_result["pop_est_dens"])
  pop <- pop[1]
  if (pop < mean(World$pop_est_dens) || is.na(pop)){
    b=1} else {b=0}
  final_list <- append(final_list, b)

  world_ineq <- World %>%drop_na(inequality)
  ineq <- as.numeric(inter_result["inequality"])
  ineq <- ineq[1]
  if (ineq < mean(world_ineq$inequality) || is.na(ineq)){
    c=1} else {c=0}
  final_list <- append(final_list, c)
  mydf <- data.frame(long=lng, lat=lat)
  mydf$water <- as.numeric(final_list[1])
  mydf$population <- as.numeric(final_list[2])
  mydf$safety <- as.numeric(final_list[3])
  mydf$total <- rowSums(mydf[,c('water', 'population', 'safety')])
  if(mydf['total'] ==3){return('Non basic conditions are violated. This could be a suitable region for a Refugee Camp')
  }else {return('Basic conditions were violated. This is not a suitable spot for a Refugee Camp')}

}

