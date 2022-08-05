\name{waterAround}
\alias{waterAround}
\title{Search for water around point}
\usage{
WaterAround(lng, lat, buffer = 500)
}
\description{
As established in the SPHERE manual on Minimum Standards for Camp Management, the camp should have access to water no more than 500m away. This function detects the presence of streams in such area.
The output of this function is a dataframe containing longitude, latitude and a binary value.
This binary value will be 1 if there is a stream within reasonable distance. Otherwise, this value will be a 0.
Note that the buffer can be set to a different value.
Note that NAs are set to 1 in order to avoid excluding any suitable areas.
}
\examples{
lat= 10
lng=25
WaterAround(lng,lat)
}
