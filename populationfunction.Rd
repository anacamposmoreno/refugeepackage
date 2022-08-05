\name{population}
\alias{population}
\title{Check that the population density is low enough}
\usage{
population(lng,lat)
}
\description{
This function was built to avoid building refugee camps in over populated areas.
The output of this function is a dataframe containing longitude, latitude and a binary value.
This binary value will be 1 if the population density is not too high (lower than average) and, therefore, the resources of the area are adequate for the camp. Otherwise, this value will be a 0.}

\examples{
lng=24
lat=35
population(lng,lat)
}

