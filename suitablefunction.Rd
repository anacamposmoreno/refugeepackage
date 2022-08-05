\name{suitable}
\alias{suitable}
\title{Check whether an area is a suitable location for a Refugee Camp based on water, population density and safety indicators.
}

\usage{
suitable(lng, lat, buffer=500)
}

\description{
This function combines the WaterAround, population and safety functions, generating a clear output stating whether an area is a suitable location or not.
Check the description of WaterAround, population and safety functions for more information on individual conditions.
}

\examples{
lng=25
lat=36
suitable(lng, lat)
}
