\name{safety}
\alias{safety}
\title{
Calculate an indicator of safety based on the inequality indicators.
}
\usage{
safety(lng, lat)
}
\description{
This function was built to make sure refugee camps are set in safe places. The indicator chosen for this task was the inequality indicator. The higher this indicator is, the more dangerous a place is considered.
The output of this function is a dataframe containing longitude, latitude and a binary value.
This binary value will be 1 if the inequality not too high (lower than the average). Otherwise, this value will be a 0.
}

\examples{
lng=24
lat=35
safety(lng,lat)
}
