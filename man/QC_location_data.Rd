% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_location_data.R
\name{QC_location_data}
\alias{QC_location_data}
\title{QC site location data}
\usage{
QC_location_data(veg_df, coordinate_format = NULL)
}
\arguments{
\item{veg_df}{vegetation dataframe, commonly created using \code{ecositer::create_veg_df()}}

\item{coordinate_format}{by default, 'NULL' allowing for function to be interactive - otherwise, must be "UTM", "DMS", or "DD".}
}
\value{
dataframe uneeded coordinate format columns removed
}
\description{
QC site location data
}
\examples{
ecositer::QC_location_data(veg_df = my_veg_df)
QC_location_data(veg_df = my_veg_df)
}
