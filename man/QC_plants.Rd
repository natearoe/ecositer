% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_plants.R
\name{QC_plants}
\alias{QC_plants}
\title{QC plants}
\usage{
QC_plants(veg_df)
}
\arguments{
\item{veg_df}{}
}
\value{
a dataframe of plants recommended for
}
\description{
\code{QC_plants()} takes a \emph{properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df})}
and provides a dataframe of plants that are recommended for QC, including plant names and the number
of times that each plant name was used. All species within genera identified to the genera or subspecies level
are recommended for QC.
}
\examples{
QC_plants(veg_df = ecositer::vegetation_dataframe) |> head()
}
