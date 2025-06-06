% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sites_in_state_phase.R
\name{sites_in_state_phase}
\alias{sites_in_state_phase}
\title{Identify siteid(s)/vegplotid(s) in states/phases of STMs}
\usage{
sites_in_state_phase(veg_df)
}
\arguments{
\item{veg_df}{a formatted vegetation dataframe created using
\link[ecositer]{formatted_veg_df}}
}
\value{
a nested list of ecosites - states/phases - and the siteids associated
with each state/phase
}
\description{
\code{sites_in_state_phase} is a way of determining what siteids are associated with
an ecosite state/phase.
}
\examples{
example_data <- access_example_data()
veg_df <- formatted_veg_df(static_location = example_data)
# user-defined static location -
# veg_df <- formatted_veg_df(static_location =
"C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
my_state_phase <- sites_in_state_phase(veg_df = veg_df)
head(my_state_phase)

}
