% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegStaticNASIS.R
\name{vegStaticNASIS}
\alias{vegStaticNASIS}
\title{Create vegetation data Static NASIS}
\usage{
vegStaticNASIS(output_path)
}
\arguments{
\item{output_path}{}
}
\value{
a static NASIS database that can be called independent of your current selected set in NASIS.
}
\description{
\code{vegStaticNASIS} saves a NASIS selected set to your local drive. The function utilizes
\code{soilDB::createStaticNASIS}, and includes the appropriate tables to access
vegetation data. The output can be used as a reliable, local copy of query results,
allowing the user to change their NASIS selected set and still access the
data of interest. The file extension needs to be ".sqlite" If you are working with a
dataset that commonly has new data entered, it is probably best to work off of your
NASIS selected set rather than using a static, local sqlite database.
}
\details{
An appropriate queries to run to populate your selected set include: MLRA13_Wasilla >
Pedon/Site/Transect/Vegetation Plot by usiteid (multiple) & MLRA13_Wasilla >
Pedon/Site/Vegetation Plot by site area overlap table

Code written by Andrew Brown
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
vegStaticNASIS(output_path = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg.sqlite")
\dontshow{\}) # examplesIf}
}
