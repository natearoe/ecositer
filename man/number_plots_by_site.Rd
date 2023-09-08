% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/number_plots_by_site.R
\name{number_plots_by_site}
\alias{number_plots_by_site}
\title{Number of plots by ecosite}
\usage{
number_plots_by_site(static_location)
}
\arguments{
\item{static_location}{the location where a static NASIS database has been
saved.}
}
\value{
a dataframe describing the number of pedons and vegplots recorded
for each ecosite
}
\description{
\code{number_plots_by_ecosite} describes the number of pedons and vegplots that
have been recorded for each ecosite. It also lists the states/phases
associated with each ecosite.
}
\examples{

\dontshow{

configDir <- tools::R_user_dir("ecositer", which="cache")
if (!dir.exists(configDir)) {
  dir.create(configDir, recursive=TRUE)
}
configFile <- file.path(configDir, "CA792_veg_data.sqlite")
if (!file.exists(configFile)) {
  destFile <- paste0(configDir, "/CA792_veg_data.zip")

  tryCatch(download.file(url = "https://github.com/natearoe/ecositer_data/blob/main/CA792_veg_data.zip?raw=TRUE",
                         destfile = destFile,
                         mode = "wb"),
           error=function(e) {
             message('An Error Occurred')
             print(e)
           },
           #if a warning occurs, tell me the warning
           warning=function(w) {
             message('A Warning Occurred')
             print(w)
             return(NA)
           })

  tryCatch(unzip(zipfile = destFile,
                 exdir = configDir),
           error=function(e) {
             message('An Error Occurred')
             print(e)
           },
           #if a warning occurs, tell me the warning
           warning=function(w) {
             message('A Warning Occurred')
             print(w)
             return(NA)
           }
  )
}

my_plots_by_site <- number_plots_by_site(static_location = configFile)
head(my_plots_by_site)

}

\dontrun{
my_plots_by_site <- number_plots_by_site(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
head(my_plots_by_site)
}

}