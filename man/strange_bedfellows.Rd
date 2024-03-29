% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strange_bedfellows.R
\name{strange_bedfellows}
\alias{strange_bedfellows}
\title{Strange bedfellows}
\usage{
strange_bedfellows(site_sim)
}
\arguments{
\item{site_sim}{the results of (\link[ecositer]{site_similarity_pairwise})}
}
\value{
plot names and percentile values. The percentile values can be used to
identify plots that have unusual plant communities. Lower percentile values
have more unusual assemblages and are worth QCing to determine if species
identifications are correct.
}
\description{
\code{strange_bedfellows} can be used to identify plots with unusual plant communities.
This could indicate incorrect identifications or unique assemblages
}
\examples{
ecositer::strange_bedfellows(site_sim = my_site_sim)
}
