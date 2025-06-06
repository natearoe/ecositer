% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmds_taxon_comparison.R
\name{nmds_taxon_comparison}
\alias{nmds_taxon_comparison}
\title{NMDS taxon comparison}
\usage{
nmds_taxon_comparison(veg_df, taxon, rare_species, min_plots)
}
\arguments{
\item{veg_df}{a properly formatted vegetation dataframe}

\item{taxon}{the taxon of interest}

\item{rare_species}{TRUE or FALSE to include or remove, respectively}

\item{min_plots}{minimum number of plots species where species is present.}
}
\value{
a NMDS of taxon of interest
}
\description{
NMDS taxon comparison
}
