% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/big_beta_diversity.R
\name{big_beta_diversity}
\alias{big_beta_diversity}
\title{Big beta diversity}
\usage{
big_beta_diversity(
  veg_df,
  wisconsin = FALSE,
  remove_rare = FALSE,
  relative_dist = TRUE,
  trans = TRUE
)
}
\arguments{
\item{veg_df}{a properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df}).
The data should be QCed to ensure that it meets minimum data requirements (\link[ecositer]{QC_vegplots})}

\item{remove_rare}{decimal value defining the fraction of sites a species must be present in to be included
(e.g., 0.01 means a species must be present in > 1\% of plots). FALSE to include all species in dataset.}

\item{relative_dist}{relativized 0-1 dissimilarity values using: \deqn{z_i = \frac{x_i - min(x)}{max(x) - min(x)}}}
}
\value{
a distance matrix. This distance matrix can be used for ordination or to determine the similarity between
different ecological sites.
}
\description{
\code{big_beta_diversity} calculates a distance matrix using the distance measure, big beta diversity.
}
\examples{
my_bbd <- big_beta_diversity(veg_df = my_veg_df)
}
