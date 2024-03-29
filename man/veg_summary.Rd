% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/veg_summary.R
\name{veg_summary}
\alias{veg_summary}
\title{Produce vegetation summary of ecosites and states/phases}
\usage{
veg_summary(veg_df)
}
\arguments{
\item{veg_df}{a properly formatted vegetation dataframe}
}
\value{
a nested list of vegetation summaries by ecosite and state/phase
}
\description{
\code{veg_summary()} takes a \emph{properly formatted vegetation dataframe} and produces summaries
of vegetation for ecosites and states/phases. Vegetation data originates from the
NASIS vegplot table. The \link[ecositer]{formatted_veg_df} function should be used to manipulate raw NASIS
vegplot data into a \emph{properly formatted vegetation dataframe}
}
\details{
\code{veg_summary()} function also runs an Indicator Species Analysis (ISA) using the \link[indicspecies]{multipatt} function.
Documentation for that function should be read to understand ISA. Briefly, ISA determines whether a species is a strong
indicator for a community. This is determined by multiplying the percent of the abundance of the species that occurs in
the community of interest (summed abundance in community of interest/summed abundance in all plots) by the the percent
of the plots within the community of interest where the species is present (# plots in community of interest where species
present/total number of plots assigned to community of interest). A strong indicator has the majority of its abundance
in the community of interest and appears in the majority of plots associated with the community of interest. ISA is used to
determine indicator species for ecosites and for states/phases.

ISA interpretation notes: ISA determines the relationship between a community and every species that occured in that
community. It ultimately only reports a p-value for the species in the community it has the strongest relationship
with (lowest p-value.) Therefore, most ISA p-values are NA because the species is not most strongly
associated with that community. When a p-value is reported, that means the species is most strongly associated
with that community, even if the p-value is high. P-values are calculated using permutations of the data. ISA,
in the manner applied here, suffers dramatically from the fact that numerous statistical tests are being run.
When running multiple statistical tests, a modified critical p-value should be determined, using something like a
Bonferroni correction. For example, if 100 tests are run resulting in 100 different p-values, the critical p-value
would be adjusted to be 0.05/100 = .0005. In this situation, it is not uncommon for 1000s test, or more, to be
calculated. It is not reasonable to modify the critical p-value to that degree for several reasons. As a result,
ISA should be interpretted with care. It provides insight into the  relationship between species and sites/states/phases.
Ecologists should use expert knowledge along with the data provided in these summaries (ISA included) to determine what
species are most representative of an ecosite/state/phase.

This function takes several minutes due to the computation related to ISA.
}
\examples{

my_veg_summary <- veg_summary(veg_df = ecositer::vegetation_dataframe)
head(my_veg_summary)


}
