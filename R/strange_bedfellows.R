#' Strange bedfellows
#'
#' `strange_bedfellows` can be used to identify plots with unusual plant communities.
#' This could indicate incorrect identifications or unique assemblages
#'
#' @param site_sim the results of (\link[ecositer]{site_similarity_pairwise})
#'
#' @return plot names and percentile values. The percentile values can be used to
#' identify plots that have unusual plant communities. Lower percentile values
#' have more unusual assemblages and are worth QCing to determine if species
#' identifications are correct.
#'
#' @export
#'
#' @examples
#' ecositer::strange_bedfellows(site_sim = my_site_sim)
strange_bedfellows <- function(site_sim){
  site_sim$bbd_ptile <- 100 - round(ecdf(site_sim$bbd)(site_sim$bbd)*100, digits = 6)
  site_sim[site_sim$plot1 == site_sim$plot2, c(1,4)]
}
