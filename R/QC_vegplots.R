#' QC vegplots
#'
#'
#'@description
#'`QC_vegplots_summary()` produces a summary regarding the completeness of
#'vegplots, including the species richness in each plot and the percent of
#'species with abundance data. This information can be used to remove vegplots
#'from analyses due to lack of completeness.
#'
#'`QC_vegplots_remove_plots()` allows you to remove vegplots from your dataset
#'based on site_ids and parameters related to vegplot completeness.
#'
#' @param veg_df
#'
#' @return `QC_vegplots_summary()` returns a summary of vegetation plot sampling detail
#' @export
#'
#' @examples
#' my_QC_vegplots <- QC_vegplots(veg_df = my_veg_df)
QC_vegplots_summary <- function(veg_df){

  veg_df |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(species_richness = dplyr::n(),
                     perc_species_with_abund = sum(!is.na(akstratumcoverclasspct)) * 100/dplyr::n()) |>
    as.data.frame() |> dplyr::arrange(species_richness)

}


#' @param veg_df veg_df created using `ecositer::formatted_veg_df()`
#' @param my_QC_vegplots_summary results of `ecositer::my_QC_vegplots_summary()`
#' @param min_species sites with < N species are removed due to non-comprehensive sampling
#' @param min_abund sites with < N% species with abundance data are removed (e.g.,
#' entering 90 would remove sites with less than 90% of species including abundance data.)
#' @param site_id site_ids to be removed as defined by user
#'
#' @return `QC_vegplots_remove_plots()` returns a veg_df satisfying the defined
#' requirements of completeness
#' @export
#'
#' @examples
#' # example
#' @rdname QC_vegplots_summary
QC_vegplots_remove_plots <- function(veg_df = NULL,
                                     my_QC_vegplots_summary = NULL,
                                     min_species = NULL,
                                     min_perc_abund = NULL,
                                     site_ids = NULL){


  if(is.null(min_species)){
    min_species = 0}
  if(!round(min_species) >= 0 | !min_species%%1==0)
    stop('min_species must be a positive, whole number', call. = FALSE)

  if(is.null(max_missing_abund)){
    max_missing_abund = 100}
  if(max_missing_abund < 0 | max_missing_abund > 100)
    stop('max_missing_abund must range from 0-100.', call. = FALSE)


  if(is.null(veg_df))
    stop('veg_df argument must be provided.', call. = FALSE)

  if(is.null(my_QC_vegplots_summary))
    stop('my_QC_vegplots_summary argument must be provided.', call. = FALSE)

  if(is.null(site_id)){
    site_id = c("")
  }

  sites_with_min_requirement <- my_QC_vegplots_summary |>
    dplyr::filter(species_richness > min_species &
                    perc_species_with_abund > min_perc_abund) |>
    dplyr::pull(site_id)

  veg_df |> dplyr::filter(site_id %in% c(sites_with_min_requirement)) |>
    dplyr::filter(!site_id %in% site_ids)


}