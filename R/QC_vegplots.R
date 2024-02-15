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
#'based on siteiids and parameters related to vegplot completeness.
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
    dplyr::group_by(siteiid) |>
    dplyr::summarise(species_richness = dplyr::n(),
                     perc_species_with_abund = sum(!is.na(akstratumcoverclasspct)) * 100/dplyr::n(),
                     perc_species_level_id = length(stringr::str_subset(plantsciname, pattern = "\\s"))*100/dplyr::n()) |>
    as.data.frame() |>
    dplyr::left_join(veg_df |> dplyr::select(siteiid, vegplotid) |>
                       unique(), by = dplyr::join_by(siteiid)) |>
    dplyr::select(siteiid, vegplotid, everything()) |>
    dplyr::arrange(species_richness)

}


#' @param veg_df veg_df created using `ecositer::formatted_veg_df()`
#' @param my_QC_vegplots_summary results of `ecositer::my_QC_vegplots_summary()`
#' @param min_species site must have >= N species to be included
#' @param min_abund sites must have >= N% species with abundance data to be included (e.g.,
#' entering 90 would include all sites with 90% or more species with abundance data).
#' WARNING: If you keep sites that are missing abundance data and intend to run vegetation
#' analyses using abundance data, abundance values will have to be populated. If you run
#' analyses on presence/absence data, abundance data is not needed.
#' @param siteiids siteiids to be removed as defined by user
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
                                     min_perc_species_with_abund = NULL,
                                     min_perc_species_level_id = NULL,
                                     siteiids = NULL){


  if(is.null(min_perc_species_level_id)){
    min_perc_species_level_id = 0}
  if(!round(min_perc_species_level_id) >= 0 | !min_perc_species_level_id%%1==0)
    stop('min_perc_species_level_id must be a positive, whole number', call. = FALSE)

  if(is.null(min_species)){
    min_species = 0}
  if(!round(min_species) >= 0 | !min_species%%1==0)
    stop('min_species must be a positive, whole number', call. = FALSE)

  if(is.null(min_perc_species_with_abund)){
    min_perc_species_with_abund = 0}
  if(min_perc_species_with_abund < 0 | min_perc_species_with_abund > 100)
    stop('min_perc_species_with_abund must range from 0-100.', call. = FALSE)


  if(is.null(veg_df))
    stop('veg_df argument must be provided.', call. = FALSE)

  if(is.null(my_QC_vegplots_summary))
    stop('my_QC_vegplots_summary argument must be provided.', call. = FALSE)

  if(is.null(siteiids)){
    siteiids = c("")
  }

  sites_with_min_requirement <- my_QC_vegplots_summary |>
    dplyr::filter(species_richness >= min_species &
                    perc_species_with_abund >= min_perc_species_with_abund &
                    perc_species_level_id >= min_perc_species_level_id) |>
    dplyr::pull(siteiid)

  veg_df |> dplyr::filter(siteiid %in% c(sites_with_min_requirement))


}
