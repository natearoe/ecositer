#' @title Summarize vegetation plot completeness
#' @description
#' This function calculates summary statistics for vegetation plots grouped by `vegplotiid`, specifically:
#' total_records - the total number of records associated with each `vegplotiid`
#' unique_species - The number of unique species recorded for each `vegplotiid`.
#' percent_to_species - the percentage of observations identified to the species level (determined by the presence of a space in `plantsciname`).
#' percent_with_abund - the percentage of observations with a non-NA value in the `pct_cover` column.
#'
#' @param veg_df vegetation dataframe
#'
#' @return a summary of vegetation plot completeness
#' @export
#' @seealso [QC_completeness_criteria()]
#' @examples
#' my_QC_vegplots <- QC_veg_completeness(veg_df = my_veg_df)
QC_veg_completeness <- function(veg_df){

    # Ensure dt is a data.table
    dt <- data.table::as.data.table(veg_df)

    # Group by vegplotiid and calculate metrics
    result <- dt[, .(
      total_records = .N,  # Total number of records for each vegplotiid
      unique_species = data.table::uniqueN(plantsym),  # Number of unique species
      percent_to_species = sum(grepl(" ", plantsciname)) / .N * 100,  # Percent identified to species
      percent_with_abund = sum(!is.na(pct_cover)) / .N * 100  # Percent with non-NA pct_cover
    ), by = c("siteiid", "siteobsiid", "vegplotiid", "primarydatacollector", "vegdataorigin")]

    return(result |> as.data.frame())


  # veg_df |>
  #   dplyr::group_by(siteiid) |>
  #   dplyr::summarise(species_richness = dplyr::n(),
  #                    perc_species_with_abund = sum(!is.na(akstratumcoverclasspct)) * 100/dplyr::n(),
  #                    perc_species_level_id = length(stringr::str_subset(plantsciname, pattern = "\\s"))*100/dplyr::n()) |>
  #   as.data.frame() |>
  #   dplyr::left_join(veg_df |> dplyr::select(siteiid, vegplotid) |>
  #                      unique(), by = dplyr::join_by(siteiid)) |>
  #   dplyr::select(siteiid, vegplotid, everything()) |>
  #   dplyr::arrange(species_richness)
  #
  #
  # veg_df |>

}

#' @title Remove vegetation plots based on completeness criteria
#' @param veg_df vegetation dataframe
#' @param min_species (%) - sites must have >= min_species to be included
#' @param min_abund integer - sites must have >= min_abund species with abundance data to be included (e.g.,
#' entering 90 would include all sites with 90% or more species with abundance data).
#' WARNING: If you keep sites that are missing abundance data and intend to run vegetation
#' analyses using abundance data, abundance values will have to be populated, otherwise that species record will be removed. If you run
#' analyses on presence/absence data, abundance data is not needed.
#'
#' @return vegetation dataframe meeting completeness criteria
#' @export
#' @seealso [QC_veg_completeness()]
#'
#' @examples
#' QC_completeness_criteria(veg_df = my_veg_df, min_unique_species = 5, min_perc_to_species = 70, min_perc_with_abund = 70)
QC_completeness_criteria <- function(veg_df = NULL,
                                     min_total_records = NULL,
                                     min_unique_species = NULL,
                                     min_perc_to_species = NULL,
                                     min_perc_with_abund = NULL){


  if(is.null(min_perc_to_species)){
    min_perc_to_species = 0}
  if(!round(min_perc_to_species) >= 0 | !min_perc_to_species%%1==0)
    stop('min_perc_to_species must be a positive, whole number', call. = FALSE)

  if(is.null(min_unique_species)){
    min_unique_species = 0}
  if(!round(min_unique_species) >= 0 | !min_unique_species%%1==0)
    stop('min_species must be a positive, whole number', call. = FALSE)

  if(is.null(min_total_records)){
    min_total_records = 0}
  if(!round(min_total_records) >= 0 | !min_total_records%%1==0)
    stop('min_species must be a positive, whole number', call. = FALSE)

  if(is.null(min_perc_with_abund)){
    min_perc_with_abund = 0}
  if(min_perc_with_abund < 0 | min_perc_with_abund > 100)
    stop('min_perc_with_abund must range from 0-100.', call. = FALSE)


  if(is.null(veg_df))
    stop('veg_df argument must be provided.', call. = FALSE)


  veg_completeness_df <- ecositer::QC_veg_completeness(veg_df)


  sites_with_min_criteria <- veg_completeness_df |>
    dplyr::filter(total_records >= min_total_records &
                    unique_species >= min_unique_species &
                    percent_to_species >= min_perc_to_species &
                    percent_with_abund >= min_perc_with_abund) |>
    dplyr::pull(siteiid)

  veg_df |> dplyr::filter(siteiid %in% sites_with_min_criteria)


}
