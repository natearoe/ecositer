
#' QC plants
#'
#' `QC_unify_data` returns a dataframe of taxa that could benefit from unifying. Any identification beyond species
#' (i.e., subspecies, variety, etc.) will be returned, as will identifications to genus level. Statistical analyses do
#' not recognize any similarity between Calamagrostis stricta and Calamagrostis stricta ssp. stricta, so if it is reasonable
#' to unify these to the same class (e.g., change all to Calamagrostis stricta), that is recommendable.
#'
#' @param veg_df
#'
#' @return a dataframe of taxa recommended for unifying
#' @export
#'
#' @examples
#' QC_plants(veg_df = ecositer::vegetation_dataframe) |> head()
QC_unify_taxa <- function(veg_df){

  # plant only appears once per plot, then split genus, species, subspecies
  plant_sci_split <- veg_df |> dplyr::select(vegplotid, plantsciname) |> unique() |>
    dplyr::filter(!is.na(plantsciname) & plantsciname != "unknown scientific name") |>
    dplyr::pull(plantsciname) |> unique() |> stringr::str_split(pattern = "[:blank:]")

  # determine all genera
  genera <- lapply(plant_sci_split, FUN = function(x){x[1]}) |> unlist() |> unique()

  # determine genera identified only to genera
  id_to_genera <- veg_df |> dplyr::filter(plantsciname %in% genera) |>
    dplyr::pull(plantsciname) |> unique()

  # determine species identified to subspecies
  my_ssp <- lapply(plant_sci_split, FUN = function(x){x[4]}) |> unlist() |> unique() |>
    na.omit()

  spp_to_ssp_list <- stringr::str_subset(veg_df$plantsciname, pattern = paste(my_ssp, collapse = "|")) |>
    stringr::str_split(pattern = "[:blank:]")

  spp_to_ssp <- lapply(spp_to_ssp_list, FUN = function(x){x[1]}) |> unlist() |> unique()

  # return genera IDed to genera and subspecies
  plant_to_examine <- veg_df |> dplyr::filter(plantsciname %in%
                                                    stringr::str_subset(veg_df$plantsciname,
                                                                        pattern = paste(c(id_to_genera, spp_to_ssp) |>
                                                                                          unique(),
                                                                                        collapse = "|"))) |>
    dplyr::arrange(plantsciname) |> dplyr::pull(plantsciname) |> table() |> as.data.frame()

  return(plant_to_examine)

}
