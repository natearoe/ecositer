
#' QC plants
#'
#' `QC_plants()` takes a *properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df})*
#' and provides a dataframe of plants that are recommended for QC, including plant names and the number
#' of times that each plant name was used. All species within genera identified to the genera or subspecies level
#' are recommended for QC.
#'
#' @param veg_df
#'
#' @return a dataframe of plants recommended for
#' @export
#'
#' @examples
#' QC_plants(veg_df = ecositer::vegetation_dataframe) |> head()
QC_plants <- function(veg_df){

  # split genus, species, subspecies
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
