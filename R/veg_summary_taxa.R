#' Vegetation summary for particular taxa
#'
#' @param veg_df properly formatted vegetation data frame
#' @param taxa taxa of interest, typically genus level (e.g., "Arabis"). This is used as a regular expression pattern.
#'
#' @return a vegetation summary for the taxa of interest
#' @export
#'
#' @examples ecositer::veg_summary_taxa(veg_df = my_veg_df, taxa = "Arabis")
veg_summary_taxa <- function(veg_df, taxa){

  taxa_unique <- veg_df$plantsciname |> stringr::str_subset(pattern = taxa) |> unique()
  taxa_siteiid <- veg_df |> dplyr::filter(plantsciname %in% taxa_unique)
  taxa_siteiid$siteiid_taxa_id <- paste0(taxa_siteiid$siteiid, taxa_siteiid$plantsym)
  taxa_siteiid$akfieldecositeid <- taxa_siteiid$plantsciname
  veg_df_taxa <- veg_df |> dplyr::filter(siteiid %in% taxa_siteiid$siteiid) |>
    dplyr::select(-ecositeid, - akfieldecositeid) |>
    dplyr::mutate(siteiid_dup = siteiid)
  veg_df_taxa$ecositeid <- taxa
  veg_df_joined <- veg_df_taxa |> dplyr::left_join(taxa_siteiid |> dplyr::select(siteiid, siteiid_taxa_id, akfieldecositeid),
                                          by = "siteiid",
                                          relationship = "many-to-many")

  ecositer::veg_summary(veg_df = veg_df_joined)

}
