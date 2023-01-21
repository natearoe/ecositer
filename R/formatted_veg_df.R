#' Create formatted vegetation dataframe
#'
#' @param static_location the location where a static NASIS database has been
#' saved.
#'
#' @return a formatted vegetation dataframe suitable for analysis
#' @export
#'
#' @examples
#'
#' formatted_veg_df(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/vegplotdata2.sqlite")
#'
#'
formatted_veg_df <- function(static_location){

  ############# Create foundational dataframes

  # Access veg data
  veg_data <- fetchVegdata(dsn = static_location, SS = FALSE)

  # Access ecosite data
  ecosite_data <- fetchNASIS(dsn = static_location,
                             from = "pedons",
                             SS = FALSE, fill = TRUE, duplicates = TRUE)

  ############ Choosing the best vegplot


  # What sites have multiple veg plots?
  siteiid_with_dup_vegplots <- veg_data$vegplot %>% dplyr::select(siteiid, vegplotiid, vegplot_id) %>%
    unique() %>%  dplyr::group_by(siteiid) %>%
    dplyr::filter(n() > 1) %>% dplyr::pull(siteiid)




  # Choose the least populated vegplots from plots with multiple vegplots; these are the vegplots to remove from veg data
  dup_vegplots <- veg_data$vegplotspecies %>% dplyr::filter(siteiid %in% siteiid_with_dup_vegplots) %>%
    group_by(siteiid, vegplotiid) %>% summarise(n = n()) %>%
    dplyr::arrange(siteiid, desc(n)) %>% dplyr::group_by(siteiid) %>%
    dplyr::filter(row_number() != 1) %>%  dplyr::pull(vegplotiid)

  # Remove dup_vegplots from veg_data$vegplotsspecies
  veg_data_species_reduced <- veg_data$vegplotspecies %>% dplyr::filter(!vegplotiid %in% dup_vegplots)

  # Remove dup_vegplots from veg_data$vegplot
  veg_data_veg_plot_reduced <- veg_data$vegplot %>% dplyr::filter(!is.na(ecositeid)) %>% dplyr::select(siteiid, ecositeid, vegplotiid, akfieldecositeid) %>%
    unique() %>% dplyr::filter(!vegplotiid %in% dup_vegplots & vegplotiid %in% veg_data$vegplotspecies$vegplotiid)



  ############ Adding peiid to veg_data_species_reduced
  veg_data_species_peiid <- dplyr::left_join(veg_data_species_reduced,
                                             veg_data_veg_plot_reduced %>%
                                               dplyr::select(vegplotiid, peiid))


  ########### Joining ecositeid to veg_data_species_peiid
  veg_data_with_ecosite <-
    dplyr::left_join(
      veg_data_species_reduced %>%
        dplyr::select(
          siteiid,
          vegplotid,
          vegplotiid,
          plantsym,
          primarydatacollector,
          plantsciname,
          plantnatvernm,
          akstratumcoverclass,
          akstratumcoverclasspct
        ),
      aqp::site(ecosite_data) %>%
        dplyr::select(siteiid, ecositeid) %>% unique()
    ) %>%
    dplyr::left_join(veg_data_veg_plot_reduced %>% dplyr::select(siteiid, vegplotiid, akfieldecositeid)) %>%
    dplyr::select(siteiid, vegplotid, ecositeid, everything())

  return(veg_data_with_ecosite)

}
