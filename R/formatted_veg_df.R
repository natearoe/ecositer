#' Create formatted vegetation dataframe
#'
#' @param static_location the location where a static NASIS database has been
#' saved.
#' @param SS TRUE or FALSE, should data be accessed from NASIS Selected Set?
#' @param veg_column NASIS column where vegetation cover data is stored
#'
#' @return a formatted vegetation dataframe suitable for analysis
#' @export formatted_veg_df
#'
#' @examples
#'
#' my_formatted_veg_df <- formatted_veg_df(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
#' head(formatted_veg_df)
#'
formatted_veg_df <- function(SS = TRUE, static_location = NULL,
                             cover_column = c("akstratumcoverclasspct",
                                            "speciescancovpct",
                                            "speciescomppct",
                                            "understorygrcovpct")){

  if(SS == FALSE & is.null(static_location))
    stop('If SS = FALSE, static location must be provided.')

  cover_column <- match.arg(cover_column)

  ############# Create foundational dataframes

  # Access veg data
  veg_data <- if(SS == TRUE){
    soilDB::fetchVegdata(SS = TRUE)
  } else {
    soilDB::fetchVegdata(dsn = static_location, SS = FALSE)}

  # Access ecosite data
  ecosite_data <- soilDB::fetchNASIS(dsn = static_location,
                             from = "pedons",
                             SS = FALSE, fill = TRUE, duplicates = TRUE)


  ############ Choosing the best vegplot

  # What sites have multiple veg plots?
  siteiid_with_dup_vegplots <- veg_data$vegplot  |>  dplyr::select(siteiid, vegplotiid, vegplot_id, peiid) |>
    unique() |>  dplyr::group_by(siteiid) |>
    dplyr::filter(dplyr::n() > 1) |> dplyr::pull(siteiid)




  # Choose the least populated vegplots from plots with multiple vegplots; these are the vegplots to remove from veg data
  dup_vegplots <- veg_data$vegplotspecies |> dplyr::filter(siteiid %in% siteiid_with_dup_vegplots) |>
    dplyr::group_by(siteiid, vegplotiid) |> dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(siteiid, desc(n)) |> dplyr::group_by(siteiid) |>
    dplyr::filter(dplyr::row_number() != 1) |>  dplyr::pull(vegplotiid)

  # Remove dup_vegplots from veg_data$vegplotsspecies
  veg_data_species_reduced <- veg_data$vegplotspecies |> dplyr::filter(!vegplotiid %in% dup_vegplots)

  # Remove dup_vegplots from veg_data$vegplot
  veg_data_veg_plot_reduced <- veg_data$vegplot |>
    dplyr::filter(!is.na(ecositeid)) |>
    dplyr::select(siteiid, ecositeid, vegplotiid, akfieldecositeid) |>
    unique() |>
    dplyr::filter(!vegplotiid %in% dup_vegplots & vegplotiid %in% veg_data$vegplotspecies$vegplotiid)


  ########### Joining ecositeid to veg_data_species_reduced
  veg_data_with_ecosite <-
    dplyr::left_join(
      veg_data_species_reduced |>
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
      aqp::site(ecosite_data) |>
        dplyr::select(siteiid, ecositeid) |> unique()
    ) |>
    dplyr::left_join(veg_data_veg_plot_reduced |> dplyr::select(siteiid, vegplotiid, akfieldecositeid)) |>
    dplyr::select(siteiid, vegplotid, ecositeid, everything())


  # Remove a question mark from a couple of instances of akfieldecositeid
  veg_data_with_ecosite$akfieldecositeid <-
    veg_data_with_ecosite$akfieldecositeid |>
    stringr::str_replace(pattern = "\\.\\?", replacement = "")

  # Identify the akfieldecositeids missing a phase
  no_phase <-
    veg_data_with_ecosite$akfieldecositeid[!veg_data_with_ecosite$akfieldecositeid %in%
                                         stringr::str_subset(veg_data_with_ecosite$akfieldecositeid,
                                                             "[:digit:]\\.[:digit:]\\.[:digit:]") &
                                         !is.na(veg_data_with_ecosite$akfieldecositeid)]

  # Add default phase to those missing - default being .1
  veg_data_with_ecosite$akfieldecositeid_edit <-
    ifelse(
      veg_data_with_ecosite$akfieldecositeid %in%
        no_phase,
      paste0(veg_data_with_ecosite$akfieldecositeid, ".1"),
      veg_data_with_ecosite$akfieldecositeid
    )

  # Still missing a phase?
  no_phase <-
    veg_data_with_ecosite$akfieldecositeid_edit[!veg_data_with_ecosite$akfieldecositeid_edit %in%
                                             stringr::str_subset(veg_data_with_ecosite$akfieldecositeid_edit,
                                                                 "[:digit:]\\.[:digit:]\\.[:digit:]") &
                                             !is.na(veg_data_with_ecosite$akfieldecositeid_edit)]


  # Add default phase to those missing - default being .1
  veg_data_with_ecosite$akfieldecositeid_edit <-
    ifelse(
      veg_data_with_ecosite$akfieldecositeid_edit %in%
        no_phase,
      paste0(veg_data_with_ecosite$akfieldecositeid_edit, ".1"),
      veg_data_with_ecosite$akfieldecositeid_edit
    )

  veg_data_with_ecosite <- veg_data_with_ecosite |> dplyr::select(-akfieldecositeid) |>
    dplyr::rename(akfieldecositeid = akfieldecositeid_edit)

  # Append coordinates
  veg_data_with_ecosite_coords <-
    dplyr::left_join(veg_data_with_ecosite,
                     veg_data$vegplotlocation |>
                       dplyr::select(siteiid, horizdatnm, utmzone, utmeasting, utmnorthing) |>
                       unique())

  return(veg_data_with_ecosite_coords)

}
