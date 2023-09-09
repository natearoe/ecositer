#' Number of plots by ecosite
#'
#' `number_plots_by_ecosite` describes the number of pedons and vegplots that
#' have been recorded for each ecosite. It also lists the states/phases
#'associated with each ecosite.
#'
#'
#' @param static_location the location where a static NASIS database has been
#' saved.
#'
#' @return a dataframe describing the number of pedons and vegplots recorded
#' for each ecosite
#' @export
#'
#' @examples
#' example_data <- access_example_data()
#' my_plots_by_site <- number_plots_by_site(static_location = example_data)
#' # specify user-defined location -
#' # my_plots_by_site <- number_plots_by_site(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
#' head(my_plots_by_site)
#'

number_plots_by_site <- function(static_location){

  # Access veg data
  veg_data <- soilDB::fetchVegdata(dsn = static_location, SS = FALSE)

  # Access fetchNASIS data
  ecosite_data <- soilDB::fetchNASIS(dsn = static_location,
                             from = "pedons",
                             SS = FALSE, fill = TRUE, duplicates = TRUE)


  # Vegdata fecosite
  veg_data_fecosite <-
    veg_data$vegplot |>  dplyr::select(site_id, akfieldecositeid) |>
    dplyr::mutate(ecosite_simple = stringr::str_sub(.$akfieldecositeid, start = 1L, end = 4L))

  # fetchNASIS ecositeid
  ecosite_id <- aqp::site(ecosite_data) |>  dplyr::select(site_id, ecositeid)


  # Determine number of pedons associated with each ecosite
  numb_pedons_by_ecosite <-
    ecosite_id |>  dplyr::count(ecositeid) |>  dplyr::arrange(desc(n)) |>  dplyr::rename(pedons = n) |>
    dplyr::mutate(ecosite_simple = stringr::str_sub(ecositeid, start = -6L, end = -3L))

  # Determine the number of vegplots associated with each ecosite
  numb_vegplots_by_ecosite <- veg_data_fecosite |>
    dplyr::mutate(ecosite_simple = stringr::str_sub(
      veg_data_fecosite$akfieldecositeid,
      start = 1L,
      end = 4L
    )) |>
    dplyr::count(ecosite_simple) |>  dplyr::arrange(desc(n)) |>  dplyr::rename(vegplots = n)



  # Remove a question mark from a couple of instances of akfieldecositeid
  veg_data_fecosite$akfieldecositeid <-
    veg_data_fecosite$akfieldecositeid |>
    stringr::str_replace(pattern = "\\.\\?", replacement = "")

  # Identify the akfieldecositeids missing a phase
  no_phase <-
    veg_data_fecosite$akfieldecositeid[!veg_data_fecosite$akfieldecositeid %in%
                                         stringr::str_subset(veg_data_fecosite$akfieldecositeid,
                                                             "[:digit:]\\.[:digit:]\\.[:digit:]") &
                                         !is.na(veg_data_fecosite$akfieldecositeid)]

  # Add default phase to those missing - default being .1
  veg_data_fecosite$akfieldecositeid_edit <-
    ifelse(
      veg_data_fecosite$akfieldecositeid %in%
        no_phase,
      paste0(veg_data_fecosite$akfieldecositeid, ".1"),
      veg_data_fecosite$akfieldecositeid
    )

  # Summarise how many state/phases each ecosite has and list them
  numb_statephases <-
    veg_data_fecosite |>  dplyr::group_by(ecosite_simple) |>
    dplyr::summarise(
      numb_statephases = n_distinct(akfieldecositeid_edit),
      statephases = paste(unique(akfieldecositeid_edit), collapse = ", ")
    )


  # Df showing number of pedons, vegplots, and states/phases for each ecosite
  numb_pedons_and_vegplots <-
    dplyr::full_join(numb_pedons_by_ecosite, numb_vegplots_by_ecosite) |>
    dplyr::select(ecositeid, ecosite_simple, everything()) |>  dplyr::full_join(numb_statephases)



  return(numb_pedons_and_vegplots)

}
