#' Create formatted vegetation dataframe
#'
#' @param from origin of vegetation data. "web_report" provides public data in NASIS. "SS" uses the users NASIS selected set (NASIS connection required).
#' "static" uses a local .sqlite NASIS database.
#' @param ecositeid ecositeid of interest - required when using from = "web_report". If used with from = "SS" or from = "static", limits returns to only ecositeid provided.
#' @param static_location file path to static NASIS .sqlite database - typically generated using `ecositer::vegStaticNASIS()`
#'
#' @description
#' This function provides access to vegetation data from multiple sources and returns the data in the same format. No QC is performed in this function.
#'
#'
#' @return vegetation dataframe fro ecological site data
#' @export create_veg_df
#'
#' @examples
#'
#' my_formatted_veg_df <- formatted_veg_df(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
#' head(formatted_veg_df)
#'
create_veg_df <- function(from = c("web_report", "SS", "static"),
                             ecositeid = NULL,
                             static_location = NULL,
                             best_vegplot = TRUE){

  ## Error handling

  # ensure from argument matches one of the options
  from <- match.arg(from)

  # if web_report used, ecositeid must be provided
  if(from == "web_report" && is.null(ecositeid)){
    stop("ecositeid must be provided if using web report.")
  }

  # if static used, static_location must be provided
  if(from == "static" && is.null(static_location)){
    stop("static_location must be provided if using static.")
  }

  # QC on static_location
  if(from == "static"){

    check_sqlite_path <- function(static_location, required_table = "site") {
      # Check if the file exists
      if (!file.exists(static_location)) {
        stop("Error: The specified file does not exist.")
      }

      # check if file exists but is not SQLite database
      conn <- tryCatch({
        DBI::dbConnect(RSQLite::SQLite(), dbname = static_location)
      }, warning = function(w) {
        stop("Error: The file exists but is not a valid SQLite database.")
      }, error = function(e) {
        stop("Error: The file exists but is not a valid SQLite database.")
      })

      # check if the database is valid by listing tables
      tables <- tryCatch({
        DBI::dbListTables(conn)
      }, warning = function(w) {
        DBI::dbDisconnect(conn)
        stop("Error in static_location: The file exists but is not a valid SQLite database.")
      }, error = function(e) {
        DBI::dbDisconnect(conn)
        stop("Error in static_location: The file exists but is not a valid SQLite database.")
      })

      # check for the required table
      if (!required_table %in% tables) {
        DBI::dbDisconnect(conn) # Clean up connection
        stop(sprintf("Error in static_location: The database does not contain the required table '%s'.", required_table))
      }

      # disconnect from the database if all checks pass
      DBI::dbDisconnect(conn)

    }

    check_sqlite_path(static_location = static_location)
  }

  # QC on SS
  if(from == "SS" && soilDB::local_NASIS_defined() == FALSE){
    stop("Could not find local NASIS odbc data source. Visit this tutorial - https://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html")
  }

  # # QC on SS
  # if(from = "SS"){
  #
  # }
  #
  # check_nasis_selected_set <- function(required_table = "site") {
  #   # Try connecting to the NASIS selected set
  #   conn <- tryCatch({
  #     soilDB::NASIS()
  #   }, warning = function(w) {
  #     stop("Error in SS: Could not connect to the NASIS database.")
  #   }, error = function(e) {
  #     stop("Error in SS: Could not connect to the NASIS database.")
  #   })
  #
  #   # Check if the connection is valid by attempting to read the required table
  #   selected_data <- tryCatch({
  #     if (required_table == "site") {
  #       soilDB::get_site_data_from_NASIS()
  #     } else if (required_table == "pedons") {
  #       soilDB::get_pedons_from_NASIS()
  #     } else {
  #       stop(sprintf("Error: Table '%s' is not recognized or unsupported.", required_table))
  #     }
  #   }, warning = function(w) {
  #     stop(sprintf("Error: The required table '%s' could not be retrieved.", required_table))
  #   }, error = function(e) {
  #     stop(sprintf("Error: The required table '%s' could not be retrieved.", required_table))
  #   })
  #
  #   # Check if the required data exists in the selected set
  #   if (nrow(selected_data) == 0) {
  #     stop(sprintf("Error: The selected set does not contain any data for the table '%s'.", required_table))
  #   }
  #
  #   # If all checks pass, return TRUE or silently complete
  #   invisible(TRUE)
  # }

  ## Create dateframes

  # follow similar methodology for "SS" and "static"
  if(from %in% c("SS", "static")){

    # access data from SS
    if(from == "SS") {
      veg_data <- soilDB::fetchVegdata(SS = TRUE, include_pedon = FALSE)
      # ecosite_data <- soilDB::fetchNASIS(
      #   dsn = NULL,
      #   from = "pedons",
      #   SS = TRUE,
      #   fill = TRUE,
      #   duplicates = TRUE
      # )
    }

    # access data from static
    if(from == "static") {
      veg_data <- soilDB::fetchVegdata(dsn = static_location, SS = FALSE, include_pedon = FALSE)
      # ecosite_data <- soilDB::fetchNASIS(
      #   dsn = static_location,
      #   from = "pedons",
      #   SS = FALSE,
      #   fill = TRUE,
      #   duplicates = TRUE
      # )
    }

    # filter to ecosite argument, if provided
    if(!is.null(ecositeid)){

      # add siteiid to vegplottext
      veg_data[["vegplottext"]] <- veg_data[["vegplottext"]] |> dplyr::left_join(veg_data$vegplot |> dplyr::select(siteiid, vegplotiid),
                                                                                 by = dplyr::join_by(vegplotiid))
      es_siteiids <- veg_data$vegplot |> dplyr::filter(ecositeid == {{ecositeid}}) |> dplyr::pull(siteiid)
      veg_data <- lapply(veg_data, FUN = function(x){
        x |> dplyr::filter(siteiid %in% es_siteiids)
      })

     }


    # assemble veg df
    vegplot <- veg_data$vegplot |> dplyr::select(
      siteiid,
      site_id,
      siteobsiid,
      vegplot_id,
      vegplotiid,
      primarydatacollector,
      vegdataorigin,
      ecositeid
    ) |>
      dplyr::left_join(
        veg_data$vegplotspecies |> dplyr::select(
          siteiid,
          siteobsiid,
          vegplotiid,
          plantsym,
          plantsciname,
          plantnatvernm,
          akstratumcoverclasspct,
          speciescancovpct,
          speciescomppct,
          understorygrcovpct
        ),
        dplyr::join_by(siteiid, vegplotiid, siteobsiid)#,relationship = "many-to-many"
      ) |>
      dplyr::left_join(
        veg_data$vegplotlocation |> dplyr::select(
          siteiid,
          siteobsiid,
          vegplotiid,
          horizdatnm,
          utmzone,
          utmeasting,
          utmnorthing
        ),
        dplyr::join_by(siteiid, vegplotiid, siteobsiid)
      ) |>
      dplyr::left_join(
        veg_data$site |> dplyr::select(siteiid,
                                       siteobsiid,
                                       ecositenm,
                                       ecostateid,
                                       ecostatename,
                                       commphaseid,
                                       commphasename),
        by = dplyr::join_by(siteiid, siteobsiid)#,relationship = "many-to-many"
      )

    vegplot <- vegplot |> dplyr::rename(usiteid = site_id, vegplotid = vegplot_id) |> as.data.frame()

  }

  if(from == "web_report") {
    url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-PlotVegDataByEcosite_ecositer_format&es1=", ecositeid)
    page <- xml2::read_html(url)
    web_data <- page |> rvest::html_node("table") |>
      rvest::html_table(header = TRUE)
    vegplot <- web_data

  }

  vegplot$siteiid <- as.character(vegplot$siteiid)
  vegplot$usiteid <- as.character(vegplot$usiteid)
  vegplot$siteobsiid <- as.character(vegplot$siteobsiid)
  vegplot$vegplotid <- as.character(vegplot$vegplotid)
  vegplot$vegplotiid <- as.character(vegplot$vegplotiid)
  vegplot$primarydatacollector <- as.character(vegplot$primarydatacollector)
  vegplot$vegdataorigin <- as.character(vegplot$vegdataorigin)
  vegplot$ecositeid <- as.character(vegplot$ecositeid)
  vegplot$ecositenm <- as.character(vegplot$ecositenm)
  vegplot$ecostateid <- as.character(vegplot$ecostateid)
  vegplot$ecostatename <- as.character(vegplot$ecostatename)
  vegplot$commphaseid <- as.character(vegplot$commphaseid)
  vegplot$commphasename <- as.character(vegplot$commphasename)
  vegplot$plantsym <- as.character(vegplot$plantsym)
  vegplot$plantsciname <- as.character(vegplot$plantsciname)
  vegplot$plantnatvernm <- as.character(vegplot$plantnatvernm)
  vegplot$akstratumcoverclasspct <- as.numeric(vegplot$akstratumcoverclasspct)
  vegplot$speciescancovpct <- as.numeric(vegplot$speciescancovpct)
  vegplot$speciescomppct <- as.numeric(vegplot$speciescomppct)
  vegplot$understorygrcovpct <- as.numeric(vegplot$understorygrcovpct)
  vegplot$horizdatnm <- as.character(vegplot$horizdatnm)
  vegplot$utmzone <- as.integer(vegplot$utmzone)
  vegplot$utmeasting <- as.numeric(vegplot$utmeasting)
  vegplot$utmnorthing <- as.numeric(vegplot$utmnorthing)

  vegplot <- vegplot |> dplyr::arrange(usiteid, vegplotiid, plantsym) |>
    dplyr::select(siteiid, usiteid, siteobsiid, vegplotid, vegplotiid, # siteecositehistoryiid,
                  primarydatacollector, vegdataorigin, ecositeid, ecositenm, ecostateid, ecostatename, commphaseid, commphasename, plantsym,
                  plantsciname, plantnatvernm, akstratumcoverclasspct, speciescancovpct, speciescomppct, understorygrcovpct,
                  horizdatnm, utmzone, utmeasting, utmnorthing) |> dplyr::arrange(usiteid) |>
    as.data.frame()



  # # Access ecosite data
  #
  # test <- aqp::site(ecosite_data)


  ############ Choosing the best vegplot

  # # What sites have multiple veg plots?
  # siteiid_with_dup_vegplots <- veg_data$vegplot |>  dplyr::select(siteiid, vegplotiid, vegplot_id, peiid) |>
  #   unique() |>  dplyr::group_by(siteiid) |>
  #   dplyr::filter(dplyr::n() > 1) |> dplyr::pull(siteiid)
  #
  #
  # siteiid_with_dup_vegplots <- veg_data$vegplot |>  dplyr::select(siteiid, vegplotiid) |>
  #   unique() |>  dplyr::group_by(siteiid) |>
  #   dplyr::filter(dplyr::n() > 1) |> dplyr::pull(siteiid)
  #
  # # Choose the least populated vegplots from plots with multiple vegplots; these are the vegplots to remove from veg data
  # dup_vegplots <- veg_data$vegplotspecies |> dplyr::filter(siteiid %in% siteiid_with_dup_vegplots) |>
  #   dplyr::group_by(siteiid, vegplotiid) |> dplyr::summarise(n = dplyr::n()) |>
  #   dplyr::arrange(siteiid, desc(n)) |> dplyr::group_by(siteiid) |>
  #   dplyr::filter(dplyr::row_number() != 1) |>  dplyr::pull(vegplotiid)
  #
  # # Remove dup_vegplots from veg_data$vegplotsspecies
  # veg_data_species_reduced <- veg_data$vegplotspecies |> dplyr::filter(!vegplotiid %in% dup_vegplots)
  #
  # # Remove dup_vegplots from veg_data$vegplot
  # veg_data_veg_plot_reduced <- veg_data$vegplot |>
  #   # dplyr::filter(!is.na(ecositeid)) |> I don't think I want to remove vegplots that don't have an ecosite correlation. Could be useful for things like SDMs.
  #   dplyr::select(siteiid, ecositeid, vegplotiid, akfieldecositeid) |>
  #   unique() |>
  #   dplyr::filter(!vegplotiid %in% dup_vegplots & vegplotiid %in% veg_data$vegplotspecies$vegplotiid)
  #
  #
  # ########### Joining ecositeid to veg_data_species_reduced
  # veg_data_with_ecosite <-
  #   dplyr::left_join(
  #     veg_data_species_reduced |>
  #       dplyr::select(
  #         siteiid,
  #         vegplotid,
  #         vegplotiid,
  #         plantsym,
  #         primarydatacollector,
  #         plantsciname,
  #         plantnatvernm,
  #         akstratumcoverclass,
  #         akstratumcoverclasspct
  #       ),
  #     aqp::site(ecosite_data) |>
  #       dplyr::select(siteiid, ecositeid) |> unique()
  #   ) |>
  #   dplyr::left_join(veg_data_veg_plot_reduced |> dplyr::select(siteiid, vegplotiid, akfieldecositeid)) |>
  #   dplyr::select(siteiid, vegplotid, ecositeid, everything())
  #
  #
  # # Remove a question mark from a couple of instances of akfieldecositeid - I don't want to remove question marks without users knowledge
  # # veg_data_with_ecosite$akfieldecositeid <-
  # #   veg_data_with_ecosite$akfieldecositeid |>
  # #   stringr::str_replace(pattern = "\\.\\?", replacement = "")
  #
  # # Identify the akfieldecositeids missing a phase
  # no_phase <-
  #   veg_data_with_ecosite$akfieldecositeid[!veg_data_with_ecosite$akfieldecositeid %in%
  #                                        stringr::str_subset(veg_data_with_ecosite$akfieldecositeid,
  #                                                            "[:digit:]\\.[:digit:]\\.[:digit:]") &
  #                                        !is.na(veg_data_with_ecosite$akfieldecositeid)]
  #
  # # Add default phase to those missing - default being .1
  # veg_data_with_ecosite$akfieldecositeid_edit <-
  #   ifelse(
  #     veg_data_with_ecosite$akfieldecositeid %in%
  #       no_phase,
  #     paste0(veg_data_with_ecosite$akfieldecositeid, ".1"),
  #     veg_data_with_ecosite$akfieldecositeid
  #   )
  #
  # # Still missing a phase?
  # no_phase <-
  #   veg_data_with_ecosite$akfieldecositeid_edit[!veg_data_with_ecosite$akfieldecositeid_edit %in%
  #                                            stringr::str_subset(veg_data_with_ecosite$akfieldecositeid_edit,
  #                                                                "[:digit:]\\.[:digit:]\\.[:digit:]") &
  #                                            !is.na(veg_data_with_ecosite$akfieldecositeid_edit)]
  #
  #
  # # Add default phase to those missing - default being .1
  # veg_data_with_ecosite$akfieldecositeid_edit <-
  #   ifelse(
  #     veg_data_with_ecosite$akfieldecositeid_edit %in%
  #       no_phase,
  #     paste0(veg_data_with_ecosite$akfieldecositeid_edit, ".1"),
  #     veg_data_with_ecosite$akfieldecositeid_edit
  #   )
  #
  # veg_data_with_ecosite <- veg_data_with_ecosite |> dplyr::select(-akfieldecositeid) |>
  #   dplyr::rename(akfieldecositeid = akfieldecositeid_edit)
  #
  # # Append coordinates
  # veg_data_with_ecosite_coords <-
  #   dplyr::left_join(veg_data_with_ecosite,
  #                    veg_data$vegplotlocation |>
  #                      dplyr::select(siteiid, horizdatnm, utmzone, utmeasting, utmnorthing) |>
  #                      unique())

  return(vegplot)

}
