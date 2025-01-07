#' Create vegetation dataframe from multiple possible locations
#'
#' @param from origin of vegetation data. "web_report" queries NASIS for vegetation data by ecosite - NASIS credentials are not needed. "SS" uses the users NASIS selected set (NASIS connection required).
#' "static" uses a local .sqlite NASIS database.
#' @param ecositeid ecositeid of interest - required when using from = "web_report". If used with from = "SS" or from = "static", limits returns to only ecositeid provided.
#' @param static_location file path to static NASIS .sqlite database - typically generated using `ecositer::vegStaticNASIS()`
#'
#' @description
#' This function provides access to vegetation data from multiple sources and returns the data in the same format regardless of source. This function provides raw data. QC should be performed on data
#' before analyzing.
#'
#'
#' @return vegetation dataframe
#' @export
#'
#' @examplesIf FALSE
#' my_formatted_veg_df <- formatted_veg_df(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
#' head(formatted_veg_df)
#'
create_veg_df <- function(from = c("web_report", "SS", "static"),
                             ecositeid = NULL,
                             static_location = NULL){

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
          utmnorthing,
          latdegrees,
          latminutes,
          latseconds,
          latdir,
          longdegrees,
          longminutes,
          longseconds,
          longdir,
          latstddecimaldegrees,
          longstddecimaldegrees
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

    vegplot <- vegplot |> dplyr::rename(usiteid = site_id, vegplotid = vegplot_id,
                                        latstDD = latstddecimaldegrees,
                                        longstDD = longstddecimaldegrees) |> as.data.frame()

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
  vegplot$latdegrees <- as.numeric(vegplot$latdegrees)
  vegplot$latminutes <- as.numeric(vegplot$latminutes)
  vegplot$latseconds <- as.numeric(vegplot$latseconds)
  vegplot$latdir <- as.character(vegplot$latdir)
  vegplot$longdegrees <- as.numeric(vegplot$longdegrees)
  vegplot$longminutes <- as.numeric(vegplot$longminutes)
  vegplot$longseconds <- as.numeric(vegplot$longseconds)
  vegplot$longdir <- as.character(vegplot$longdir)
  vegplot$latstDD <- as.numeric(vegplot$latstDD)
  vegplot$longstDD <- as.numeric(vegplot$longstDD)

  vegplot <- vegplot |> dplyr::arrange(usiteid, vegplotiid, plantsym) |>
    dplyr::select(siteiid, usiteid, siteobsiid, vegplotid, vegplotiid, # siteecositehistoryiid,
                  primarydatacollector, vegdataorigin, ecositeid, ecositenm, ecostateid, ecostatename, commphaseid, commphasename, plantsym,
                  plantsciname, plantnatvernm, akstratumcoverclasspct, speciescancovpct, speciescomppct, understorygrcovpct,
                  horizdatnm, utmzone, utmeasting, utmnorthing, latdegrees, latminutes, latseconds, latdir, longdegrees,
                  longminutes, longseconds, longdir, latstDD, longstDD) |> as.data.frame()

  return(vegplot)

}
