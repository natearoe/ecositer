#' Create vegetation data frame
#'
#' @description
#' This function return a vegetation data frame from multiple data sources. Accessing data through web reports provides public access to NASIS vegetation data.
#'
#' @param from Source of vegetation data. Options:
#'   - `"web_report"`: Accesses NASIS vegetation data via web report (no NASIS credentials required)
#'   - `"SS"`: Uses NASIS Selected Set (requires NASIS connection)
#'   - `"static"`: Uses a local NASIS `.sqlite` database
#' @param ecositeid Ecological site ID of interest. Required for `"web_report"`.
#' If used with `"SS"` or `"static"`, limits results to that ecosite ID.
#' @param static_location File path to a static NASIS `.sqlite` database, typically created with `ecositer::vegStaticNASIS()`
#'
#' @return vegetation data frame
#' @export
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
      ecositeid,
      siteecositehistory.classifier,
      cancovtotalpct,
      cancovtotalclass,
      overstorycancontotalpct,
      overstorycancovtotalclass
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
          understorygrcovpct,
          speciestraceamtflag,
          vegetationstratalevel,
          akstratumcoverclass,
          plantheightcllowerlimit,
          plantheightclupperlimit,
          planttypegroup
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
                                        longstDD = longstddecimaldegrees,
                                        ecositeclassifier = siteecositehistory.classifier) |> as.data.frame()

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
  vegplot$ecositeclassifier <- as.character(vegplot$ecositeclassifier)
  vegplot$ecostateid <- as.character(vegplot$ecostateid)
  vegplot$ecostatename <- as.character(vegplot$ecostatename)
  vegplot$commphaseid <- as.character(vegplot$commphaseid)
  vegplot$commphasename <- as.character(vegplot$commphasename)
  vegplot$cancovtotalpct <- as.character(vegplot$cancovtotalpct)
  vegplot$cancovtotalclass <- as.character(vegplot$cancovtotalclass)
  vegplot$overstorycancontotalpct <- as.character(vegplot$overstorycancontotalpct)
  vegplot$overstorycancovtotalclass <- as.character(vegplot$overstorycancovtotalclass)
  vegplot$plantsym <- as.character(vegplot$plantsym)
  vegplot$plantsciname <- as.character(vegplot$plantsciname)
  vegplot$plantnatvernm <- as.character(vegplot$plantnatvernm)
  vegplot$akstratumcoverclasspct <- as.numeric(vegplot$akstratumcoverclasspct)
  vegplot$speciescancovpct <- as.numeric(vegplot$speciescancovpct)
  vegplot$speciescomppct <- as.numeric(vegplot$speciescomppct)
  vegplot$understorygrcovpct <- as.numeric(vegplot$understorygrcovpct)
  vegplot$speciestraceamtflag <- as.character(vegplot$speciestraceamtflag)
  vegplot$vegetationstratalevel <- as.character(vegplot$vegetationstratalevel)
  vegplot$plantheightclupperlimit <- as.character(vegplot$plantheightclupperlimit)
  vegplot$plantheightcllowerlimit <- as.character(vegplot$plantheightcllowerlimit)
  vegplot$planttypegroup <- as.character(vegplot$planttypegroup)
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
                  primarydatacollector, vegdataorigin, ecositeid, ecositenm, ecostateid, ecostatename, commphaseid, commphasename, cancovtotalpct,
                  cancovtotalclass, overstorycancontotalpct, overstorycancovtotalclass,  plantsym,
                  plantsciname, plantnatvernm, akstratumcoverclasspct, speciescancovpct, speciescomppct, understorygrcovpct, speciestraceamtflag,
                  vegetationstratalevel, akstratumcoverclass, plantheightcllowerlimit, plantheightclupperlimit, planttypegroup, horizdatnm, utmzone, utmeasting, utmnorthing, latdegrees, latminutes, latseconds, latdir, longdegrees,
                  longminutes, longseconds, longdir, latstDD, longstDD) |> as.data.frame()

  return(vegplot)

}
