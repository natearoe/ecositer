#' @title Create local NASIS database
#' @description
#' This function creates a local .sqlite database from your NASIS local database
#' (not selected set!). It uses [soilDB::createStaticNASIS()] but makes accessing the desired tables
#' easier without having to explicitly name them. For datasets that are static,
#' this is an efficient way of accessing data without having to clear your local
#' database and query.
#'
#' @param output_path path where file should be saved (must have .sqlite extension)
#' @return a local .sqlite database of your local database
#' @export
#'
#' @examplesIf requireNamespace("curl") && curl::has_internet()
#'   result <- try(create_local_veg_db(output_path = "Users/You/veg_db.sqlite"), silent = TRUE)
#'   if (inherits(result, "try-error")) {
#'   message("Connection failed. Check database status.")
#'   } else {
#'   print(results)
#'   }
#'

create_local_veg_db <- function(output_path){

  p <-
    c(
      "area",
      "legend",
      "mapunit",
      "datamapunit",
      "component",
      "metadata",
      "lookup",
      "nasis",
      "transect",
      "site",
      "pedon",
      "vegetation"
    )

  soilDB::createStaticNASIS(
    tables = soilDB::get_NASIS_table_name_by_purpose(p),
    output_path = output_path
  )

}





