#' Create local vegetation database
#'
#'`create_local_veg_db` saves your NASIS local database (not selected set!) to your local drive as a .sqlite database. The function utilizes
#'`soilDB::createStaticNASIS`, and includes the appropriate tables to access
#'vegetation data. The output can be used as a reliable, local copy of query results,
#'allowing the user to change their NASIS selected set and still access the
#'data of interest. The file extension needs to be ".sqlite". If you are working with a
#'dataset that commonly has new data entered, it is probably best to work off of your
#'NASIS selected set rather than using a local sqlite database.
#'
#'
#' @param output_path path where file should be saved (must have .sqlite extension)
#' @return a local .sqlite database of your SS
#' @export
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





