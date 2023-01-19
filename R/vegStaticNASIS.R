#' Create vegetation data Static NASIS
#'
#'`vegStaticNASIS` saves a NASIS selected set to your local drive. The function utilizes
#'`soilDB::createStaticNASIS`, and includes the appropriate tables to access
#'vegetation data. The output can be used as a reliable, local copy of query results,
#'allowing the user to change their NASIS selected set and still access the
#'data of interest.
#'
#'Examples of queries to use this function with include, MLRA13_Wasilla >
#'Pedon/Site/Transect/Vegetation Plot by usiteid (multiple) & MLRA13_Wasilla >
#'Pedon/Site/Vegetation Plot by site area overlap table
#'
#'Code written by Andrew Brown
#'
#' @param output_path
#'
#' @return
#' @export
#'
#' @examples
vegStaticNASIS <- function(output_path){

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
    SS = TRUE,
    tables = c(
      soilDB::get_NASIS_table_name_by_purpose(p),
      soilDB::get_NASIS_table_name_by_purpose(p, SS = TRUE)
    ),
    output_path = output_path
  )

}





