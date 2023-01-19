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



