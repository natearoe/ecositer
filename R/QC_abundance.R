#' Title
#'
#' @param veg_df
#'
#' @return
#' @export
#' @seealso [QC_find_multiple_abundance()]
#'
#' @examples
#'
QC_aggregate_abundance <- function(veg_df){

  abund_cols <- c("akstratumcoverclasspct",
                  "speciescancovpct",
                  "speciescomppct",
                  "understorygrcovpct")

  # Convert to data.table
  data <- data.table::as.data.table(veg_df)

  # Add a column counting non-NA values across abundance columns
  data[, non_na_abundance_count := rowSums(!is.na(.SD)),
       .SDcols = abund_cols]

  # Identify problematic records
  problematic_records <- data[non_na_abundance_count > 1]

  # If problematic records exist, issue a warning
  if (nrow(problematic_records) > 0) {
    warning(
      "Some records have multiple abundance values populated.\n",
      "Use `ecositer::QC_find_multiple_abundance()` to investigate the affected plots and records."
    )
  }

  # Create the 'abundance_master' column
  data[, pct_cover := rowMeans(.SD, na.rm = TRUE),
       .SDcols = abund_cols]

  # Remove the helper column and drop original abundance columns
  data[, `:=`(non_na_abundance_count = NULL,
              akstratumcoverclasspct = NULL, speciescancovpct = NULL, speciescomppct = NULL, understorygrcovpct = NULL)]

  return(data |> as.data.frame() |>
           dplyr::select(siteiid, usiteid, siteobsiid, vegplotid, vegplotiid, # siteecositehistoryiid,
                         primarydatacollector, vegdataorigin, ecositeid, ecositenm, ecostateid, ecostatename, commphaseid, commphasename, plantsym,
                         plantsciname, plantnatvernm, pct_cover,
                         horizdatnm, utmzone, utmeasting, utmnorthing))

}


#' Find vegetation records with multiple abundances
#'
#' @param veg_df
#'
#' @return dataframe of records with multiple abundance columns populated
#' @export
#' @seealso [QC_aggregate_abundance()]
#'
#' @examples
#'
QC_find_multiple_abundance <- function(veg_df){

  # abundance columns
  abund_cols <- c("akstratumcoverclasspct",
                  "speciescancovpct",
                  "speciescomppct",
                  "understorygrcovpct")

  # Convert to data.table
  data <- data.table::as.data.table(veg_df)

  # Add a column counting non-NA values across abundance columns
  data[, non_na_abundance_count := rowSums(!is.na(.SD)),
       .SDcols = abund_cols]

  # Return only the rows with multiple abundance columns populated
  data[non_na_abundance_count > 1]

  # Remove the helper column
  data[, non_na_abundance_count := NULL]

  return(data |> as.data.frame())

}



