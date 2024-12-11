#' Title
#'
#' @param veg_df
#'
#' @return
#' @export
#'
#' @examples
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
                         horizdatnm, utmzone, utmeasting, utmnorthing)

}


QC_find_multiple_abundance

