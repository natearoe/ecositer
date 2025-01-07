#' Aggregate abundance columns
#'
#' @param veg_df
#' @param fail_on_dup logical - whether the function should fail if any records
#' have multiple abundance columns populated.
#'
#' @description
#' This function is used to aggregate abundance data into one column (pct_cover). Abundance can come from four columns:
#' "akstratumcoverclasspct", "speciescancovpct", "speciescomppct", and "understorygrcovpct". If the same record uses multiple abundance
#' columns, pct_cover is calculated as the average of those values. When only one abundance column is used for a record,
#' that value is used for pct_cover.
#'
#' @return dataframe with one abundance column
#' @export
#' @seealso [QC_find_multiple_abundance()]
#'
#' @examples
#' QC_aggregate_abundance(veg_df = B100_veg)
QC_aggregate_abundance <- function(veg_df,
                                   fail_on_dup = FALSE){

  abund_cols <- c("akstratumcoverclasspct",
                  "speciescancovpct",
                  "speciescomppct",
                  "understorygrcovpct")

  if("pct_cover" %in% abund_cols){
    print("Abundance columns have already been aggregated to 'pct_cover'.")
  }

  if(sum(abund_cols %in% colnames(veg_df)) < 3){
    stop("Not all default abundance columns are present.")
  }

  if(sum(abund_cols %in% colnames(veg_df)) == 4){

    # Convert to data.table
    data <- data.table::as.data.table(veg_df)

    abund_cols_count <- data[, lapply(.SD, function(x) sum (x > 0, na.rm = TRUE)), .SDcols = abund_cols]

    abund_cols_used <- names(abund_cols_count)[unlist(abund_cols_count) > 0]

    if(length(abund_cols_used) > 1){
      warning(sprintf("Multiple abundance columns are used in this dataset: %s", paste(abund_cols_used, collapse = ", ")))
    }

    if(length(abund_cols_used) == 1){
      message(paste("Note ->", abund_cols_used, "is the only abundance column used in this dataset"))
    }

    names(abund_cols_used)[unlist(abund_cols_used) > 0]

    # Add a column counting non-NA values across abundance columns
    data[, non_na_abundance_count := rowSums(!is.na(.SD)),
         .SDcols = abund_cols]

    # Identify problematic records
    problematic_records <- data[non_na_abundance_count > 1]

    # If problematic records exist, issue a warning
    if(fail_on_dup == TRUE){
      if (nrow(problematic_records) > 0) {
        stop(
          "Some records have multiple abundance values populated. Use `ecositer::QC_find_multiple_abundance()` to investigate the affected plots and records."
        )
      }
    } else {
      if (nrow(problematic_records) > 0) {
        warning(
          "Some records have multiple abundance values populated. Use `ecositer::QC_find_multiple_abundance()` to investigate the affected plots and records."
        )
      }
    }





    # Create the 'abundance_master' column
    data[, pct_cover := rowMeans(.SD, na.rm = TRUE),
         .SDcols = abund_cols]

    # Remove the helper column and drop original abundance columns
    data[, `:=`(non_na_abundance_count = NULL,
                akstratumcoverclasspct = NULL, speciescancovpct = NULL, speciescomppct = NULL, understorygrcovpct = NULL)]

    return(data |> as.data.frame() |>
             dplyr::relocate(pct_cover, .after = 16)
           )

  }

}


#' Find vegetation records with multiple abundances
#'
#' @description
#' This function returns records that use multiple abundance columns. This allows users
#' to determine the cause and determine a better solution than averaging.
#'
#' @param veg_df
#'
#' @return dataframe of records with multiple abundance columns populated
#' @export
#' @seealso [QC_aggregate_abundance()]
#'
#' @examples
#' QC_find_multiple_abundance(veg_df = B100_veg)
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
  data_red <- data[non_na_abundance_count > 1]

  # Remove the helper column
  data_red[, non_na_abundance_count := NULL]

  return(data_red |> as.data.frame())

}



# QC_aggregate_species_abundance <- function(veg_df){
#
#   # convert to data.table
#   veg_df <- data.table::as.data.table(veg_df)
#
#   # grouping columns
#   cols_keep <- colnames(veg_df)[colnames(veg_df) != "plantsym"]
#
#   # grouping columns
#   # cols_keep <- c("siteiid", "usiteid", "siteobsiid", "vegplotid", "vegplotiid", "primarydatacollector", "vegdataorigin", "ecositeid", "ecositenm", "ecostateid", "ecostatename", "commphaseid", "commphasename")
#
#   # aggregate abundances when the same species listed multiple times in vegplotiid (this occurs when the same species occurs in multiple strata)
#   veg_df_c <- merge(x = veg_df[, ..cols_keep] |> unique(),
#                     y = veg_df[, .(pct_cover = sum(pct_cover, na.rm = TRUE)),
#                                by = .(vegplotiid, plantsym)])
# }


