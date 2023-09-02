#' Convert normal abundance data to presence/absence
#'
#'`pres_abs_conversion` is an internal function that is used within the
#'\link[ecositer]{nmds_ecosite} function. Using presence/absence data will
#'allow you to use data from plots that do not have abundance data. In certain
#'datasets, this is an advantageous feature. When I have used NMDS on moderate
#'sized datasets (i.e., 100+ plots), I have found it to yield similar results
#'as abundance data. This could be different for small datasets.
#'
#' @param veg_df vegetation dataframe that will be converted to presence/absence.
#'
#' @return a presence/absence dataframe
#' @export
#'
#' @examples
#' my_pres_abs <- pres_abs_conversion(veg_df = ecositer::vegetation_dataframe)
#' head(my_pres_abs)
#'
pres_abs_conversion <- function(veg_df){

  # assign all species observations to 1. This gives a value to those previously unassigned (NAs)
 veg_df$akstratumcoverclasspct <- 1

  # aggregate the plot level species data - combined the same species within same plot
  veg_df_summarized <- veg_df %>% dplyr::group_by(vegplotid, plantsciname) %>%
    dplyr::summarise(sum(akstratumcoverclasspct)) %>% dplyr::rename(pctcover = `sum(akstratumcoverclasspct)`)

  # pivot df wider - rows plots, columns species
  veg_df_wide_pres_abs <- veg_df_summarized %>% tidyr::pivot_wider(names_from = plantsciname, values_from = pctcover)

  # Plot names from column to rownames
  veg_df_wide_pres_abs <- veg_df_wide_pres_abs %>% tibble::column_to_rownames(var = "vegplotid")

  # turn NAs to zeros
  veg_df_wide_pres_abs[is.na(veg_df_wide_pres_abs)] <- 0

  # Remove rows if all zeros
  veg_df_wide_pres_abs_reduced <<- veg_df_wide_pres_abs[as.logical(rowSums(veg_df_wide_pres_abs !=
                                                                             0)), ]

  return(veg_df_wide_pres_abs_reduced)


}
