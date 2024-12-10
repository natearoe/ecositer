

#' Determine
#'
#' @param veg_df
#'
#' @return
#' @export
#'
#' @examples
QC_multiple_vegplots_for_site <- function(veg_df, choose_best = FALSE) {

  # add checks to see if the veg_df is properly formatted.

  if(!choose_best){
    data <- as.data.table(veg_df)
    data[, data.table::uniqueN(vegplotiid) > 1, by = siteiid][, any(V1)]
  }

  if(choose_best){
    # use QC_best_vegplot_for_site()
  }


}



