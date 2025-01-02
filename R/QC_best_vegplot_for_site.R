#' Keep best vegplot when multiple exist for site
#'
#' @description
#' This function determines what sites have multiple vegetation plots and
#' keeps only the best. The best vegetation plot is decided based on
#' the number of records for the vegetation plot (i.e., the number of rows in the
#' vegetation plot table). This function is intended to
#' provide basic functionality to avoid gross misrepresentations (i.e.,
#' pseudo-replication of vegetation plot/site relationships). This function should
#' not be considered a replacement for careful review of vegetation plots. If,
#' there are multiple vegetation plots with the same number of records, this function
#' will error. This most commonly occurs when there are two vegetation plots with
#' identical data. The best solution in that case is to remove duplicated
#' vegetation plots from NASIS.
#'
#'
#' @param veg_df
#'
#' @return a vegetation dataframe with one vegetation plot per site
#' @export
#'
#' @examples

QC_best_vegplot_for_site <- function(veg_df) {


    data <- data.table::as.data.table(veg_df)

    # Summarize data: count rows per vegplot for each site
    vegplot_summary <- data[, .(vegplot_count = .N), by = .(siteiid, vegplotiid)]

    # Determine the maximum count of rows for each site
    vegplot_summary[, max_count := max(vegplot_count), by = siteiid]

    # Check for ties: count the number of vegplots with the maximum count for each site
    tie_sites <- vegplot_summary[vegplot_count == max_count, .N, by = siteiid][N > 1, siteiid]

    # If ties exist, throw an error
    if (length(tie_sites) > 0) {
      stop("Cannot choose the best vegplot using number of records because there is a tie. ",
           "The following siteiids have ties: ", paste(tie_sites, collapse = ", "))
    }

    # Are there multiple vegplotiids for any siteiids?
    multi_logi <- data[, data.table::uniqueN(vegplotiid) > 1, by = siteiid][, any(V1)]

    if(multi_logi){
      message("Warning: There are sites with multiple vegetation plots. Reviewing these sites is preferable to automated selection. To view which sites have multiple vegetation plots:
              'Your veg_df' |> dplyr::group_by(siteiid) |>
                               dplyr::summarise(unique_vegplots = dplyr::n_distinct(vegplotiid)) |>
                               dplyr::filter(unique_vegplots > 1)")
    }

    # Keep only the vegplot(s) with the maximum count for each site
    best_vegplots <- vegplot_summary[vegplot_count == max_count, .(siteiid, vegplotiid)]

    # Merge with the original data to keep only the relevant vegplots
    result <- data[best_vegplots, on = .(siteiid, vegplotiid)]

    return(result)

}
