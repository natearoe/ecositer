#' Distance matrix to pairwise distance dataframe
#'
#' @param dist_matrix
#'
#' @return a data frame of pairwise distances between plots
#' @export
#'
#' @examples
#' site_similarity_pairwise(dist_matrix = my_dist)
site_similarity_pairwise <- function(dist_matrix){
  dist_matrix[lower.tri(dist_matrix, diag = FALSE)] = NA
  dist_matrix <- as.data.frame(as.table(dist_matrix))
  dist_matrix<-na.omit(dist_matrix) |> dplyr::rename(bbd = Freq,
                                         plot1 = Var1,
                                         plot2 = Var2) |>
    dplyr::arrange(bbd)

  return(dist_matrix)
}


