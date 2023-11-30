#' Ecosite similarity list
#'
#' @param veg_df a properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df}).
#' The data should be QCed to ensure that it meets minimum data requirements (\link[ecositer]{QC_vegplots})
#' @param dist_pair a distance
#'
#' @return a list of ecosite similarities
#' @export
#'
#' @examples
#' ecositer::ecosite_similarity_pairwise(veg_df = my_veg_df, dist_pair = my_dist)
ecosite_similarity_pairwise <- function(veg_df, dist_pair){

  options(digits = 15)

  dist_pair <- dist_pair |> as.data.frame() |> dplyr::left_join(veg_df |>
                                       dplyr::select(vegplotid, ecositeid) |>
                                         unique() |>
                                       dplyr::rename(plot1_es = ecositeid,
                                                     plot1 = vegplotid)) |>
    dplyr::left_join(veg_df |>
                       dplyr::select(vegplotid, ecositeid) |>
                       unique() |>
                       dplyr::rename(plot2_es = ecositeid,
                                     plot2 = vegplotid))


  list_es <- list()
  es_cols <- c("plot1_es", "plot2_es")

  es_group <- function(a, b){
    p <- c(a, b)
    ifelse(anyNA(p), i,
           ifelse(all(p == i), i,
                  p[p != i]))
  }

  for(i in unique(veg_df$ecositeid[!is.na(veg_df$ecositeid)])){
    dist_pair_i <-  dist_pair |> dplyr::filter(plot1_es == i |
                                               plot2_es == i)

    dist_pair_i$group <- mapply(FUN = es_group, dist_pair_i$plot1_es, dist_pair_i$plot2_es)


    dist_pair_i_sum <- dist_pair_i |> dplyr::group_by(group) |> dplyr::summarise(avg_bbd = mean(bbd),
                                                                          sd_bbd = sd(bbd))
    list_es[[paste(i)]] <- dist_pair_i_sum
  }

  return(list_es)

}


