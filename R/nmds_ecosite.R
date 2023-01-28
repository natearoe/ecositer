#' Run NMDS on vegetation data
#'
#' `nmds_ecosite` runs an NMDS using the vegan package. NMDS has numerous settings
#' that can be adjusted, not all of which can be manipulated through the arguments
#' of this function. This function is intended to be used as a quick way of producing
#' ordinations. If you are interested in optimizing your NMDS, please do your own
#' research. At a minimum, you should understand stress and evaluate the stress
#' report in the results.
#'
#' @param veg_summary veg_summary object where the `ecositer::veg_summary()` function was saved.
#' @param ecosite ecosite is the ecosite(s) you want to include in an ordination. If you are
#' interested in ordinating multiple ecosites together, concatenate the ecosites of interest
#' (e.g., `c("a", "b")`)
#' @param pres_abs use TRUE to convert data to presence/absence. This is the more
#' robust option. If you have missing abundance values, presence/absence should be used.
#' If you have abundance data, use FALSE.
#' @param nmds_dim specify the number of dimensions you want in your NMDS. This value
#' will affect stress considerably.
#' @param reduce_species `NA` if you do not want to remove species. A numeric value,
#' interpretted as percent, specifying the minimum percentage of plots a species
#' must be present in to remain in the dataset.
#'
#' @return
#' @export
#'
#' @examples
nmds_ecosite <- function(veg_summary, ecosite, pres_abs, nmds_dim, reduce_species){

  ecosite_bind <- NULL

  if(isTRUE(pres_abs)){

    if(length(ecosite) == 1){

      # use pres_abs_conversion to create pres_abs df
      veg_df_wide_pres_abs_reduced <<- ecositer::pres_abs_conversion(veg_summary[[ecosite]]$Raw_data)


    }

    if(length(ecosite) != 1){

      # for loop to rbind multiple dataframes
      ecosite_bind <- NULL

      for(i in seq(length(ecosite))){
        ecosite_bind <- rbind(veg_summary[[ecosite[i]]]$Raw_data, ecosite_bind)
      }

      veg_df_wide_pres_abs_reduced <<- ecositer::pres_abs_conversion(ecosite_bind)



    }

    # remove rare species, loop for the condition of reduce_species

    if(is.na(reduce_species)){

      pres_abs_nmds <- vegan::metaMDS(veg_df_wide_pres_abs_reduced, autotransform = FALSE, try = 50,
                                      trymax = 100, k = nmds_dim, maxit = 100, parallel = 10, distance = "bray",
                                      binary = TRUE)

    }

    else {

      # calculating how many sites each species is present in
      numb_of_plots <- apply(veg_df_wide_pres_abs_reduced > 0, 2, sum)

      # calculating the percent of plots each species is present in
      perc_of_plots <- 100 * numb_of_plots/nrow(veg_df_wide_pres_abs_reduced)

      # create new df wtih only species in more than 5 plots
      veg_df_wide_pres_abs_reduced <- veg_df_wide_pres_abs_reduced[, perc_of_plots >= reduce_species]

      # Remove rows if all zeros
      veg_df_wide_pres_abs_reduced <- veg_df_wide_pres_abs_reduced[as.logical(rowSums(veg_df_wide_pres_abs_reduced !=
                                                                                        0)), ]

      # Run NMDS
      pres_abs_nmds <- vegan::metaMDS(veg_df_wide_pres_abs_reduced, autotransform = FALSE, try = 50,
                                      trymax = 100, k = 1, maxit = 100, parallel = 10, distance = "bray",
                                      binary = TRUE)

    }



  }

  if(isFALSE(pres_abs)){

    return("Dev ongoing")

  }

  return(pres_abs_nmds)

}


#
# foobar <- nmds_ecosite(veg_summary = test, ecosite = c("R022AB006CA", "R022AA102CA"),
#              pres_abs = TRUE, reduce_species = 5, nmds_dim = 3)
#
#
# foobar <- nmds_ecosite(veg_summary = test, ecosite = "R022AB006CA",
#                        pres_abs = TRUE, reduce_species = NA, nmds_dim = 3)

