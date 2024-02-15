#' NMDS taxon comparison
#'
#' @param veg_df a properly formatted vegetation dataframe
#' @param taxon the taxon of interest
#' @param rare_species TRUE or FALSE to include or remove, respectively
#' @param min_plots
#'
#' @return a NMDS of taxon of interest
#' @export
#'
#' @examples
nmds_taxon_comparison <- function(veg_df, taxon, rare_species, min_plots){

  # remove missing species
  veg_df <- veg_df |> dplyr::filter(!is.na(plantsciname))

  # determine taxon identified
  taxon_of_interest <- stringr::str_subset(veg_df$plantsciname,
                                           paste(taxon, collapse = "|"))

  # make dataframe of taxon of interest
  taxon_df <- veg_df |> dplyr::filter(plantsciname %in%
                                               taxon_of_interest)

  # split taxon dataframe into separate lists by individual taxon
  taxon_list <- split(taxon_df, taxon_df$plantsciname)

  #combine dataframes and put an identifying column
  taxon_df_class <- dplyr::bind_rows(taxon_list, .id = "class")

  # join the rest of the data
  veg_df_class <- dplyr::left_join(taxon_df_class |>
                                     dplyr::select(class, siteiid) |>
                                     unique(),
                                   veg_df,
                                   by = dplyr::join_by(siteiid),
                                   multiple = "all")

  # pivot df wider
  veg_df_class_wide <- veg_df_class |> dplyr::select(siteiid, plantsciname, akstratumcoverclasspct) |>
    tidyr::pivot_wider(names_from = plantsciname,
                                     values_from = akstratumcoverclasspct,
                                     values_fn = sum)

  # change NAs to 0
  veg_df_class_wide[is.na(veg_df_class_wide)] <- 0

  # remove rare species
  numb_of_plots <- apply(veg_df_class_wide > 0, 2, sum)
  perc_of_plots <- 100 * numb_of_plots/nrow(veg_df_class_wide)
  veg_df_class_wide_reduced <- veg_df_class_wide[, perc_of_plots >= rare_species]

  # relativize by max


  # run nmds
  my_nmds <- vegan::metaMDS(comm = veg_df_class_wide_reduced,
                 distance = "bray",
                 autotransform = TRUE,
                 k = 2)

  # species scores
  spp.envfit <- vegan::envfit(my_nmds, veg_df_class_wide_reduced,
                              choices = c(1,2), permutations = 9999)
  spp.scores <- as.data.frame(vegan::scores(spp.envfit, display = "vectors"))
  spp.scores <- cbind(spp.scores, species = rownames(spp.scores))
  spp.scores <- cbind(spp.scores, pval = spp.envfit$vectors$pvals)
  sig.spp.scrs <- subset(spp.scores, pval <= 0.005)

  # site scrs
  site_scrs <- as.data.frame(vegan::scores(my_nmds, display = "sites"))
  site_scrs <- cbind(site_scrs, veg_df_class_wide_reduced[,1]) |>
    dplyr::left_join(y = veg_df_class |>
                       dplyr::select(class, vegplotid, siteiid) |>
                       unique(),
                     by = dplyr::join_by(siteiid), multiple = "all")
  site_scrs$class <- as.factor(site_scrs$class)

  # plot
  my_plot <- ggplot2::ggplot() + ggplot2::geom_point(data = site_scrs,
                                 ggplot2::aes(x = NMDS1, y = NMDS2,
                                              text = vegplotid,
                                              colour = class,
                                              position = "jitter"),
                                 size = 2.5) +
    ggplot2::geom_text(data = sig.spp.scrs,
                             ggplot2::aes(x = NMDS1,
                                 y = NMDS2,
                                 label = species),
                       size = 2,
                       position = ggplot2::position_jitter())

  plotly::ggplotly(my_plot, tooltip = c("all"))


}
