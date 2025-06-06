#' Run NMDS for taxa of interest
#'
#' This function is intended to provide a quick visualization of sites containing
#' a taxa of interest. It is intended to be used in assessing whether taxa can
#' be combined. For example, an ordination showing strong overlap in vegetative
#' communities between sites with "Arenaria" and "Arenaria kingii" might suggest
#' that these two taxa can be combined.
#'
#' @param veg_df properly formatted vegetation dataframe
#' @param taxa taxa of interested (e.g., "Arenaria")
#' @param spp.scr.thrshld p-value threshold for species scores being displayed
#' @param reference_taxa additional taxa used to compare spread in ordination space
#'
#' @return an ordination of plots in species space of all plots that contain the taxa of interest
#' @noRd
#'
nmds_taxa <- function(veg_df, taxa,
                      reference_taxa = NULL,
                      spp.scr.thrshld = 0.01){

  if(is.null(reference_taxa)){
    taxa_v <- veg_df$plantsciname |>
      stringr::str_subset(pattern = taxa)
  } else {
    taxa_v <- veg_df$plantsciname |>
      stringr::str_subset(pattern = taxa)
    taxa_r <- veg_df$plantsciname |>
      stringr::str_subset(pattern = reference_taxa)
    taxa_v <- c(taxa_v, taxa_r)
  }

  # Reduce veg_df to taxa strings
  sites_with_taxa <- veg_df |> dplyr::filter(plantsciname %in% taxa_v)
  sites_with_taxa$taxa_id <- sites_with_taxa$plantsciname
  sites_with_taxa$vegplotid <- as.character(sites_with_taxa$vegplotid)

  # Remove taxa of interest so that they do not affect the ordination results;
  # only keep sites with taxa of interest
  taxa_df <- veg_df |> dplyr::filter(!plantsciname %in% taxa_v) |>
    dplyr::filter(vegplotid %in% sites_with_taxa$vegplotid)
  taxa_df$vegplotid <- as.character(taxa_df$vegplotid)

  # Join in ctaxa_id
  taxa_df <- taxa_df |> dplyr::left_join(sites_with_taxa |> dplyr::select(vegplotid, taxa_id))

  # Prep df for NMDS
  taxa_df2 <- taxa_df |> dplyr::select(vegplotid, plantsciname, akstratumcoverclasspct, ecositeid) |>
    dplyr::group_by(vegplotid, plantsciname) |>
    dplyr::summarise(total_abund = sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    tidyr::pivot_wider(names_from = plantsciname,
                       values_from = total_abund) |>
    tibble::column_to_rownames("vegplotid")

  # Assign NA values to zero
  taxa_df2[is.na(taxa_df2)] <- 0

  tot <- rowSums(taxa_df2)
  taxa_df2 <- taxa_df2[tot > 0, ]

  # Run ordination
  my_nmds <- vegan::metaMDS(taxa_df2)

  # Calculate site scores
  site_scrs <- as.data.frame(vegan::scores(my_nmds, display = "sites")) |>
    tibble::rownames_to_column("vegplotid") |>
    dplyr::left_join(taxa_df |> dplyr::select(vegplotid, taxa_id) |>
                       unique())

  # Calculate species scores
  spp.envfit <- vegan::envfit(my_nmds, taxa_df2,
                              choices = c(1,2), permutations = 999)
  spp.scores <- as.data.frame(vegan::scores(spp.envfit, display = "vectors"))
  spp.scores <- cbind(spp.scores, species = rownames(spp.scores))
  spp.scores <- cbind(spp.scores, pval = spp.envfit$vectors$pvals)
  sig.spp.scrs <- subset(spp.scores, pval <= spp.scr.thrshld)


  my_plot <- ggplot2::ggplot() + ggplot2::geom_point(data = site_scrs,
                                                   ggplot2::aes(x = NMDS1, y = NMDS2,
                                                                text = vegplotid,
                                                                colour = taxa_id,
                                                                position = "jitter"),
                                                   size = 2.5)  +
    ggplot2::geom_text(data = sig.spp.scrs,
                       ggplot2::aes(x = NMDS1,
                                    y = NMDS2,
                                    label = species),
                       size = 2,
                       position = ggplot2::position_jitter())


  plotly::ggplotly(my_plot)


  }
