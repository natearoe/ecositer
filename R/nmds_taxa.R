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
#'
#' @return an ordination of plots in species space of all plots that contain the taxa of interest
#' @export
#'
#' @examples
nmds_taxa <- function(veg_df, taxa, spp.scr.thrshld = 0.01){
  # Determine strings matching taxa argument
  taxa_v <- veg_df$plantsciname |>
    stringr::str_subset(pattern = taxa)

  # Reduce veg_df to taxa strings
  sites_with_taxa <- veg_df |> dplyr::filter(plantsciname %in% taxa_v)
  sites_with_taxa$taxa_id <- sites_with_taxa$plantsciname
  sites_with_taxa$siteiid <- as.character(sites_with_taxa$siteiid)

  # Remove taxa of interest so that they do not affect the ordination results;
  # only keep sites with taxa of interest
  taxa_df <- veg_df |> dplyr::filter(!plantsciname %in% taxa_v) |>
    dplyr::filter(siteiid %in% sites_with_taxa$siteiid)
  taxa_df$siteiid <- as.character(taxa_df$siteiid)

  # Join in ctaxa_id
  taxa_df <- taxa_df |> dplyr::left_join(sites_with_taxa |> dplyr::select(siteiid, taxa_id))

  # Prep df for NMDS
  taxa_df <- taxa_df |> dplyr::select(siteiid, plantsciname, akstratumcoverclasspct, ecositeid) |>
    dplyr::group_by(siteiid, plantsciname) |>
    dplyr::summarise(total_abund = sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    tidyr::pivot_wider(names_from = plantsciname,
                       values_from = total_abund) |>
    tibble::column_to_rownames("siteiid")

  # Assign NA values to zero
  taxa_df2[is.na(taxa_df2)] <- 0

  # Run ordination
  my_nmds <- vegan::metaMDS(taxa_df2)

  # Calculate site scores
  site_scrs <- as.data.frame(vegan::scores(my_nmds, display = "sites")) |>
    tibble::rownames_to_column("siteiid") |>
    dplyr::left_join(taxa_df |> dplyr::select(siteiid, taxa_id) |>
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
                                                                text = siteiid,
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
