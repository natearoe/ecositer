#' NMDS plot
#'
#'`nmds_plot` produces and interactive NMDS from the object created with `ecositer::nmds_ecosite`. The function allows the user to define several properties,
#'such as where to use pres/abs data, the number of dimensions, and whether to remove rare species.
#'
#' @param static_location path to static nasis SQlite.
#' @param nmds object created from `ecositer::nmds_ecosite`
#' @param veg_summary objected created from `ecositer::veg_summary`
#'
#' @return interactive NMDS plot
#' @export
#'
#' @examples
#' example_data <- access_example_data()
#' my_nmds_plot <- nmds_plot(static_location = example_data,
#' nmds = my_nmds, veg_summary = my_veg_summary)
#' # user-defined static location
#' # my_nmds_plot <- nmds_plot(static_location = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite",
#' nmds = my_nmds, veg_summary = my_veg_summary)
#'

nmds_plot <- function(static_location, nmds, veg_summary){

  ecosite_data <- soilDB::fetchNASIS(dsn = static_location,
                             from = "pedons",
                             SS = FALSE, fill = TRUE, duplicates = TRUE)

  veg_data <- soilDB::fetchVegdata(dsn = static_location, SS = FALSE)


  veg_df_with_plot <- veg_df_wide_pres_abs_reduced %>% tibble::rownames_to_column("vegplot_id") %>%
    dplyr::left_join(veg_data$vegplot %>% dplyr::select(vegplot_id, site_id, akfieldecositeid) %>%
                dplyr::arrange(site_id, akfieldecositeid) %>% dplyr::group_by(site_id) %>%
                dplyr::filter(row_number() == 1))

  veg_df_with_plot$richness <- rowSums(veg_df_with_plot %>% dplyr::select(-vegplot_id, -site_id, -akfieldecositeid))
  veg_df_with_plot$shannon <- apply(veg_df_with_plot %>% dplyr::select(-vegplot_id, -site_id, -akfieldecositeid) %>%
                                      as.matrix, MARGIN = 1, FUN = function(x){
                                        vegan::diversity(x)
                                      })

  plot_data <- ecosite_data@site %>% dplyr::filter(site_id %in% veg_df_with_plot$site_id) %>%
    dplyr::select(site_id, slope_field, elev_field, drainagecl, pmkind, surface_total_frags_pct, bedrckdepth) %>% unique() %>%
    dplyr::left_join(veg_df_with_plot %>% dplyr::select(site_id, richness, shannon, akfieldecositeid))




  # Environmental variable
  env.envfit <- vegan::envfit(my_nmds, plot_data %>% dplyr::select(-site_id), choices = c(1,2), permutations = 9999,
                              na.rm = TRUE)

  # Species scores
  species.envfit <- vegan::envfit(my_nmds, veg_df_with_plot %>%
                                    dplyr::select(-vegplot_id, -site_id, -richness, -shannon,
                                                  -akfieldecositeid),
                                  permutations = 9999)


  species.scores <- as.data.frame(vegan::scores(my_nmds, "species")) %>%
    tibble::rownames_to_column("species") %>%
    cbind(pval = species.envfit$vectors$pvals) %>%
    dplyr::filter(pval <= 0.05)

  # Site scores
  site.scrs <- as.data.frame(vegan::scores(my_nmds, display = "sites")) %>% tibble::rownames_to_column("vegplot_id") %>%
    dplyr::left_join(veg_df_with_plot %>% dplyr::select(vegplot_id, site_id))

  site.scrs <- dplyr::full_join(site.scrs, plot_data) %>% dplyr::select(site_id, NMDS1, NMDS2, akfieldecositeid)

  # Environmental scores
  env.scores <- as.data.frame(vegan::scores(env.envfit, display = "vectors")) %>%
    tibble::rownames_to_column("env.variables")

  env.scores <- cbind(env.scores, pval = env.envfit$vectors$pvals,
                      r = env.envfit$vectors$r)

  sig.env.scores <- subset(env.scores, r >= 0.08)

  # plotting
  pointSize = 5
  textSize = 10
  spaceLegend = 0.3

  bar <- ggplot2::ggplot(site.scrs, ggplot2::aes(x = NMDS1, y = NMDS2)) +
    geom_point(ggplot2::aes(color = factor(akfieldecositeid),
                   text = site_id)) +
    labs(title = "Presence absence NMDS", color = "Ecosite state/phase") +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize),
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines")) +
    geom_text(data = species.scores, ggplot2::aes(x = NMDS1, y = NMDS2, label = species), size = 2) +
    geom_segment(data = env.scores, ggplot2::aes(x = 0, xend = NMDS1, y = 0,
                                        yend = NMDS2),
                 arrow = arrow(length = unit(0.25, "cm")),
                 colour = "grey10", lwd = 0.3) +
    geom_text(data = env.scores, ggplot2::aes(x = NMDS1, y = NMDS2,
                                     label = env.variables), cex = 4)

  plotly::ggplotly(bar, tooltip = c("text", "color"), width = 750, height = 500)





}
