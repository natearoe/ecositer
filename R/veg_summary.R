#' Produce vegetation summary of ecosites and states/phases
#'
#' `veg_summary()` takes a *properly formatted vegetation dataframe* and produces summaries
#' of vegetation for ecosites and states/phases. Vegetation data originates from the
#' NASIS vegplot table. The BLANK function should be used to manipulate raw NASIS
#' vegplot data into a *properly formatted vegetation dataframe*
#'
#' @param veg_df a properly formatted vegetation dataframe
#'
#' @return a nested list of vegetation summaries by ecosite and state/phase
#' @export
#'
#' @examples
#'
#' veg_summary(veg_df = ecositer::vegetation_dataframe)
#'
#'
veg_summary <- function(veg_df){


  ecosite_list <- list()
  foo_list <- list()
  goo_list <- list()

  for(i in unique(veg_df$ecositeid[!is.na(veg_df$ecositeid)])){

    # Create'ecosite_list' and a dataframe with all the raw veg data for each ecosite
    ecosite_list[[i]][["Raw_data"]] <- veg_df %>% dplyr::filter(ecositeid == i)

    # Create placeholder sub-list
    ecosite_list[[i]][["STM"]] <- list()


    # Modify raw veg data to sum cover within species (e.g., if a single plant species is in multiple strata, its cover
    # will be summed across strata to give a single summed cover)
    ecosite_species_sum <- veg_df %>% dplyr::filter(ecositeid == i) %>%
      dplyr::group_by(vegplotid, plantsciname) %>% summarise(sum(akstratumcoverclasspct)) %>%
      dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct)`)


    # Clear foo_list
    foo_list <- list()

    # This loop summarizes the vegetation data by ecosite
    for(j in unique(ecosite_species_sum$plantsciname[!is.na(ecosite_species_sum$plantsciname)])){

      foo <- ecosite_species_sum %>% dplyr::filter(plantsciname == j)
      foo_list[[j]] <-
        data.frame(
          constancy = nrow(foo) * 100/length(unique(ecosite_species_sum$vegplotid)),
          avg_abundance = sum(foo$total_plot_cover) / length(unique(ecosite_species_sum$vegplotid)),
          max_abundance = max(foo$total_plot_cover),
          min_abundance = ifelse(nrow(foo) < length(unique(ecosite_species_sum$vegplotid)), 0, min(foo$total_plot_cover)),
          numb_plots_found = nrow(foo),
          numb_plots_not_found = length(unique(ecosite_species_sum$vegplotid)) - nrow(foo)
        )
    }

    # This aggregates the j loop lists into a single dataframe summarizing all the vegetation for the ecosite
    ecosite_list[[i]][["Cover_data"]] <- do.call(rbind, foo_list) %>% dplyr::arrange(desc(constancy))

    ## This nested loop separates ecosites into states/phases and summarizes vegetation by state/phase

    # Divide ecosites into states/phases
    for(j in ecosite_list[[i]][["Raw_data"]]$akfieldecositeid_edit[!is.na(ecosite_list[[i]][["Raw_data"]]$akfieldecositeid_edit)] %>% unique()){
      species_sum <- ecosite_list[[i]][["Raw_data"]] %>% dplyr::filter(akfieldecositeid_edit == j) %>%
        dplyr::group_by(vegplotid, plantsciname) %>% summarise(sum(akstratumcoverclasspct)) %>%
        dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct)`)

      #Clear goo_list
      goo_list <- list()

      # Loop through species, summarizing each
      for(g in unique(species_sum$plantsciname[!is.na(species_sum$plantsciname)])){

        foo <- species_sum %>% dplyr::filter(plantsciname == g)
        goo_list[[g]] <-
          data.frame(
            constancy = nrow(foo) * 100/length(unique(species_sum$vegplotid)),
            avg_abundance = sum(foo$total_plot_cover) / length(unique(species_sum$vegplotid)),
            max_abundance = max(foo$total_plot_cover),
            min_abundance = ifelse(nrow(foo) < length(unique(species_sum$vegplotid)), 0, min(foo$total_plot_cover)),
            numb_plots_found = nrow(foo),
            numb_plots_not_found = length(unique(species_sum$vegplotid)) - nrow(foo)
          )

      }

      #Assemble species summary lists
      ecosite_list[[i]][["STM"]][[j]] <- do.call(rbind, goo_list) %>% dplyr::arrange(desc(constancy))

    }

  }

  return(ecosite_list)

}
