#' Produce vegetation summary of ecosites and states/phases
#'
#' @param veg_df a properly formatted vegetation dataframe
#'
#' @return a nested list of vegetation summaries by ecosite and state/phase
#' @export
#'
#' @examples
#' #' my_veg_summary <- veg_summary(veg_df = ecositer::vegetation_dataframe)
#' head(my_veg_summary)
#'
veg_summary <- function(veg_df){

  # Convert to data.table
  veg_df <- data.table::as.data.table(veg_df)

  # If plant scientific name is NA, call it unknown
  veg_df$plantsciname[is.na(veg_df$plantsciname)] <- "undefined"

  # If strata is NA, call it unknown
  veg_df$akstratumcoverclass[is.na(veg_df$akstratumcoverclass)] <- "undefined"

  # If strata is NA, call it unknown
  veg_df$akfieldecositeid[is.na(veg_df$akfieldecositeid)] <- "undefined"

  # remove instances where ecosite, vegplotid, or akstratumcoverclasspct
  veg_df <- veg_df |> dplyr::filter(!is.na(ecositeid) & !is.na(vegplotid) & !is.na(akstratumcoverclasspct))


  # aggregate plant abundances. this means that if the same plot has two records of the same species in
  # different strata, they will be combined. then pivot data wider. this data.table is what veg
  # summaries are derived from
  ecosite_species_sum <- veg_df |>
    dplyr::group_by(vegplotid, akfieldecositeid, ecositeid, plantsciname) |> dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`) |>
    tidyr::pivot_wider(names_from = plantsciname,
                       values_from = total_plot_cover) |> data.table::as.data.table()

  # replace NAs with zero
  ecosite_species_sum[is.na(ecosite_species_sum)] <- 0

  # aggregate strata. this means that if the same plot has multiple records of the same strata,
  # they will be combined. then pivot data wider. this data.table is what strata
  # summaries are derived from
  ecosite_strata_sum <- veg_df |>
    dplyr::group_by(vegplotid, akfieldecositeid, ecositeid, akstratumcoverclass) |> dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    dplyr::rename(plot_strata_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`) |>
    tidyr::pivot_wider(names_from = akstratumcoverclass,
                       values_from = plot_strata_cover) |> data.table::as.data.table()

  # replace NAs with zero
  ecosite_strata_sum[is.na(ecosite_strata_sum)] <- 0

  # calculate study-wide statistics
  species_stats <- sapply(ecosite_species_sum[,-c(1,2,3)], FUN = function(x){
    c(sum(x > 0, na.rm = TRUE),
      sum(x, na.rm = TRUE))
  }) |> as.data.frame()

  # create empty list
  ecosite_list <- list()

  # loop through ecosites
  for(i in unique(veg_df$ecositeid[!is.na(veg_df$ecositeid)])){

    # reduce data to ecosite of interest
    ecosite_list[[i]][["Raw_data"]] <- veg_df[veg_df$ecositeid == i, ]

    # reduce aggregated strata data to ecosite
    ecosite_strata_sum_i <- ecosite_strata_sum[ecosite_strata_sum$ecositeid == i, ]

    # remove strata with no occurrences
    ecosite_strata_sum_ix <- ecosite_strata_sum_i[, colSums(ecosite_strata_sum_i != 0) > 0, with = FALSE]

    # calculate various statistics by strata
    ecosite_strata_sum_ix_results <- sapply(ecosite_strata_sum_ix[,-c(1,2,3)], FUN = function(x){
      c(length(x))
    }) |> t()

    # calculate various statistics by strata
    ecosite_strata_sum_ix_results <- sapply(ecosite_strata_sum_ix[,-c(1,2,3)], FUN = function(x){
      c(100*sum(x > 0, na.rm = TRUE)/nrow(ecosite_strata_sum_ix),
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        sum(x, na.rm = TRUE),
        quantile(x, probs = 0.2, na.rm = TRUE, names = FALSE),
        quantile(x, probs = 0.8, na.rm = TRUE, names = FALSE),
        sum(x > 0, na.rm = TRUE),
        sum(x == 0, na.rm = TRUE)
      )
    }) |> t()

    # add column names
    colnames(ecosite_strata_sum_ix_results) <- c("constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                                                 "sites_present", "sites_absent")

    # order results by constancy
    ecosite_strata_sum_ix_results <- ecosite_strata_sum_ix_results[order(ecosite_strata_sum_ix_results[,"constancy"], decreasing = TRUE),]

    # put results into list
    ecosite_list[[i]][["Cover_by_strata"]] <- ecosite_strata_sum_ix_results

    # reduce aggregated veg data to ecosite
    ecosite_species_sum_i <- ecosite_species_sum[ecosite_species_sum$ecositeid == i, ]

    # remove species with no occurences
    ecosite_species_sum_ix <- ecosite_species_sum_i[, colSums(ecosite_species_sum_i != 0) > 0, with = FALSE]

    # calculate various statistics by species
    ecosite_veg_sum <- sapply(ecosite_species_sum_ix[,-c(1,2,3)], FUN = function(x){
      c(100*sum(x > 0, na.rm = TRUE)/nrow(ecosite_species_sum_ix),
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        sum(x, na.rm = TRUE),
        quantile(x, probs = 0.2, na.rm = TRUE, names = FALSE),
        quantile(x, probs = 0.8, na.rm = TRUE, names = FALSE),
        sum(x > 0, na.rm = TRUE),
        sum(x == 0, na.rm = TRUE)
      )
    }) |> t()

    # reduce study-wide statistics to species of interest
    species_stats_i <- species_stats[, row.names(ecosite_veg_sum)] |> t()

    # calculate using ecosite statistics & study-wide statistics where necessary
    perc_obs_in_ecosite <- 100*ecosite_veg_sum[,9] |> array() / species_stats_i[,1] |> array()
    perc_abund_in_ecosite <- 100*ecosite_veg_sum[,6] |> array()/species_stats_i[,2] |> array()

    # add those columns
    ecosite_veg_sum <- cbind(ecosite_veg_sum, perc_obs_in_ecosite, perc_abund_in_ecosite)

    # add column names
    colnames(ecosite_veg_sum) <- c("constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                                   "sites_present", "sites_absent", "perc_obs_in_ecosite", "perc_abund_in_ecosite")

    # order results by constancy
    ecosite_veg_sum <- ecosite_veg_sum[order(ecosite_veg_sum[,"constancy"], decreasing = TRUE),]

    # put results into list
    ecosite_list[[i]][["Summary"]] <- ecosite_veg_sum

    # create empty list
    ecosite_list[[i]][["STM"]] <- list()

    # loop through akfieldecositeids (AKA state) within ecosite
    for(j in ecosite_list[[i]][["Raw_data"]]$akfieldecositeid[!is.na(ecosite_list[[i]][["Raw_data"]]$akfieldecositeid)] |> unique()){

      # create empty list
      ecosite_list[[i]][["STM"]][[j]] <- list()

      # reduce raw data to state
      ecosite_list[[i]][["STM"]][[j]][["Raw_data"]] <- ecosite_list[[i]][["Raw_data"]] |> dplyr::filter(akfieldecositeid == j)

      # reduce aggregated veg data to ecosite and state
      ecosite_strata_sum_i_state <- ecosite_strata_sum[ecosite_strata_sum$ecositeid == i &
                                                         ecosite_strata_sum$akfieldecositeid == j, ]

      # remove absent strata
      ecosite_strata_sum_ix_state <- ecosite_strata_sum_i_state[, colSums(ecosite_strata_sum_i_state != 0) > 0, with = FALSE]

      # calculate various statistics by strata
      ecosite_strata_sum_state <- sapply(ecosite_strata_sum_ix_state[,-c(1,2,3)], FUN = function(x){
        c(100*sum(x > 0, na.rm = TRUE)/nrow(ecosite_strata_sum_ix_state),
          mean(x, na.rm = TRUE),
          median(x, na.rm = TRUE),
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE),
          sum(x, na.rm = TRUE),
          quantile(x, probs = 0.2, na.rm = TRUE, names = FALSE),
          quantile(x, probs = 0.8, na.rm = TRUE, names = FALSE),
          sum(x > 0, na.rm = TRUE),
          sum(x == 0, na.rm = TRUE)
        )
      }) |> t()

      # add column names
      colnames(ecosite_strata_sum_state) <- c("constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                                              "sites_present", "sites_absent")

      # sort by constancy
      ecosite_strata_sum_state <- ecosite_strata_sum_state[order(ecosite_strata_sum_state[,"constancy"], decreasing = TRUE),]

      # put result in list
      ecosite_list[[i]][["STM"]][[j]][["Cover_by_strata"]] <- ecosite_strata_sum_state

      # # summarize strata in state
      # ecosite_list[[i]][["STM"]][[j]][["Cover_by_strata"]] <- ecosite_list[[i]][["STM"]][[j]][["Raw_data"]] |>
      #   dplyr::group_by(akstratumcoverclass) |> dplyr::summarise(avg = mean(akstratumcoverclasspct, na.rm = TRUE),
      #                                                            median = median(akstratumcoverclasspct, na.rm = TRUE),
      #                                                            min = min(akstratumcoverclasspct, na.rm = TRUE),
      #                                                            max = max(akstratumcoverclasspct, na.rm = TRUE),
      #                                                            species_strata = dplyr::n()) |> dplyr::arrange(desc(avg))

      # reduce aggregated veg data to ecosite and state
      ecosite_species_sum_i_state <- ecosite_species_sum[ecosite_species_sum$ecositeid == i &
                                                           ecosite_species_sum$akfieldecositeid == j, ]

      # remove absent species
      ecosite_species_sum_ix_state <- ecosite_species_sum_i_state[, colSums(ecosite_species_sum_i_state != 0) > 0, with = FALSE]

      # calculate various statistics by species
      ecosite_veg_sum_state <- sapply(ecosite_species_sum_ix_state[,-c(1,2,3)], FUN = function(x){
        c(100*sum(x > 0, na.rm = TRUE)/nrow(ecosite_species_sum_ix_state),
          mean(x, na.rm = TRUE),
          median(x, na.rm = TRUE),
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE),
          sum(x, na.rm = TRUE),
          quantile(x, probs = 0.2, na.rm = TRUE, names = FALSE),
          quantile(x, probs = 0.8, na.rm = TRUE, names = FALSE),
          sum(x > 0, na.rm = TRUE),
          sum(x == 0, na.rm = TRUE)
        )
      }) |> t()

      # reduce study-wide stats to species of interest
      species_stats_i_state <- species_stats[, row.names(ecosite_veg_sum_state)] |> t()

      # calculate using state statistics & study-wide statistics where necessary
      perc_obs_in_ecosite <- 100*ecosite_veg_sum_state[,9] |> array() / species_stats_i_state[,1] |> array()
      perc_abund_in_ecosite <- 100*ecosite_veg_sum_state[,6] |> array()/species_stats_i_state[,2] |> array()

      # add columns
      ecosite_veg_sum_state <- cbind(ecosite_veg_sum_state, perc_obs_in_ecosite, perc_abund_in_ecosite)


      # add column names
      colnames(ecosite_veg_sum_state) <- c("constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                                           "sites_present", "sites_absent", "perc_obs_in_ecosite", "perc_abund_in_ecosite")

      # sort by constancy
      ecosite_veg_sum_state <- ecosite_veg_sum_state[order(ecosite_veg_sum_state[,"constancy"], decreasing = TRUE),]

      # put result in list
      ecosite_list[[i]][["STM"]][[j]][["Summary"]] <- ecosite_veg_sum_state

    }

  }

  return(ecosite_list)

}

