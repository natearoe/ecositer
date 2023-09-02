#' Produce vegetation summary of ecosites and states/phases
#'
#' `veg_summary()` takes a *properly formatted vegetation dataframe* and produces summaries
#' of vegetation for ecosites and states/phases. Vegetation data originates from the
#' NASIS vegplot table. The \link[ecositer]{formatted_veg_df} function should be used to manipulate raw NASIS
#' vegplot data into a *properly formatted vegetation dataframe*
#'
#' `veg_summary()` function also runs an Indicator Species Analysis (ISA) using the \link[indicspecies]{multipatt} function.
#' Documentation for that function should be read to understand ISA. Briefly, ISA determines whether a species is a strong
#' indicator for a community. This is determined by multiplying the percent of the abundance of the species that occurs in
#' the community of interest (summed abundance in community of interest/summed abundance in all plots) by the the percent
#' of the plots within the community of interest where the species is present (# plots in community of interest where species
#' present/total number of plots assigned to community of interest). A strong indicator has the majority of its abundance
#' in the community of interest and appears in the majority of plots associated with the community of interest. ISA is used to
#' determine indicator species for ecosites and for states/phases.
#'
#' ISA interpretation notes: ISA determines the relationship between a community and every species that occured in that
#' community. It ultimately only reports a p-value for the species in the community it has the strongest relationship
#' with (lowest p-value.) Therefore, most ISA p-values are NA because the species is not most strongly
#' associated with that community. When a p-value is reported, that means the species is most strongly associated
#' with that community, even if the p-value is high. P-values are calculated using permutations of the data. ISA,
#' in the manner applied here, suffers dramatically from the fact that numerous statistical tests are being run.
#' When running multiple statistical tests, a modified critical p-value should be determined, using something like a
#' Bonferroni correction. For example, if 100 tests are run resulting in 100 different p-values, the critical p-value
#' would be adjusted to be 0.05/100 = .0005. In this situation, it is not uncommon for 1000s test, or more, to be
#' calculated. It is not reasonable to modify the critical p-value to that degree for several reasons. As a result,
#' ISA should be interpretted with care. It provides insight into the  relationship between species and sites/states/phases.
#' Ecologists should use expert knowledge along with the data provided in these summaries (ISA included) to determine what
#' species are most representative of an ecosite/state/phase.
#'
#'
#' This function takes several minutes due to the computation related to ISA.
#'
#' @param veg_df a properly formatted vegetation dataframe
#'
#' @return a nested list of vegetation summaries by ecosite and state/phase
#' @export
#'
#' @examples
#'
#' my_veg_summary <- veg_summary(veg_df = ecositer::vegetation_dataframe)
#' head(my_summary)
#'
#'
veg_summary <- function(veg_df){

  # This script starts with an Indicator Species Analysis (ISA). Then it begins
  #   using for loops to summarize species by ecosite as well as the states/phases
  #   within ecosites.

  #########################         ISA         #################################


  ############ Ecosite level ISA

  # filtering out missing data, grouping, summarizing, pivoting data wide
  IV_ecosite_df <- veg_df |> dplyr::filter_at(dplyr::vars(ecositeid, vegplotid, plantsciname), dplyr::all_vars(!is.na(.))) |>
    dplyr::group_by(ecositeid, vegplotid, plantsciname) |>
    dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`) |>
    tidyr::pivot_wider(values_from = total_plot_cover, names_from = plantsciname) |> dplyr::ungroup() |>
    dplyr::mutate_if(is.numeric, ~ tidyr::replace_na(., 0))  |> dplyr::select_if(~ !is.numeric(.) || sum(.) != 0)

  # run ISA for ecosite level
  IV_ecosite_results <- indicspecies::multipatt(IV_ecosite_df |> dplyr::select(-ecositeid, -vegplotid), cluster = IV_ecosite_df$ecositeid,
                                                func = "IndVal.g", duleg = TRUE, control = permute::how(nperm = 9))

  # several steps assembling ISA results into df with species, ecosite, and p.values
  IV_ecosite_results_df <- IV_ecosite_results$sign |> tibble::rownames_to_column("plantsciname")

  index_values <- colnames(IV_ecosite_results_df)[!colnames(IV_ecosite_results_df) %in%
                                                    c("plantsciname", "index", "stat", "p.value")] |> stringr::str_sub(start = 3L)

  ecosite_values <- unique(IV_ecosite_results_df$index) |> sort()

  index_values <- index_values[ecosite_values]


  IV_ecosite_results_df$ecosite <- plyr::mapvalues(IV_ecosite_results_df$index,
                                                   from = ecosite_values,
                                                   to = index_values)

  IV_ecosite_results_df <- IV_ecosite_results_df |> dplyr::select(c("plantsciname", "index", "stat", "p.value", "ecosite"))

  # put ISA results into a list by ecosite
  IV_ecosite_list <- list()

  for(i in IV_ecosite_results_df$ecosite){
    IV_ecosite_list[[i]] <- IV_ecosite_results_df |> dplyr::filter(ecosite == i)
  }

  ############ State/phase level ISA

  # States/phases are assigned in the field. In this script, we are using the
  #   akfieldecositeid for state/phase (in the future, I believe that the site >
  #   ecological site history table should have a column for state/phase, to serve
  #   as a more official place for state/phase to be assigned). The field ecosite
  #   call does not always match ecosites. For example, some field ecosites are
  #   assigned that do not end up ecosites at all. For this reason we start by
  #   determining if there is an associated ecosite to the field ecosite.

  veg_df$ecosite_from_state <-  sapply(veg_df$akfieldecositeid, FUN = function(x){
    field_eco_reduced <- x |> stringr::str_sub(start = 0L, end = 4L)
    ecosite_with_missings <- stringr::str_subset(unique(veg_df$ecositeid), pattern = field_eco_reduced)
    ifelse(length(ecosite_with_missings) == 0, NA,
           ecosite_with_missings)
  })

  # filter out missings, group_by, summarize, pivot_wider
  IV_state_df <- veg_df |> dplyr::filter_at(dplyr::vars(ecositeid, akfieldecositeid, vegplotid, plantsciname, ecosite_from_state), dplyr::all_vars(!is.na(.))) |>
    dplyr::group_by(ecositeid, akfieldecositeid, vegplotid, plantsciname) |>
    dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
    dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`) |>
    tidyr::pivot_wider(values_from = total_plot_cover, names_from = plantsciname) |> dplyr::ungroup() |>
    dplyr::mutate_if(is.numeric, ~ tidyr::replace_na(., 0))  |> dplyr::select_if(~ !is.numeric(.) || sum(.) != 0)


  # run ISA for state/phase level
  IV_state_results <- indicspecies::multipatt(IV_state_df |> dplyr::select(-akfieldecositeid, -vegplotid, -ecositeid), cluster = IV_state_df$akfieldecositeid,
                                              func = "IndVal.g", duleg = TRUE, control = permute::how(nperm = 99))

  # several steps assembling ISA results into df with species, ecosite, and p.values
  IV_state_results_df <- IV_state_results$sign |> tibble::rownames_to_column("plantsciname")


  index_values <- colnames(IV_state_results_df)[!colnames(IV_state_results_df) %in%
                                                  c("plantsciname", "index", "stat", "p.value")] |> stringr::str_sub(start = 3L)

  state_values <- unique(IV_state_results_df$index) |> sort()

  index_values <- index_values[state_values]

  IV_state_results_df$state <- plyr::mapvalues(IV_state_results_df$index,
                                               from = state_values,
                                               to = index_values)

  IV_state_results_df$ecosite <- sapply(IV_state_results_df$state, FUN = function(x){
    field_eco_reduced <- x |> stringr::str_sub(start = 0L, end = 4L)
    ecosite_with_missings <- stringr::str_subset(unique(veg_df$ecositeid), pattern = field_eco_reduced)
  })

  # put ISA results into a list by state
  IV_state_list <- list()

  for(i in unique(IV_state_results_df$state)){
    IV_state_list[[i]] <- IV_state_results_df |> dplyr::filter(state == i)
  }

  #########################    final prep before looping      ####################

  # initiate lists
  ecosite_list <- list()
  foo_list <- list()
  goo_list <- list()

  # change missing field ecosites to "undefined". This will be used for summaries, not for
  #   ISA.
  veg_df$akfieldecositeid <- ifelse(is.na(veg_df$akfieldecositeid), "undefined", veg_df$akfieldecositeid)

  #########################    begin loop through ecosite      ####################
  for(i in unique(veg_df$ecositeid[!is.na(veg_df$ecositeid)])){

    # print(i)

    # Subset ecosite ISA
    IV_ecosite <- if(i %in% names(IV_ecosite_list)){
      IV_ecosite_list[[i]]
    } else {
      data.frame(plantsciname = NA, index = NA, stat = NA,
                 p.value = NA, ecosite = NA)
    }

    # Create'ecosite_list' and a dataframe with all the raw veg data for each ecosite
    ecosite_list[[i]][["Raw_data"]] <- veg_df |> dplyr::filter(ecositeid == i)

    # Create placeholder sub-list
    ecosite_list[[i]][["STM"]] <- list()


    # Modify raw veg data to sum cover within species (e.g., if a single plant species is in multiple strata, its cover
    # will be summed across strata to give a single summed cover)

    # ecosite_species_sum is the dataframe that should be used for all the ecosite level calculations
    ecosite_species_sum <- veg_df |> dplyr::filter(ecositeid == i) |>
      dplyr::group_by(vegplotid, plantsciname) |> dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
      dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`)

    ecosite_list[[i]][["Cover_by_strata"]] <- veg_df |> dplyr::filter(ecositeid == i) |>
      dplyr::group_by(akstratumcoverclass) |> dplyr::summarise(avg = ifelse(sum(akstratumcoverclasspct) == 0, 0,
                                                                            sum(akstratumcoverclasspct, na.rm = TRUE)/length(vegplotiid)),
                                                               min = ifelse(min(akstratumcoverclasspct) == 0, 0,
                                                                            min(akstratumcoverclasspct, na.rm = TRUE)/length(vegplotiid)),
                                                               max = ifelse(max(akstratumcoverclasspct) == 0, 0,
                                                                            max(akstratumcoverclasspct, na.rm = TRUE)),
                                                               sd = ifelse(sd(akstratumcoverclasspct) == 0, 0,
                                                                           sd(akstratumcoverclasspct, na.rm = TRUE))
      ) |> dplyr::arrange(desc(avg))


    # Clear foo_list
    foo_list <- list()


    ########## Within ecosite loop, loop through plant species ################
    # This loop summarizes the vegetation data by ecosite
    for(j in unique(ecosite_species_sum$plantsciname[!is.na(ecosite_species_sum$plantsciname)])){

      # print(c(i, j))

      # Subset ecosite ISA by species
      IV_ecosite_species <- IV_ecosite |> dplyr::filter(plantsciname == j)

      # Calculations for reported values
      foo <- ecosite_species_sum |> dplyr::filter(plantsciname == j)
      foo_list[[j]] <-
        data.frame(
          constancy = nrow(foo) * 100/length(unique(ecosite_species_sum$vegplotid)),
          # Why subtract sum(foo$total_plot_cover == 0)? $total_plot_cover should be 0 where abundance was 0 because sum(NA, na.rm = TRUE) == 0 -
          # see summarise in the calculation of species_sum above.
          # Therefore, we are removing plots with NA abundance from averaging. Plots with NA abundance cannot increase the numerator in averaging
          # and should therefore not be used to influence the denominator either.
          # There could be a scenario where abundance was entered as 0. This functions the same as having an NA in abundance as it suggests
          # presence absence data collection.
          avg_abund = sum(foo$total_plot_cover, na.rm = TRUE) / (length(unique(ecosite_species_sum$vegplotid)) - sum(foo$total_plot_cover == 0)),
          twenty_percentile = quantile(c(foo$total_plot_cover, rep(0, times = length(unique(ecosite_species_sum$vegplotid)) -
                                                                     nrow(foo))), na.rm = TRUE, probs = 0.2),
          eighty_percentile = quantile(c(foo$total_plot_cover, rep(0, times = length(unique(ecosite_species_sum$vegplotid)) -
                                                                     nrow(foo))), na.rm = TRUE, probs = 0.8),
          median_abund = median(foo$total_plot_cover, na.rm = TRUE),
          max_abund = max(foo$total_plot_cover, na.rm = TRUE),
          min_abund = ifelse(nrow(foo) < length(unique(ecosite_species_sum$vegplotid)), 0, min(foo$total_plot_cover, na.rm = TRUE)),
          sum_abund = sum(foo$total_plot_cover, na.rm = TRUE),
          numb_plots_found = nrow(foo),
          numb_plots_not_found = length(unique(ecosite_species_sum$vegplotid)) - nrow(foo),
          perc_obs_pres_abs = sum(foo$total_plot_cover == 0)/nrow(foo),
          perc_obs_in_ecosite = nrow(foo)/(veg_df |> dplyr::filter(plantsciname == j) |> dplyr::pull(vegplotid) |>
                                             unique() |> length()),
          perc_abund_in_ecosite = sum(foo$total_plot_cover, na.rm = TRUE)/(veg_df |> dplyr::filter(plantsciname == j) |>
                                                                             dplyr::pull(akstratumcoverclasspct) |> sum(na.rm = TRUE))
        ) |> dplyr::mutate(importance = perc_obs_in_ecosite * perc_abund_in_ecosite,
                           ISA_p.value = ifelse(length(IV_ecosite_species$p.value) == 0, NA, IV_ecosite_species$p.value))
    }

    # This aggregates the j loop lists into a single dataframe summarizing all the vegetation for the ecosite
    ecosite_list[[i]][["Cover_data"]] <- do.call(rbind, foo_list) |> dplyr::arrange(desc(constancy))

    ########## Within ecosite loop, loop through field ecosite ################
    # This nested loop separates ecosites into states/phases and summarizes vegetation by state/phase

    # Divide ecosites into states/phases
    for(k in ecosite_list[[i]][["Raw_data"]]$akfieldecositeid[!is.na(ecosite_list[[i]][["Raw_data"]]$akfieldecositeid)] |> unique()){

      # print(c(i,k))

      # Subset ISA ecosite
      IV_state <- if(is.null(IV_state_list[[k]])){
        IV_state_list[[1]][0,]
      } else {IV_state_list[[k]]}

      # species sum is the dataframe for all the state/phase data
      species_sum <- ecosite_list[[i]][["Raw_data"]] |> dplyr::filter(akfieldecositeid == k) |>
        dplyr::group_by(vegplotid, plantsciname) |> dplyr::summarise(sum(akstratumcoverclasspct, na.rm = TRUE)) |>
        dplyr::rename(total_plot_cover = `sum(akstratumcoverclasspct, na.rm = TRUE)`)

      #Clear goo_list
      goo_list <- list()

      # Loop through species, summarizing each
      for(g in unique(species_sum$plantsciname[!is.na(species_sum$plantsciname)])){

        #print(c(i,k,g))

        # state/phase species ISA
        IV_state_species <- IV_state |> dplyr::filter(plantsciname == g)

        # Calculations for reported values
        foo <- species_sum |>  dplyr::filter(plantsciname == g)
        goo_list[[g]] <-
          data.frame(
            constancy = nrow(foo) * 100/length(unique(species_sum$vegplotid)),
            avg_abund = sum(foo$total_plot_cover, na.rm = TRUE) / (length(unique(species_sum$vegplotid)) - sum(foo$total_plot_cover == 0)),
            twenty_percentile = quantile(c(foo$total_plot_cover, rep(0, times = length(unique(species_sum$vegplotid)) -
                                                                       nrow(foo))) , na.rm = TRUE, probs = 0.2),
            eighty_percentile = quantile(c(foo$total_plot_cover, rep(0, times = length(unique(species_sum$vegplotid)) -
                                                                       nrow(foo))) , na.rm = TRUE, probs = 0.8),
            median_abund = median(foo$total_plot_cover, na.rm = TRUE),
            max_abund = max(foo$total_plot_cover, na.rm = TRUE),
            min_abund = ifelse(nrow(foo) < length(unique(species_sum$vegplotid)), 0, min(foo$total_plot_cover, na.rm = TRUE)),
            sum_abund = sum(foo$total_plot_cover, na.rm = TRUE),
            numb_plots_found = nrow(foo),
            numb_plots_not_found = length(unique(species_sum$vegplotid)) - nrow(foo),
            perc_obs_pres_abs = sum(foo$total_plot_cover == 0)/nrow(foo),
            perc_obs_in_ecosite = nrow(foo)/(veg_df |> dplyr::filter(plantsciname == k) |> dplyr::pull(vegplotid) |>
                                               unique() |> length()),
            perc_abund_in_ecosite = sum(foo$total_plot_cover, na.rm = TRUE)/(veg_df |> dplyr::filter(plantsciname == g) |>
                                                                               dplyr::pull(akstratumcoverclasspct) |> sum(na.rm = TRUE))
          ) |> dplyr::mutate(importance = median_abund * constancy,
                             ISA_p.value = ifelse(length(IV_state_species$p.value) == 0, NA, IV_state_species$p.value))

      }

      #Assemble species summary lists
      ecosite_list[[i]][["STM"]][[k]] <- do.call(rbind, goo_list) |> dplyr::arrange(desc(constancy))

    }

  }

  return(ecosite_list)

}
