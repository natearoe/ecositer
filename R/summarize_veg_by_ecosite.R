#' Summarize vegetation data by ecosite, state, and community phase
#'
#' @param veg_df vegetation dataframe
#' @param agg_abund logical - aggregate abundance columns using `ecositer::QC_aggregate_abundance()`
#' @param update_taxonomy logical - update taxonomy from USDA PLANTS using `ecositer::QC_update_taxonomy()`,
#' @param best_vegplot logical - use best vegplot when multiple for site using `ecositer::QC_best_vegplot_for_site()`,
#' @param use_id logical - use state and phase id (i.e., numeric)? Alternatively, state and phase names are used
#'
#' @return list of summarized vegetation data
#' @export
#'
#' @examples
summarize_veg_by_ecosite <- function(veg_df,
                           agg_abund = TRUE,
                           update_taxonomy = TRUE,
                           best_vegplot = TRUE,
                           use_id = TRUE){

  # convert to data.table
  veg_df <- data.table::as.data.table(veg_df)

  # apply QC fxns if requested
  if(agg_abund){
    veg_df <- ecositer::QC_aggregate_abundance(veg_df)
  }

  if(best_vegplot){
    veg_df <- ecositer::QC_best_vegplot_for_site(veg_df)
  }

  if(update_taxonomy){
    veg_df <- ecositer::QC_update_taxonomy(veg_df)
  }

  # check for multiple statenames per stateid
  multi_state_name_per_stateid <- veg_df[, data.table::uniqueN(ecostatename) > 1, by = c("ecositeid", "ecostateid")][,any(V1)]

  # check for multiple phasenames per phaseid
  multi_phase_name_per_phaseid <- veg_df[, data.table::uniqueN(commphasename) > 1, by = c("ecositeid", "commphaseid")][,any(V1)]

  # provide warning for mult statename per stateid
  if(multi_state_name_per_stateid){
    message("Warning: There are multiple state names associated with the same stateid. To identify these situations, use:
          `Your veg df` |>
    dplyr::group_by(ecositeid, ecostateid) |>
    dplyr::filter(dplyr::n_distinct(ecostatename) > 1) |>
    dplyr::distinct(ecositeid, ecostateid, ecostatename) |>
    dplyr::arrange(ecositeid, ecostateid, ecostatename)")
  }



  # provide warning for mult phasename per phaseid
  if(multi_phase_name_per_phaseid){
    message("Warning: There are multiple phase names associated with the same phaseid. To identify these situations, use:
            `Your veg df` |>
    dplyr::group_by(ecositeid, ecostateid, commphaseid) |>
    dplyr::filter(dplyr::n_distinct(commphasename) > 1) |>
    dplyr::distinct(ecositeid, ecostateid, commphaseid, commphasename) |>
    dplyr::arrange(ecositeid, ecostateid, commphaseid, commphasename)")
  }

  # note number of sites missing ecosite and remove those sites
  if(anyNA(veg_df$ecositeid)){
    sites_missing_ecositeid <- veg_df |> dplyr::group_by(siteiid) |>
      dplyr::summarise(anyNA(ecositeid)) |> dplyr::filter(`anyNA(ecositeid)` == TRUE)
    numb_sites_missing_ecositeid <- sum(sites_missing_ecositeid$`anyNA(ecositeid)`)
    message(sprintf("Warning: %s sites missing correlation to ecositeid. These sites are removed from results.", numb_sites_missing_ecositeid))
    veg_df <- veg_df |> dplyr::filter(!siteiid %in% sites_missing_ecositeid$siteiid)
  }

  # note number of sites missing vegplotiid and remove those
  if(anyNA(veg_df$vegplotiid)){
    sites_missing_vegplotiid <- veg_df |> dplyr::filter(is.na(vegplotiid))
    message(sprintf("Warning: %s sites have no associated vegplot. These sites are removed from results.", nrow(sites_missing_vegplotiid)))
    veg_df <- veg_df |> dplyr::filter(!siteiid %in% sites_missing_vegplotiid$siteiid)
  }

  # note number of records missing abundance and remove them
  if(anyNA(veg_df$pct_cover)){
    records_missing_cover <- veg_df |> dplyr::filter(is.na(pct_cover))
    message(sprintf("Warning: %s records missing abundance. These records are removed from results.", nrow(records_missing_cover)))
    veg_df <- veg_df |> dplyr::filter(!is.na(pct_cover))
  }

  # note the number of species missing taxonomies
  if(anyNA(veg_df$plantsym)){
    numb_records_missing <- veg_df |> dplyr::filter(is.na(plantsym)) |> nrow()
    message(sprintf("Warning: %s records missing taxonomical assignment. `unknown` will be populated in the plantsym column.", numb_records_missing))
    veg_df$plantsym[is.na(veg_df$plantsym)] <- "unknown"
  }

  # aggregate plant abundances, this means that if the same plot has two records of the same species
  # (presumably from different strata - NASIS strata will be introduced into this process later),
  # they will be combined.

  # grouping columns
  cols_keep <- c("siteiid", "usiteid", "siteobsiid", "vegplotid", "vegplotiid", "primarydatacollector", "vegdataorigin", "ecositeid", "ecositenm", "ecostateid", "ecostatename", "commphaseid", "commphasename")

  # aggregate abundances when the same species listed multiple times in vegplotiid (this occurs when the same species occurs in multiple strata)
  veg_df_c <- merge(x = veg_df[, ..cols_keep] |> unique(),
        y = veg_df[, .(pct_cover = sum(pct_cover, na.rm = TRUE)),
                   by = .(vegplotiid, plantsym)])

  # pivot abundance data wider
  veg_df_w <- data.table::dcast(veg_df_c,
                                siteiid + usiteid + siteobsiid + vegplotid + vegplotiid + primarydatacollector + vegdataorigin + ecositeid + ecositenm + ecostateid + ecostatename + commphaseid + commphasename ~ plantsym,
                                value.var = "pct_cover")

  # column names of species columns
  species_cols <- colnames(veg_df_w)[!colnames(veg_df_w) %in% cols_keep]

  # make NA abundance 0 these are the result of pivoting wider. actual NA abundances were already removed.
  veg_df_w[, (species_cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = species_cols]

  # data.table of plantsym and plantsciname
  plant_sym_sci <- veg_df[, c("plantsym", "plantsciname", "plantnatvernm")] |> unique()

  # define column order
  col_order <- c("plantsym", "plantsciname", "plantnatvernm", "constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                 "sites_present", "sites_absent")

  # create empty list
  ecosite_list <- list()

  # loop through ecositeids - not NAs
  for(i in unique(veg_df_c$ecositeid[!is.na(veg_df_c$ecositeid)])){

    # reduce long data to ecosite of interest
    site_veg <- veg_df_c[veg_df_c$ecositeid == i,]

    # assign that ecosite to "raw_data"
    ecosite_list[[i]][["raw_data"]] <- site_veg

    # reduce wide data to ecosite of interest
    site_veg_w <- veg_df_w[veg_df_w$ecositeid == i,]

    sum_df <- sapply(site_veg_w[, ..species_cols], FUN = function(x){
      c(100*sum(x > 0, na.rm = TRUE)/nrow(site_veg_w),
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
    }) |>  data.table::as.data.table() |> data.table::transpose(keep.names = "row_names")

    # add column names
    colnames(sum_df) <- c("plantsym","constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                                                 "sites_present", "sites_absent")

    # merge in the plantsciname and plantnatvernm
    sum_df <- merge(x = sum_df, y = plant_sym_sci)

    # rearrange order
    data.table::setcolorder(sum_df, col_order)

    # order by constancy
    data.table::setorder(sum_df, -constancy)

    # set sum_df to list
    ecosite_list[[i]][["species_summary"]] <- sum_df

    for(j in ecosite_list[[i]][["raw_data"]]$ecostateid[!is.na(ecosite_list[[i]][["raw_data"]]$ecostateid)] |> unique()){

      state_veg <- site_veg[ecostateid == j,]

      # reduce to ecosite of interest
      ecosite_list[[i]][["STM"]][[paste0("state", j)]][["raw_data"]] <- state_veg

      # reduce wide data to state of interest
      state_veg_w <- site_veg_w[site_veg_w$ecostateid == j,]

      sum_df <- sapply(state_veg_w[, ..species_cols], FUN = function(x){
        c(100*sum(x > 0, na.rm = TRUE)/nrow(state_veg_w),
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
      }) |>  data.table::as.data.table() |> data.table::transpose(keep.names = "row_names")

      # add column names
      colnames(sum_df) <- c("plantsym","constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                            "sites_present", "sites_absent")

      # merge in the plantsciname and plantnatvernm
      sum_df <- merge(x = sum_df, y = plant_sym_sci)

      # rearrange order
      data.table::setcolorder(sum_df, col_order)

      # order by constancy
      data.table::setorder(sum_df, -constancy)

      # set sum_df to list
      ecosite_list[[i]][["STM"]][[paste0("state", j)]][["species_summary"]] <- sum_df

        for(k in  ecosite_list[[i]][["STM"]][[paste0("state", j)]][["raw_data"]]$commphaseid[!is.na(ecosite_list[[i]][["STM"]][[paste0("state", j)]][["raw_data"]]$commphaseid)] |> unique()){

        comm_veg <- site_veg[commphaseid == k,]

        # reduce to ecosite of interest
        ecosite_list[[i]][["STM"]][[paste0("state", j)]][[paste0("comm", k)]][["raw_data"]] <- state_veg

        # reduce wide data to state of interest
        comm_veg_w <- state_veg_w[state_veg_w$commphaseid == k,]

        sum_df <- sapply(comm_veg_w[, ..species_cols], FUN = function(x){
          c(100*sum(x > 0, na.rm = TRUE)/nrow(comm_veg_w),
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
        }) |>  data.table::as.data.table() |> data.table::transpose(keep.names = "row_names")

        # add column names
        colnames(sum_df) <- c("plantsym","constancy", "mean", "median", "min", "max", "sum", "20th", "80th",
                              "sites_present", "sites_absent")

        # merge in the plantsciname and plantnatvernm
        sum_df <- merge(x = sum_df, y = plant_sym_sci)

        # rearrange order
        data.table::setcolorder(sum_df, col_order)

        # order by constancy
        data.table::setorder(sum_df, -constancy)

        # set sum_df to list
        ecosite_list[[i]][["STM"]][[paste0("state", j)]][[paste0("comm", k)]][["species_summary"]] <- sum_df

      }

    }

  }

return(ecosite_list)


}
