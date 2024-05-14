#' Big beta diversity
#'
#' `big_beta_diversity` calculates a distance matrix using the distance measure, big beta diversity.
#'
#' @param veg_df a properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df}).
#' The data should be QCed to ensure that it meets minimum data requirements (\link[ecositer]{QC_vegplots})
#' @param remove_rare decimal value defining the fraction of sites a species must be present in to be included
#' (e.g., 0.01 means a species must be present in > 1% of plots). FALSE to include all species in dataset.
#' @param relative_dist relativized 0-1 dissimilarity values using: \deqn{z_i = \frac{x_i - min(x)}{max(x) - min(x)}}
#' @return a distance matrix. This distance matrix can be used for ordination or to determine the similarity between
#' different ecological sites.
#' @export
#'
#' @examples
#' my_bbd <- big_beta_diversity(veg_df = my_veg_df)
big_beta_diversity <- function(veg_df, wisconsin = FALSE, remove_rare = FALSE,
                               relative_dist = TRUE){

  # manipulating data for process
  .sd_veg_df <- veg_df |>
    dplyr::select(siteiid, plantsciname, akstratumcoverclasspct) |>
    dplyr::group_by(siteiid, plantsciname) |>
    dplyr::summarise(abund = sum(akstratumcoverclasspct)) |>
    tidyr::pivot_wider(names_from = plantsciname,
                       values_from = abund) |>
    tibble::column_to_rownames("siteiid") |>
    dplyr::select(dplyr::contains(" "))

  .sd_veg_df[is.na(.sd_veg_df)] <- 0
  .sd_veg_df <- .sd_veg_df[,colSums(.sd_veg_df) > 0]

  if(wisconsin){
    .sd_veg_df <- vegan::wisconsin(.sd_veg_df)
  }

  if(remove_rare != FALSE){
    .sd_veg_df <- .sd_veg_df[, colSums(.sd_veg_df > 0) > nrow(.sd_veg_df) * remove_rare]
  }

  no_abund <- .sd_veg_df[rowSums(.sd_veg_df) <= 0, ] |> row.names()

  if(length(no_abund) != 0){
    warning("The following plots have no abundance and have been removed: ",
            paste(no_abund, collapse = ", "))
  }

  .sd_veg_df <- .sd_veg_df[rowSums(.sd_veg_df)>0, ]

  ### DATASET WIDE CORRELATION

  # Remove species in only one plot. Spearman correlation cannot be
  # calculated with only one record.
  .sd_veg_df_m <- as.matrix(.sd_veg_df)

  # Create and name grid of all species combinations.
  comb_m_t <- t(RcppAlgos::comboGrid(colnames(.sd_veg_df_m), colnames(.sd_veg_df_m)))
  comb_m <- t(comb_m_t)

  my_list <- list()
  for(i in seq(ncol(comb_m_t))){
    sp_comb <- comb_m_t[,i]
    p1 <- .sd_veg_df_m[,sp_comb[[1]]]
    p2 <- .sd_veg_df_m[,sp_comb[[2]]]
    my_list[[i]] <- 1-sum(abs(p1 - p2))/sum(p1, p2)
  }

  bray_values <- do.call("rbind", my_list)
  bray_values <- round(bray_values, digits = 15)
  m1 <- matrix(, ncol(.sd_veg_df_m), ncol(.sd_veg_df_m))
  m1[lower.tri(m1, diag=TRUE)] <- bray_values
  bray_matrix <- t(m1)
  bray_matrix[lower.tri(bray_matrix, diag=TRUE)] <- bray_values
  colnames(bray_matrix) <- colnames(.sd_veg_df_m)

  ### CALCULATION OF SHARED ABUNDANCE

  # denominator multiplier
  denom_multiplier <- ncol(.sd_veg_df_m) * 2

  # Determine all plot combinations
  plot_combs <- t(RcppAlgos::comboGrid(seq(nrow(.sd_veg_df_m)),
                                       seq(nrow(.sd_veg_df_m))))

  # transpose for efficiency
  .sd_veg_df_m_t <- t(.sd_veg_df_m)

  bbd_values <- list()
  for(i in seq(ncol(plot_combs))){
    # reduce to plot comparison of interest
    .sd_plots <- .sd_veg_df_m_t[,plot_combs[,i]]

    # remove species absent in both plots
    .sd_plots <- .sd_plots[rowSums(.sd_plots > 0) > 0, , drop = FALSE]

    # remove column names - this is important when you have a one row
    # matrix because of row/column naming in the outer function.
    # see - https://stackoverflow.com/questions/78462981/subset-a-single-row-matrix-array-and-return-row-names/78463350#78463350
    colnames(.sd_plots) <- NULL

    # calculate denominator
    my_denom <- sum(denom_multiplier * .sd_plots)

    sp_comp <- outer(.sd_plots[, 1], .sd_plots[, 2], pmin)

    # reduce coeffs down to those of interest
    species_positions <- which(colnames(bray_matrix) %in% colnames(sp_comp))

    # multiply species comparison by coeffs
    res <- sp_comp * bray_matrix[species_positions, species_positions] * 2

    # perform bray equation
    bbd_values[[i]] = 1 - sum(res) / my_denom

  }

  # put results into a matrix
  bbd_values <- do.call("rbind", bbd_values)
  bbd_values <- round(bbd_values, digits = 15)
  bbd1 <- matrix(, nrow(.sd_veg_df_m), nrow(.sd_veg_df_m))
  bbd1[lower.tri(bbd1, diag=TRUE)] <- bbd_values
  bbd_matrix <- t(bbd1)
  bbd_matrix[lower.tri(bbd_matrix, diag=TRUE)] <- bbd_values
  colnames(bbd_matrix) <- rownames(.sd_veg_df_m)
  rownames(bbd_matrix) <- colnames(bbd_matrix)

  # renaming sites: this part is only applicable to NRCS ecosites
  siteiid_vegplotid <- data.frame(siteiid = as.character(veg_df$siteiid),
                                  vegplotid = veg_df$vegplotid) |>
    unique()

  colnames_df <- data.frame(siteiid = colnames(bbd_matrix)) |>
    dplyr::left_join(siteiid_vegplotid)

  colnames(bbd_matrix) <- colnames_df$vegplotid
  rownames(bbd_matrix) <- colnames(bbd_matrix)

  if(relative_dist == TRUE){
    bbd_matrix <- (bbd_matrix - min(bbd_matrix))/(max(bbd_matrix) - min(bbd_matrix))
  }

  return(bbd_matrix)

  }

