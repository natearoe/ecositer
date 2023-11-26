



#' Big beta diversity
#'
#' `big_beta_diversity` calculates a distance matrix using the distance measure big beta diversity.
#'
#' @param veg_df a properly formatted vegetation dataframe (\link[ecositer]{formatted_veg_df}).
#' The data should be QCed to ensure that it meets minimum data requirements (\link[ecositer]{QC_vegplots})
#'
#' @return a distance matrix. This distance matrix can be used for ordination or to determine the similarity between
#' different ecological sites.
#' @export
#'
#' @examples
#' my_bbd <- big_beta_diversity(veg_df = my_veg_df)
big_beta_diversity <- function(veg_df){
  # R CMD check
  .SD <- NULL

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

  #.sd_veg_df <- vegan::wisconsin(.sd_veg_df)

  ### DATASET WIDE CORRELATION

  # Remove species in only one plot. Spearman correlation cannot be
  # calculated with only one record.
  #.sd_veg_df_dt <- data.table::data.table(.sd_veg_df)
  .sd_veg_df_m <- as.matrix(.sd_veg_df)
  #.sd_veg_df_dt <- data.table::data.table(vegan::wisconsin(.sd_veg_df_dt_cor))

  # Create and name grid of all species combinations.
  #comb_df <- data.table::data.table(expand.grid(colnames(.sd_veg_df_dt), colnames(.sd_veg_df_dt), stringsAsFactors = FALSE))
  comb_m_t <- t(as.matrix(expand.grid(colnames(.sd_veg_df_dt), colnames(.sd_veg_df_dt), stringsAsFactors = FALSE)))
  comb_m_t_t <- expand.grid(colnames(.sd_veg_df_dt), colnames(.sd_veg_df_dt), stringsAsFactors = FALSE) |> unlist()





## elapse = 76.05
# system.time(  for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   coeff <- 2*sum(pmin(.sd_veg_df_m[,sp_comb[[1]]],
#                       .sd_veg_df_m[,sp_comb[[2]]]))/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
#   data.frame(species1 = sp_comb[[1]], species2 = sp_comb[[2]], correlation = coeff)
# })

## elapse = 11.32, so building the dataframes takes 64 seconds....
# system.time(  for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   coeff <- 2*sum(pmin(.sd_veg_df_m[,sp_comb[[1]]],
#                       .sd_veg_df_m[,sp_comb[[2]]]))/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
# })

## elapse = 12.67. Saving to a list is fast.
# t1 <- Sys.time()
# my_list <- list()
# for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   my_list[[i]] <- 2*sum(pmin(.sd_veg_df_m[,sp_comb[[1]]],
#                       .sd_veg_df_m[,sp_comb[[2]]]))/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
# }
#
# test <- do.call("rbind", my_list)
#
# cbind(test, comb_df)
#
# t2 <- Sys.time()
# t2-t1

## find an Rcpp equivalent to pmin
# system.time(
#   for(i in seq(nrow(comb_df))){
#     sp_comb < comb_m_t[,i]
#     min(.sd_veg_df_m[,sp_comb[[1]]],
#              .sd_veg_df_m[,sp_comb[[2]]])
#   }
# )
#
#
# cbind(test, comb_df)
#
# t2 <- Sys.time()
# t2-t1

## 35% faster without pmin
# my_list <- list()
# system.time(  for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   my_list[[i]] <- 2*sum(.sd_veg_df_m[,sp_comb[[1]]],
#                              .sd_veg_df_m[,sp_comb[[2]]])/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
# })

# This uses the alternative Bray equation. It is the fastest option so far.
t1 <- Sys.time()

my_list <- list()
#sp_comb <- matrix(nrow = nrow(comb_m_t), ncol = ncol(comb_m_t))
for(i in seq(ncol(comb_m_t))){
  sp_comb <- comb_m_t[,i]
  my_list[[i]] <- 2*sum(abs(.sd_veg_df_m[,sp_comb[[1]]] -
                              .sd_veg_df_m[,sp_comb[[2]]]))/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
}

test <- do.call("rbind", my_list)

cbind(test, comb_df)

t2 <- Sys.time()
t2-t1

t1 <- Sys.time()

my_list <- list()
#sp_comb <- matrix(nrow = nrow(comb_m_t), ncol = ncol(comb_m_t))
for(i in seq(ncol(comb_m_t))){
  sp_comb <- comb_m_t[,i]
  p1 <- .sd_veg_df_m[,sp_comb[[1]]]
  p2 <- .sd_veg_df_m[,sp_comb[[2]]]
  my_list[[i]] <- 2*sum(abs(p1 - p2))/sum(p1, p2)
}

test <- do.call("rbind", my_list)

cbind(test, comb_df)

t2 <- Sys.time()
t2-t1

#########

t1 <- Sys.time()

my_list <- list()
#sp_comb <- matrix(nrow = nrow(comb_m_t), ncol = ncol(comb_m_t))
my_list <- list()
p1 <- matrix(nrow = nrow(.sd_veg_df_m), ncol = 1)
p2 <- matrix(nrow = nrow(.sd_veg_df_m), ncol = 1)
#sp_comb <- matrix(nrow = nrow(comb_m_t), ncol = ncol(comb_m_t))
for(i in seq(ncol(comb_m_t))){
  sp_comb <- comb_m_t[,i]
  p1 <- .sd_veg_df_m[,sp_comb[[1]]]
  p2 <- .sd_veg_df_m[,sp_comb[[2]]]
  my_list[[i]] <- 2*sum(abs(p1 - p2))/sum(p1, p2)
}

test <- do.call("rbind", my_list)

testy10 <- cbind(test, comb_df)

t2 <- Sys.time()
t2-t1

##########

for(i in seq(ncol(comb_m_t))){
  sp_comb <- comb_m_t[,i]
  p1 <- .sd_veg_df_m[,sp_comb[[1]]]
  p2 <- .sd_veg_df_m[,sp_comb[[2]]]
  my_list[[i]] <- 2*sum(abs(p1 - p2))/sum(p1, p2)
}

profvis({
  my_list <- list()
  p1 <- matrix(nrow = nrow(.sd_veg_df_m), ncol = 1)
  p2 <- matrix(nrow = nrow(.sd_veg_df_m), ncol = 1)
  #sp_comb <- matrix(nrow = nrow(comb_m_t), ncol = ncol(comb_m_t))
  for(i in seq(ncol(comb_m_t))){
    sp_comb <- comb_m_t[,i]
    p1 <- .sd_veg_df_m[,sp_comb[[1]]]
    p2 <- .sd_veg_df_m[,sp_comb[[2]]]
    my_list[[i]] <- 2*sum(abs(p1 - p2))/sum(p1, p2)
  }

  test <- do.call("rbind", my_list)

  cbind(test, comb_df)
})

test <- apply(.sd_veg_df_m, MARGIN = 2, FUN = function(x){
  sum(x)
})

test <- lapply(as.data.frame(comb_m_t), FUN = function(x){
  paste(x, collapse = ", ")
})
## lapply seems to be slower though I haven't experimented as much as for
# t1 <- Sys.time()
# df1 = lapply(comb_m_t |> as.data.frame(), function(x){
#   p1 <- .sd_veg_df_m[,x[[1]]]
#   p2 <- .sd_veg_df_m[,x[[2]]]
#   2*sum(abs(p1 - p2))/sum(p1, p2)
# })
# t2 <- Sys.time()
# t2-t1
#
# colnames(df1) = lapply(comb_m_t, function(x) paste(x[2], x[1], sep="-"))
# cbind(df,df1)



library(tidyverse)
f <- function(x, y) sum((x+y)^2)
.sd_veg_df_m <- data.frame(A=rnorm(100), B=rnorm(100), C=rnorm(100))

as.list(.sd_veg_df_m) %>%
  expand.grid(., .) %>%
  mutate(out = map2_dbl(Var1, Var2, f)) %>%
  as_tibble()


set.seed(42)
library(RcppAlgos)

test <- matrix(permuteGeneral(ncol(.sd_veg_df_m), 2, repetition = TRUE, FUN = function(y) {
  p1 <- .sd_veg_df_m[,y[1]]
  p2 <- .sd_veg_df_m[,y[2]]
  2*sum(abs(p1 - p2))/sum(p1, p2)
}), ncol = ncol(.sd_veg_df_m))



lapply(.sd_veg_df_m, FUN = function(x){
  sum(x)
})

mylisty <- list()
my_coeff <- combn(seq_along(.sd_df), 2, simplify = FALSE,
                  FUN = function(x){
                    p1 <- .sd_df[,x[1]]
                    p2 <- .sd_df[,x[2]]
                    2*sum(abs(p1 - p2))/sum(p1, p2)
                  })

test <- do.call("rbind", my_coeff)

testy9 <- cbind(test, comb_df)

combn(seq_along(.sd_df), 2, FUN = function(x) .sd_df[,x[1]]- .sd_df[,x[2]])

.sd_df <- as.data.frame(.sd_veg_df_m)

# system.time(  for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   my_list[[i]] <- 2*sum(abs(.sd_veg_df_m[,sp_comb[[1]]] -
#                               .sd_veg_df_m[,sp_comb[[2]]]))/(.sd_veg_df_m[,sp_comb[[1]]] + .sd_veg_df_m[,sp_comb[[2]]])
# })
#
# system.time(for(i in seq(nrow(comb_df))){
#   sp_comb <- comb_m_t[,i]
#   my_list[[i]] <- 2*sum(pmin(.sd_veg_df_m[,sp_comb[[1]]],
#                              .sd_veg_df_m[,sp_comb[[2]]]))/sum(.sd_veg_df_m[,sp_comb[[1]]], .sd_veg_df_m[,sp_comb[[2]]])
# }
# )



# elapse for pmin = 7.83
system.time(for(i in seq(nrow(comb_df))){
  pmin(  .sd_veg_df_m[, comb_m_t[,i][[1]]],
         .sd_veg_df_m[, comb_m_t[,i][[2]]])
})

# elapse for sum = 3.86
system.time(for(i in seq(nrow(comb_df))){
  sum(  .sd_veg_df_m[, comb_m_t[,i][[1]]],
         .sd_veg_df_m[, comb_m_t[,i][[2]]])
})

#faster
system.time(  for(i in seq(nrow(comb_df))){
  sp_comb <- comb_m_t[,i]
  sp1 <- .sd_veg_df_m[,sp_comb[[1]]]
  sp2 <- .sd_veg_df_m[,sp_comb[[2]]]
  coeff <- 2*sum(pmin(sp1,
                      sp2))/sum(sp1, sp2)
  data.frame(species1 = sp_comb[[1]], species2 = sp_comb[[2]], correlation = coeff)
})

  .NEWMATRIXFUN <- function(i){

  }




  .MATRIXTEST <- function(i){
    coeff <- 2*sum(pmin(.sd_veg_df_m[,comb_m[i,][[1]]],
                        .sd_veg_df_m[,comb_m[i,][[2]]]))/sum(.sd_veg_df_m[,comb_m[i,][[1]]], .sd_veg_df_m[,comb_m[i,][[2]]])
    data.frame(species1 = comb_m[i,][[1]], species2 = comb_m[i,][[2]], correlation = coeff)
  }

  comb_df[, .MATRIXTEST(.SD), by = list(seq(nrow(comb_m)))]

  # Calculate spearman correlation between each species combination.
  .BRAYSPECIES <- function(x) {
    coeff <- 2*sum(pmin(.sd_veg_df_dt[[x[[1]]]], .sd_veg_df_dt[[x[[2]]]]))/sum(.sd_veg_df_dt[[x[[1]]]], .sd_veg_df_dt[[x[[2]]]])
    data.frame(species1 = x[[1]], species2 = x[[2]], correlation = coeff)
  }

  system.time(comb_df[, .BRAYSPECIES(.SD), by = list(seq(nrow(comb_df)))])

  # run correlation
  my_cor_df <- comb_df[, .BRAYSPECIES(.SD), by = list(seq(nrow(comb_df)))][, -1] |>
    tidyr::pivot_wider(names_from = species2,
                       values_from = correlation) |>
    tibble::column_to_rownames("species1")

  ### CALCULATION OF SHARED ABUNDANCE

  # denominator multiplier
  denom_multiplier <- ncol(.sd_veg_df_dt) * 2

  # Determine all plot combinations
  plot_combs <- data.table::data.table(t(combn(seq(nrow(.sd_veg_df_dt)), 2)))











  .SHAREDABUND <- function(x) {

    # reduce dataset to pairwise plot comparison

    .sd_veg_df_dt_pw <- .sd_veg_df_dt[as.numeric(x), ]
    select_cols = names(which(colSums(.sd_veg_df_dt_pw > 0) > 0))
    .sd_veg_df_dt_pw <- .sd_veg_df_dt_pw[, ..select_cols]

    ## Begin calculation of BBD

    # Calculating denominator
    my_denom <- sum(denom_multiplier * .sd_veg_df_dt_pw)

    # Calculating numerator
    my_num <- NULL

    # Calculate the "outer minimum" of shared abundance matrix times 2
    res <- outer(t(as.matrix(.sd_veg_df_dt_pw[1, ])), t(as.matrix(.sd_veg_df_dt_pw[2, ])), pmin) * 2
    res <- res[seq(ncol(.sd_veg_df_dt_pw)), , seq(ncol(.sd_veg_df_dt_pw)), ]

    # Multiply by correlation matrix
    # NOTE: times 2 _again_; only doing half the comparisons...
    #       b/c we have symmetry in pairwise comps. Is it ever not symmetrical?
    #       Additional factor needed to match original code => "k" for-loop redundant?

    # positions of species in .sd_veg_df_dt_pw
    species_positions <- which(colnames(.sd_veg_df_dt) %in% colnames(.sd_veg_df_dt_pw))

    res <- res * as.matrix(my_cor_df)[species_positions, species_positions] * 2

    # bbd is the sum of shared abundance*correlation divided by
    # the sum of abundance scaled by number of species
    data.frame(plot1 = x[[1]],
               plot2 = x[[2]],
               bbd = 1 - sum(res) / my_denom)

  }

  # Iterate over plot combinations, calculate bbd (sum of minimum shared abundance * 2)
  my_bbd <- plot_combs[, .SHAREDABUND(.SD), by = seq(nrow(plot_combs))][, -1]

  siteiid_vegplotid <- data.frame(current = 1:nrow(veg_df),
                                  siteiid = as.integer(rownames(veg_df))) |>
    dplyr::left_join(veg_df |> dplyr::select(siteiid, vegplotid, ecositeid) |> unique())

  my_bbd$plot1 <- plyr::mapvalues(my_bbd$plot1,
                                 from = siteiid_vegplotid$current,
                                 to = siteiid_vegplotid$vegplotid)

  my_bbd$plot2 <- plyr::mapvalues(my_bbd$plot2,
                                 from = siteiid_vegplotid$current,
                                 to = siteiid_vegplotid$vegplotid)


  }

