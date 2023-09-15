nmds_taxon_comparison <- function(veg_df, taxon){

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

  # remove plots that are poorly sampled
  test <- veg_df |>
    dplyr::group_by(siteiid) |>
    dplyr::summarise(species_richness = dplyr::n()) |>
    as.data.frame()





}
