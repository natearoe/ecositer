#' Update taxonomy according to USDA PLANTS
#'
#' @param veg_df
#'
#' @return vegetation dataframe with updated taxonomy
#' @export
#'
#' @examples
#' ecositer::QC_update_taxonomy(veg_df = CA792_veg)
QC_update_taxonomy <- function(veg_df){

  zip_url <- "https://github.com/natearoe/ecositer_data/blob/main/PLANTS_2024_official.zip?raw=TRUE"


  USDA_plants <- data.table::fread(zip_url, showProgress = FALSE)

  USDA_cols <- colnames(USDA_plants)
  USDA_cols_ <- gsub(" ", "_", USDA_cols)
  colnames(USDA_plants) <- USDA_cols_

  veg_df[veg_df == ""] <- NA

  veg_df_tax <- veg_df |> dplyr::left_join(USDA_plants |>
                                       dplyr::select(Synonym_Symbol, Accepted_Symbol),
                                     dplyr::join_by(plantsym == Synonym_Symbol)) |>
    dplyr::left_join(USDA_plants |> dplyr::select(Symbol, Scientific_Name, Common_Name),
                     dplyr::join_by(Accepted_Symbol == Symbol))

  tax_change <- veg_df_tax |> dplyr::select(Scientific_Name, plantsciname) |>
    dplyr::filter(!is.na(Scientific_Name) & !is.na(plantsciname)) |> unique()

  if(nrow(tax_change) > 0){
    message("Note -> The following taxonomical changes have been made.")
    message(paste(tax_change$plantsciname, "changed to", tax_change$Scientific_Name, "\n"))
  }

  if(nrow(tax_change) == 0){
    message("Note -> All taxonomies up-to-date.")
  }

  veg_df_tax$plantsym <- ifelse(is.na(veg_df_tax$Accepted_Symbol), veg_df_tax$plantsym, veg_df_tax$Accepted_Symbol)
  veg_df_tax$plantsciname <- ifelse(is.na(veg_df_tax$Scientific_Name), veg_df_tax$plantsciname, veg_df_tax$Scientific_Name)
  veg_df_tax$plantnatvernm <- ifelse(is.na(veg_df_tax$Accepted_Symbol), veg_df_tax$plantnatvernm, veg_df_tax$Common_Name)

  veg_df_tax$Accepted_Symbol <- NULL
  veg_df_tax$Scientific_Name <- NULL
  veg_df_tax$Common_Name <- NULL

  veg_df_tax

}
