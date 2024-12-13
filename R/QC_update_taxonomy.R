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


  USDA_plants <- data.table::fread(zip_url)

  USDA_cols <- colnames(USDA_plants)
  USDA_cols_ <- gsub(" ", "_", USDA_cols)
  colnames(USDA_plants) <- USDA_cols_

  veg_df_tax <- veg_df |> dplyr::left_join(USDA_plants |>
                                       dplyr::select(Synonym_Symbol, Accepted_Symbol),
                                     dplyr::join_by(plantsym == Synonym_Symbol)) |>
    dplyr::left_join(USDA_plants |> dplyr::select(Symbol, Scientific_Name, Common_Name),
                     dplyr::join_by(Accepted_Symbol == Symbol))

  tax_change <- veg_df_tax |> dplyr::select(Scientific_Name, plantsciname) |>
    dplyr::filter(!is.na(Scientific_Name) & !is.na(plantsciname))

  message(paste(tax_change$plantsciname, "changed to", tax_change$Scientific_Name, "\n"))

  veg_df_tax$plantsym <- ifelse(is.na(veg_df_tax$Accepted_Symbol), veg_df_tax$plantsym, veg_df_tax$Accepted_Symbol)
  veg_df_tax$plantsciname <- ifelse(is.na(veg_df_tax$Scientific_Name), veg_df_tax$plantsciname, veg_df_tax$Scientific_Name)
  veg_df_tax$plantnatvernm <- ifelse(is.na(veg_df_tax$Accepted_Symbol), veg_df_tax$plantnatvernm, veg_df_tax$Common_Name)

  veg_df_tax$Accepted_Symbol <- NULL
  veg_df_tax$Scientific_Name <- NULL
  veg_df_tax$Common_Name <- NULL

  return(veg_df_tax)

}

















# Example veg_df
veg_df <- data.table(
  plantsym = c("SYMBOL1", "SYMBOL2", "MODERN1", ""),
  plantsciname = c("Old Name 1", "Old Name 2", "Modern Plant Name", "No Name"),
  other_column = c(1, 2, 3, 4)
)

# Example USDA PLANTS dataframe
usda_plants <- data.table(
  `Accepted Symbol` = c("MODERN1", "MODERN2"),
  `Synonym Symbol` = c("SYMBOL1", "SYMBOL2", ""),
  `Scientific Name` = c("Modern Plant Name", "Another Modern Plant Name", "")
)

# Ensure column names are compatible with data.table syntax
setnames(usda_plants, c("Accepted Symbol", "Synonym Symbol", "Scientific Name"),
         c("accepted_symbol", "synonym_symbol", "scientific_name"))

USDA_cols <- colnames(USDA_plants)
USDA_cols_ <- gsub(" ", "_", USDA_cols)
colnames(USDA_plants) <- USDA_cols_

# Filter out blank Synonym Symbols from USDA PLANTS
USDA_plants <- USDA_plants[Synonym_Symbol != "", ]

veg_df_veg_df_tax <- data.table::setDT(veg_df)

veg_df_tax <- veg_df_veg_df_tax[USDA_plants,
               on = .(plantsym = Synonym_Symbol),
               `:=`(
                 new_plantsym = data.table::fifelse(!is.na(Accepted_Symbol) & Accepted_Symbol != "", Accepted_Symbol, plantsym))]

# Perform the update and handle blanks
veg_df_tax2 <- veg_df_veg_df_tax[
  USDA_plants,
  on = .(plantsym = Synonym_Symbol),
  `:=`(
    new_plantsym = data.table::fifelse(!is.na(Accepted_Symbol) & Accepted_Symbol != "", Accepted_Symbol, plantsym),
    new_plantsciname = data.table::fifelse(!is.na(Accepted_Symbol) & Accepted_Symbol != "", Scientific_Name, plantsciname)
  )
]

# Create a message for changes in plantsciname
change_message <- veg_df[
  plantsym != new_plantsym | plantsciname != new_plantsciname,
  paste(plantsciname, "changed to", new_plantsciname, collapse = "\n")
]

# Apply the changes only if new_plantsym and new_plantsciname have values
veg_df[
  new_plantsym != plantsym & new_plantsym != "",
  plantsym := new_plantsym
]
veg_df[
  new_plantsciname != plantsciname & new_plantsciname != "",
  plantsciname := new_plantsciname
]

# Remove temporary columns
veg_df[, `:=`(new_plantsym = NULL, new_plantsciname = NULL)]

# Display the message
if (nzchar(change_message)) {
  message("Changes in plant scientific names:\n", change_message)
}

# View the updated veg_df
print(veg_df)

