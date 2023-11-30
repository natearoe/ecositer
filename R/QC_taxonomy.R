#' QC taxonomy
#'
#' @param veg_df a properly formatted vegetation dataframe.
#'
#' @return vegetation dataframe with updated taxonomy according to USDA PLANTS.
#' @export
#'
#' @examples
#' QC_taxonomy(veg_df = my_veg_df)
QC_taxonomy <- function(veg_df){
  # there is a lot of room for efficiency improvements. the regex is pretty complicated but could be done in a more elegant way.
  PLANTS <- read.delim2("https://plants.usda.gov/assets/docs/CompletePLANTSList/plantlst.txt",
                        sep = ",")

  sym_key <- veg_df |> dplyr::select(plantsym, plantsciname) |>
    dplyr::filter(plantsym %in% PLANTS$Synonym.Symbol) |>
    dplyr::left_join(PLANTS |> dplyr::select(Symbol, Synonym.Symbol),
                     by = c("plantsym" = "Synonym.Symbol")) |>
    dplyr::rename(new_code = Symbol) |> unique() |>
    dplyr::left_join(PLANTS |> dplyr::filter(Synonym.Symbol == "") |>
                     dplyr::select(Symbol, Scientific.Name.with.Author),
                     by = c("new_code" = "Symbol"))

  sym_key$ascii <- stringi::stri_trans_general(sym_key$Scientific.Name.with.Author, "latin-ascii")
  sym_key$ascii <- gsub(pattern = "\\*", replacement = "x", x = sym_key$ascii)

  sym_key$new_genus <- stringr::str_extract(sym_key$ascii,
                                           pattern = "[:alpha:]+(?=[:space:])")

  sym_key$new_species <- stringr::str_extract(sym_key$ascii,
                                             pattern = "(?<=[:space:])[:lower:]+")

  sym_key$f_var <- stringr::str_extract(sym_key$ascii,
                                       pattern = "(?<=[:space:])f\\.[:space:]var\\.[:space:][:lower:]+")

  sym_key$f_ssp <- stringr::str_extract(sym_key$ascii,
                                       pattern = "(?<=[:space:])f\\.[:space:]ssp\\.[:space:][:lower:]+")

  sym_key$f <- stringr::str_extract(sym_key$ascii,
                                   pattern = "(?<=[:space:])f\\.[:space:][:lower:]+")

  sym_key$f <- stringr::str_replace(sym_key$f, pattern = "f\\.[:space:]ex",
                                   replacement = "f\\.")


  sym_key$var <- stringr::str_extract(sym_key$ascii,
                                     pattern = "(?<=[:space:])var\\.[:space:][:lower:]+")

  sym_key$ssp <- stringr::str_extract(sym_key$ascii,
                                     pattern = "(?<=[:space:])ssp\\.[:space:][:lower:]+")

  sym_key$var <- ifelse(is.na(sym_key$f_var), sym_key$var, NA)

  sym_key$f <- ifelse(is.na(sym_key$f_var), sym_key$f, NA)



  sym_key$ssp <- ifelse(is.na(sym_key$f_ssp), sym_key$ssp, NA)

  sym_key$f <- ifelse(is.na(sym_key$f_ssp), sym_key$f, NA)


  sym_key$new_sci_name <- paste(sym_key$new_genus, sym_key$new_species, sym_key$f_var, sym_key$f_ssp, sym_key$ssp, sym_key$var, sym_key$f)

  sym_key$new_sci_name <- gsub(pattern = "NA", replacement = "", x = sym_key$new_sci_name)

  sym_key$new_sci_name <- gsub("^ *|(?<= ) | *$", "", sym_key$new_sci_name, perl = TRUE)

  sym_key <- sym_key |> dplyr::left_join(veg_df |> dplyr::select(plantsym, plantsciname) |> unique(), by = c("new_code" = "plantsym"))

  sym_key$final_sci_name <- ifelse(is.na(sym_key$plantsciname.y), sym_key$new_sci_name, sym_key$plantsciname.y)

  veg_df$plantsym <- plyr::mapvalues(veg_df$plantsym, from = sym_key$plantsym, to = sym_key$new_code)

  veg_df$plantsciname <- plyr::mapvalues(veg_df$plantsciname, from = sym_key$plantsciname.x, to = sym_key$final_sci_name)

  return(veg_df)










  # sym_key$new_
  #
  # sym_key$new_sci <- stringr::str_extract(sym_key$Scientific.Name.with.Author,
  #                                        pattern = "[:upper:][:lower:]+(?![:space:][:upper:])")
  #
  # PLANTS$ascii <- stringi::stri_trans_general(PLANTS$Scientific.Name.with.Author, "latin-ascii")
  # PLANTS$ascii <- gsub(pattern = "\\*", replacement = "x", x = PLANTS$ascii)
  #
  # PLANTS$new_sci <- stringr::str_extract(PLANTS$Scientific.Name.with.Author,
  #                                                             pattern = "[:upper:][:lower:]+[:space:][:lower:]+")
  #
  # PLANTS$new_genus <- stringr::str_extract(PLANTS$Scientific.Name.with.Author,
  #                                        pattern = "[:alpha:]+(?=[:space:])")
  #
  # PLANTS$new_species <- stringr::str_extract(PLANTS$Scientific.Name.with.Author,
  #                                          pattern = "(?<=[:space:])[:lower:]+")
  #
  #
  # PLANTS$ascii <- stringi::stri_trans_general(PLANTS$Scientific.Name.with.Author, "latin-ascii")
  # PLANTS$ascii <- gsub(pattern = "\\*", replacement = "x", x = PLANTS$ascii)
  #
  #
  # PLANTS$new_genus <- stringr::str_extract(PLANTS$ascii,
  #                                          pattern = "[:alpha:]+(?=[:space:])")
  #
  # PLANTS$new_species <- stringr::str_extract(PLANTS$ascii,
  #                                            pattern = "(?<=[:space:])[:lower:]+")
  #
  # # PLANTS$new_var <- stringr::str_extract(PLANTS$ascii,
  # #                                        pattern = "(?<=[:space:])f\\.[:space:][:lower:]+")
  #
  #
  # PLANTS$f_var <- stringr::str_extract(PLANTS$ascii,
  #                                      pattern = "(?<=[:space:])f\\.[:space:]var\\.[:space:][:lower:]+")
  #
  # PLANTS$f_ssp <- stringr::str_extract(PLANTS$ascii,
  #                                      pattern = "(?<=[:space:])f\\.[:space:]ssp\\.[:space:][:lower:]+")
  #
  # PLANTS$f <- stringr::str_extract(PLANTS$ascii,
  #                                  pattern = "(?<=[:space:])f\\.[:space:][:lower:]+")
  #
  # PLANTS$f <- stringr::str_replace(PLANTS$f, pattern = "f\\.[:space:]ex",
  #                                  replacement = "f\\.")
  #
  #
  # PLANTS$var <- stringr::str_extract(PLANTS$ascii,
  #                                      pattern = "(?<=[:space:])var\\.[:space:][:lower:]+")
  #
  # PLANTS$ssp <- stringr::str_extract(PLANTS$ascii,
  #                                    pattern = "(?<=[:space:])ssp\\.[:space:][:lower:]+")
  #
  # PLANTS$var <- ifelse(is.na(PLANTS$f_var), PLANTS$var, NA)
  #
  # PLANTS$f <- ifelse(is.na(PLANTS$f_var), PLANTS$f, NA)
  #
  #
  #
  # PLANTS$ssp <- ifelse(is.na(PLANTS$f_ssp), PLANTS$ssp, NA)
  #
  # PLANTS$f <- ifelse(is.na(PLANTS$f_ssp), PLANTS$f, NA)
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  # PLANTS$new_sci_name <- paste(PLANTS$new_genus, PLANTS$new_species)
  #
  # PLANTS$test <- paste(PLANTS$f_var, PLANTS$f_ssp, PLANTS$ssp, PLANTS$var, PLANTS$f)
  #
  # PLANTS$test <- gsub(pattern = "NA", replacement = "", x = PLANTS$test)
  #
  # PLANTS$
  #
  #
  # PLANTS$test <- ifelse(!is.na(PLANTS$f_var), )
  #
  #
  # PLANTS$below_spp <- stringr::str_extract(PLANTS$ascii,
  #                                        pattern = "(?<=[:space:])f\\.[:space:][:lower:]+|
  #                                        f\\.var\\.[:space:][:lower:]+
  #                                        ssp\\.[:space:][:lower:]+|
  #                                        var\\.[:space:][:lower:]+")
  #
  #
  # PLANTS[grepl("[^ -~]", PLANTS$Scientific.Name.with.Author),] |> View()
  #
  #
  #
  #
  #
  #
  #
  # sym_key$new_sci <- stringr::str_extract(sym_key$Scientific.Name.with.Author,
  #                                         pattern = "[:upper:][:lower:]+[:space:][:lower:]+")
  #
  # "[:upper:][:lower:]+[:space:][:lower:])"
  #


}



