#' Identify Taxa for Unification
#'
#' @description
#' This function returns a data frame of plant taxonomies that should be evaluated for unification. Unifying taxa means lumping
#' multiple distinct taxa together. Statistical analyses do not recognize any relationship
#' between different taxonomic classes, therefore taxonomically similar situations are viewed by distance measures as
#' unrelated, despite potentially having very close relationship. Therefore, it is more meaningful to unify taxa rather than preserving
#' distinctions between closely related taxa.
#'
#' There are two types of situations that are evaluated:
#' 1. **Infraspecific Species Variation**: Cases where observations exist both at the species and infraspecific levels (e.g., *Pinus contorta* and *Pinus contorta var. murrayana*). Users may wish to collapse these to the species level.
#' 2. **Mixed Genus and Species-Level Observations**: Records identified to genus (e.g., *Pinus*) alongside specific species within that genus (e.g., *Pinus contorta*). In data sets where there are genus level observations and
#' either a single species, or species with similar ecological function, users may collapse to the genus level.
#'
#' @param veg_df vegetation dataframe
#'
#' @return a dataframe of taxa recommended for unifying
#' @export
#'
#' @examples
#' QC_unify_taxa(veg_df = SEKI_NP_veg)
QC_unify_taxa <- function(veg_df){

  # convert to data.table
  veg_df <- data.table::as.data.table(veg_df)
  # keep only plantsciname column and remove NAs
  veg_df <- veg_df[, .SD, .SDcols = "plantsciname"][!is.na(plantsciname)]

  taxa_levels <- veg_df[, `:=` (
    # create a genus column
    genus = sapply(strsplit(plantsciname, " "), `[`, 1),
    # create a species column
    species = sapply(strsplit(plantsciname, " "), `[`, 2),
    # create a subspecies column - this might pick up things like ssp or var but I'm not worried about that as it still indicates ID beyond species
    subspecies = sapply(strsplit(plantsciname, " "), `[`, 3),
    # how many levels of ID are there are indicated by blank spaces, again - ssp and var are not concern. beyond 1 space is ID beyond species
    levels = sapply(strsplit(plantsciname, " "), length)
    # create columns with combine genus/species and species/sub, use ifelse to change NAs to ""
  )][, `:=` (genus_species = paste(ifelse(is.na(genus), "", genus),
                                   ifelse(is.na(species), "", species)),
             species_subspecies = paste(ifelse(is.na(species), "", species),
                                        ifelse(is.na(subspecies), "", subspecies)))]

  # remove records without species level id, determine unique levels grouped by genus_species, only keep those with > 1 unique level
  spp_ssp <- taxa_levels[!is.na(species)][, .(unique_vals = data.table::uniqueN(levels)), by = genus_species][unique_vals > 1]

  # filter taxa_level down by matching on genus_species with spp_ssp, count number of times plantsciname used
  spp_ssp <- taxa_levels[genus_species %in% spp_ssp$genus_species][, .(occurences = .N), by = plantsciname]

  # group by genus, is that an NA and non-NA value in species?
  gen_spp <- taxa_levels[!is.na(genus)][, .(species_na_and_not = any(is.na(species)) & any(!is.na(species))), by = genus][species_na_and_not == TRUE]

  # filter taxa_level down using logical
  gen_spp <- taxa_levels[genus %in% gen_spp$genus][, .(occurences = .N), by = plantsciname]

  # combine the two dataframes
  combined_taxa <- rbind(spp_ssp, gen_spp) |> unique()

  # sort alphabeti-cool
  data.table::setorder(combined_taxa, plantsciname)

  #return results
  return(combined_taxa)

}
