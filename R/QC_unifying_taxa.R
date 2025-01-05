
#' QC taxonomies for unifying
#'
#' `QC_unify_data` returns a dataframe of taxa that should be QCed for opportunities to unify. There are two types of situations
#' that are evaluatied. The first is whether there are IDs beyond species (i.e., var or ssp) as well as to species.
#' For example, Pinus contorta and Pinus contorta var. murrayana. Users may want to unify these observations
#' (e.g., change all to Pinus contorta). The second situation is observations to genus as well as genus species. For example
#' IDs to Pinus and to Pinus contorta. In some instances, it will be possible to unify these records (e.g., at the end of a
#' project you determine there is only one species of a genus present). Statistical analyses do not recognize any relationship
#' between different classes, therefore these taxonomically similar situations are viewed by distance measures as completely
#' unrelated. Therefore, when reasonable, it is best to unify taxonomic observations.
#'
#' @param veg_df
#'
#' @return a dataframe of taxa recommended for unifying
#' @export
#'
#' @examples
#' QC_plants(veg_df = ecositer::vegetation_dataframe) |> head()
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
