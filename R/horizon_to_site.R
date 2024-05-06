# This script is a continuation of efforts to take pedon horizon or component
## horizon data and summarize it to a site level property.

# I am currently thinking of three primary categories of properties:
## horizon, depth interval, and taxonomic. It is possible that particle
## size control section is another, but that may also fit into depth interval?

# Let's start with horizon.


#' Create site level properties from soil horizon data
#'
#' @param soil_data a soilDB SoilProfileCollection (SPC)
#'
#' @return an SPC with ecologically relevant properties appended to the site df
#' @export
#'
#' @examples
#'
#'
site_level_soil_properties <- function(soil_data){

  soil_data <- soilDB::fetchNASIS(from = "pedons")
  soil_data2 <- soil_data


  ###################### Property categories ##############################

  ####### calculate thickness
  soil_data2[["thk"]] <- soil_data2$hzdepb - soil_data2$hzdept


  ####### assign horizon designations
  masters <- c("O", "A", "B", "C", "R")


  # create master horizon logic
  for(i in masters){
    soil_data2[[paste0("is", i)]] <- grepl(i, soil_data2$hzname)
  }


  ####### depth intervals
  depths <- c(5, 25, 50, 100, 150)

  ####### taxonomy



  ###################### Properties #######################################

  ###### thickness of masters
  # possibly do this with for loop - group/summarize horizons, join to sites
  soil_data2 <- soil_data2 |> aqp::mutate_profile(o_hz_thk = sum(thk[isO], na.rm = TRUE),
                                                  a_hz_thk = sum(thk[isA], na.rm = TRUE),
                                                  b_hz_thk = sum(thk[isB], na.rm = TRUE),
                                                  c_hz_thk = sum(thk[isC], na.rm = TRUE),
                                                  r_hz_thk = sum(thk[isR], na.rm = TRUE))

  # o surface thickness
  o_surf_thk <- aqp::getSurfaceHorizonDepth(soil_data2,
                              pattern = "O")[, c(1,3)]

  colnames(o_surf_thk) <- c("peiid", "o_surf_thk")

  aqp::site(soil_data2) <- o_surf_thk

  # texture by horizon
  ssc_texcl <- cbind(soil_data2$phiid, aqp::texcl_to_ssc(soil_data2$texcl))

  colnames(ssc_texcl) <- c("phiid", "sand_texcl", "silt_texcl", "clay_texcl")

  aqp::horizons(soil_data2) <- ssc_texcl

  # calculate best texture
  soil_data2$sand_best <- ifelse(is.na(soil_data2$sand), soil_data2$sand_texcl,
                                 soil_data2$sand)
  soil_data2$silt_best <- ifelse(is.na(soil_data2$silt), soil_data2$silt_texcl,
                                 soil_data2$silt)
  soil_data2$clay_best <- ifelse(is.na(soil_data2$clay), soil_data2$clay_texcl,
                                 soil_data2$clay)

  # site level texture properties
  soil_data2 <- soil_data2 |> aqp::mutate_profile(o_sand_wtd = weighted.mean(sand_best[isO],
                                                                             thk[isO], na.rm = TRUE),
                                                  o_silt_wtd = weighted.mean(silt_best[isO],
                                                                             thk[isO], na.rm = TRUE),
                                                  o_clay_wtd = weighted.mean(clay_best[isO],
                                                                             thk[isO], na.rm = TRUE),
                                                  a_sand_wtd = weighted.mean(sand_best[isA],
                                                                             thk[isA], na.rm = TRUE),
                                                  a_silt_wtd = weighted.mean(silt_best[isA],
                                                                             thk[isA], na.rm = TRUE),
                                                  a_clay_wtd = weighted.mean(clay_best[isA],
                                                                             thk[isA], na.rm = TRUE),
                                                  b_sand_wtd = weighted.mean(sand_best[isB],
                                                                             thk[isB], na.rm = TRUE),
                                                  b_silt_wtd = weighted.mean(silt_best[isB],
                                                                             thk[isB], na.rm = TRUE),
                                                  b_clay_wtd = weighted.mean(clay_best[isB],
                                                                             thk[isB], na.rm = TRUE),
                                                  c_sand_wtd = weighted.mean(sand_best[isC],
                                                                             thk[isC], na.rm = TRUE),
                                                  c_silt_wtd = weighted.mean(silt_best[isC],
                                                                             thk[isC], na.rm = TRUE),
                                                  c_clay_wtd = weighted.mean(clay_best[isC],
                                                                             thk[isC], na.rm = TRUE),
                                                  full_prof_sand_wtd = weighted.mean(sand_best,
                                                                                     thk, na.rm = TRUE),
                                                  full_prof_silt_wtd = weighted.mean(silt_best,
                                                                                     thk, na.rm = TRUE),
                                                  full_prof_clay_wtd = weighted.mean(clay_best,
                                                                                     thk, na.rm = TRUE))

  # pH
  soil_data2 <- soil_data2 |> aqp::mutate_profile(o_ph_wtd = weighted.mean(phfield[isO],
                                                                           thk[isO], na.rm = TRUE),
                                                  a_ph_wtd = weighted.mean(phfield[isA],
                                                                           thk[isA], na.rm = TRUE),
                                                  b_ph_wtd = weighted.mean(phfield[isB],
                                                                           thk[isB], na.rm = TRUE),
                                                  c_ph_wtd = weighted.mean(phfield[isC],
                                                                           thk[isC], na.rm = TRUE),
                                                  full_prof_ph_wtd = weighted.mean(phfield,
                                                                                   thk, na.rm = TRUE))

  # frag vol.
  soil_data2 <- soil_data2 |> aqp::mutate_profile(o_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isO],
                                                                                     thk[isO], na.rm = TRUE),
                                                  a_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isA],
                                                                                     thk[isA], na.rm = TRUE),
                                                  b_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isB],
                                                                                     thk[isB], na.rm = TRUE),
                                                  c_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isC],
                                                                                     thk[isC], na.rm = TRUE),
                                                  full_prof_frag_vol_tot_wtd = weighted.mean(total_frags_pct,
                                                                                     thk, na.rm = TRUE))

  # depth
  depth_class <- aqp::getSoilDepthClass(soil_data2)
  aqp::site(soil_data2) <- depth_class

  # properties by depth
  aqp::slab(soil_data2,
            fm = phiid ~ clay_best,
            slab.structure = 0:50)

  head(soil_data2$peiid)
  test <- soil_data2[soil_data2$peiid == "100256"]

  aqp::dice(test,
            fm = 0:10 ~ .,
            slab.structure = 0:50)

  aqp::glomApply()




  # simulate some data, IDs are 1:20
  d <- lapply(1:20, random_profile)
  d <- do.call('rbind', d)

  # init SoilProfileCollection object
  depths(d) <- id ~ top + bottom
  head(horizons(d))

  # generate single slice at 10 cm
  # output is a SoilProfileCollection object
  s <- dice(d, fm = 10 ~ name + p1 + p2 + p3)


  aqp::dice(soil_data2, fm = 0:10 ~ .)





  aqp::dice(soil_data2)

  test <- aqp::repairMissingHzDepths(soil_data2)

  aqp::dice(test)












  o_surf_thk <- aqp::getSurfaceHorizonDepth(soils_data2,
                                            pattern = "O")[[3]]



  site


  pat <- c('^A', '^B', '^E', '^C')
  lab <- c('A', 'B', 'E', 'C')
  th <- lapply(seq_along(pat), FUN = function(i) {
    thicknessOf(x, pattern = pat[i], prefix = lab[i], thickvar = '_thick')
  })

  s <- Reduce(merge, th)

  site(x) <- s


  for(i in masters){
    soils_data2[[paste0(i), "thk"]] <- soils_data2$thk |> aggregate(FUN = )
  }

  soils_data2 |> dplyr::select(peiid, thk)

  aqp::subset(soils_data2, texcl == "c")

  aqp::subset(soils_data2, c("peiid", "thk"))

  soils_data2[, c("peiid", "thk")]

  soils_data2



  ###### texture of masters
  texs <- c("sand", "silt", "clay")

  for(i in seq_along(texs)){
    soils_data2[[paste0(texs[i], "_texcl")]]
  }


  soils_data2 <- aqp::texcl_to_ssc(texcl = soils_data2$texcl)

  for(i in unique(soils_data2$texcl[!is.na(soils_data2$texcl)])){
    print(i)
    aqp::texcl_to_ssc(texcl = i) |> print()
  }

  aqp::texcl_to_ssc(texcl = soils_data2$texcl)[[1]] ==



  # pre-calculate values used by profile mutate expressions (not profile specific)
  jacobs2000 <- transform(
    jacobs2000,
    thk = bottom - top,
    is_a = grepl('A', name),
    is_b = grepl('B', name),
    texcl = ssc_to_texcl(sand, clay)
  )

  jacobs2000 <- jacobs2000 |>
    mutate_profile(a_hz_thk = sum(thk[is_a]), # sum of thickness of A horizons
                   b_hz_clay = weighted.mean(clay[is_b], thk[is_b]), # weighted mean of B horizon clay
                   b_hz_ph = -log10(weighted.mean(10^-pH[is_b], thk[is_b])), # geometric weighted mean of B horizon pH
                   b_hz_texcl = texcl[which.max(thk[is_b])]) # texture class of thickest B horizon

}

