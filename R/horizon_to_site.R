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


  ###################### Property categories ##############################

  ####### calculate thickness
  soil_data[["thk"]] <- soil_data$hzdepb - soil_data$hzdept


  ####### assign horizon designations
  masters <- c("O", "A", "B", "C", "R")


  # create master horizon logic
  for(i in masters){
    soil_data[[paste0("is", i)]] <- grepl(i, soil_data$hzname)
  }


  ####### depth intervals
  depths <- c(5, 25, 50, 100, 150)

  ####### taxonomy



  ###################### Properties #######################################

  ###### thickness of masters
  # possibly do this with for loop - group/summarize horizons, join to sites
  soil_data <- soil_data |> aqp::mutate_profile(o_hz_thk = sum(thk[isO], na.rm = TRUE),
                                                  a_hz_thk = sum(thk[isA], na.rm = TRUE),
                                                  b_hz_thk = sum(thk[isB], na.rm = TRUE),
                                                  c_hz_thk = sum(thk[isC], na.rm = TRUE),
                                                  r_hz_thk = sum(thk[isR], na.rm = TRUE))

  # o surface thickness
  o_surf_thk <- aqp::getSurfaceHorizonDepth(soil_data,
                              pattern = "O")[, c(1,3)]

  colnames(o_surf_thk) <- c("peiid", "o_surf_thk")

  aqp::site(soil_data) <- o_surf_thk

  # texture by horizon
  ssc_texcl <- cbind(soil_data$phiid, aqp::texcl_to_ssc(soil_data$texcl))

  colnames(ssc_texcl) <- c("phiid", "sand_texcl", "silt_texcl", "clay_texcl")

  aqp::horizons(soil_data) <- ssc_texcl

  # calculate best texture
  soil_data$sand_best <- ifelse(is.na(soil_data$sand), soil_data$sand_texcl,
                                 soil_data$sand)
  soil_data$silt_best <- ifelse(is.na(soil_data$silt), soil_data$silt_texcl,
                                 soil_data$silt)
  soil_data$clay_best <- ifelse(is.na(soil_data$clay), soil_data$clay_texcl,
                                 soil_data$clay)

  # site level texture properties
  soil_data <- soil_data |> aqp::mutate_profile(o_sand_wtd = weighted.mean(sand_best[isO],
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
  soil_data <- soil_data |> aqp::mutate_profile(o_ph_wtd = weighted.mean(phfield[isO],
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
  soil_data <- soil_data |> aqp::mutate_profile(o_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isO],
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
  depth_class <- aqp::getSoilDepthClass(soil_data)
  aqp::site(soil_data) <- depth_class



}

