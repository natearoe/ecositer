# This function is being developed. It's purpose is to create ecologically relevant
# site-level properties from horizon data.

# Currently, I am thinking of including four primary categories of properties:
# horizon level properties (master horizons or more specific), depth range properties
# (0-25, 0-50, etc. - though ideally they would be depth related to plant rooting depths...
# could be an argument in the function), full profile description (e.g., horizon thickness
# weighted average of texture across entire profile), and taxonomically relevant
# properties given their recognized importance in pedology.

# Properties of interest include; depth, texture, fragments, color, pH, AWC,
# ksat, drainage, CEC. Data population completeness likely varies between these
# properties.

# These properties will be useful for ecological site development, QA/QC of
# ecological sites, and ecological site modeling.


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
site_level_soil_properties <- function(soil_data,
                                       checkHzDepthLogic = TRUE,
                                       stopOnHzLogicFail = TRUE,
                                       byDepth = list(c(0,25), c(0, 50))){

  ################# QC horizon data ############################

  # check for horizon logic - errors should be fixed
  if (checkHzDepthLogic) {
    my_logic <- aqp::checkHzDepthLogic(soil_data, byhz = TRUE)
    my_illogic <- my_logic[my_logic$valid == FALSE,]

    if (nrow(my_illogic >= 0)) {
      if (stopOnHzLogicFail == TRUE) {
        stop(
          "Illogical conditions exist in horizon data. Please run aqp::checkHzDepthLogic() on your SoilProfileCollection and correct errors where possible. If you want to run the function despite failed horizon logic, set stopOnHzLogicFail == FALSE "
        )
      }
      else{
        warning(
          "Illogical conditions exist in horizon data. Please run aqp::checkHzDepthLogic() on your SoilProfileCollection and correct errors where possible."
        )
      }
    }
  }

  ################ Pre-division actions ################################
  # In the next step, we will divide our SPC into multiple SPCs (if the
  # user requests properties by depth). Here, we will make modifications
  # to our SPC that can be done prior to this process, so that it does not
  # need to be applied to multiple SPCs unnecessarily (inefficient).
  # These properties are: thickness, master horizon designations

  ## calculate thickness
  soil_data[["thk"]] <- soil_data$hzdepb - soil_data$hzdept

  ## assign horizon designations
  masters <- c("O", "A", "B", "C", "R")

  ## create master horizon logic
  for(i in masters){
    soil_data[[paste0("is", i)]] <- grepl(i, soil_data$hzname)
  }

  ## set soil color
  munsell <- c("hue", "value", "chroma")
  moisture <- c("d", "m")

  color_list <- lapply(seq_along(moisture), FUN = function(x){
    paste(moisture[x], munsell, sep = "_")
  })

  lapply(seq_along(color_list), FUN = function(x){
    cie_lab <- cbind(soil_data$phiid, aqp::munsell2rgb(soil_data[[paste(color_list[[x]][1])]],
                                                       soil_data[[paste(color_list[[x]][2])]],
                                                       soil_data[[paste(color_list[[x]][3])]],
                                                       returnLAB = TRUE))
    colnames(cie_lab) <- c("phiid", paste(moisture[x], c("L", "A", "B"),
                                          sep = "_"))
    aqp::horizons(soil_data) <<- cie_lab
  })

  ### pull color from deepest horizon with color data




  test <- soil_data@horizons |> dplyr::filter(
    !is.na(soil_data$d_L) &
      !is.na(soil_data$d_A) &
      !is.na(soil_data$d_B)) |>
    dplyr::filter(isB == TRUE | isC == TRUE | isR == TRUE) |> dplyr::group_by(peiid, d_L, d_A, d_B) |>
    dplyr::summarise(max(hzdept))

  test$test <- ifelse(soil_data$peiid == test$peiid &
                        soil_data$hzdept == test$`max(hzdept)`,
                      TRUE,
                      FALSE)

  lapply(unique(soil_data$peiid), FUN = function(x){
    aqp::subset(soil_data, peiid == x)
  })

  soil_

  soil_data$peiid




  test <- soil_data[is.na(soil_data$d_L) &
              is.na(soil_data$d_A) &
              is.na(soil_data$d_B)
              ,] |> aqp::subsetHz(hzdept == max(hzdept))

  soil_data[is.na(soil_data$d_L) &
              is.na(soil_data$d_A) &
              is.na(soil_data$d_B)
            ,] |> dplyr::group_by(peiid) |> summar

  soil_data@horizons |> dplyr::filter(is.na(soil_data$d_L) &
                                        is.na(soil_data$d_A) &
                                        is.na(soil_data$d_B) ) |>
    dplyr::summarise(max(hzdept))

  soil_data$test <-



  soil_data@horizons |> dplyr::filter(is.na(soil_data$d_L) &
                                        is.na(soil_data$d_A) &
                                        is.na(soil_data$d_B)) |> dplyr::group_by(peiid) |>
    dplyr::summarise(max(hzdept))

  soil_data$deepest_color <- ifelse(peiid)




  ################ Create a list of SPCs ###############################
  # SPC is divided into multiple SPCs, if user requests depth ranges.
  # All future actions are applied to
  byDepthNames <- c("full_profile",  lapply(byDepth, FUN = function(x){
    paste(x, collapse = "_")
  }) |> unlist())
  byDepth <- c(list(c(0,999)), byDepth)


  SPC_list <- lapply(seq_along(byDepth), FUN = function(x){
    SPC_sub <- soil_data |> aqp::subsetHz(!hzdept >= byDepth[[x]][2] &
                                            !hzdepb <= byDepth[[x]][1])
    SPC_sub$hzdepb <- ifelse(SPC_sub$hzdepb > byDepth[[x]][2],
                             byDepth[[x]][2],
                             SPC_sub$hzdepb)
    SPC_sub$hzdept <- ifelse(SPC_sub$hzdept < byDepth[[x]][1],
                             byDepth[[x]][1],
                             SPC_sub$hzdept)
    return(SPC_sub)
  })

  names(SPC_list) <- byDepthNames

  ###################### Property calculations ##############################

  ###### thickness of masters
  # possibly do this with for loop - group/summarize horizons, join to sites
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x |> aqp::mutate_profile(o_hz_thk = sum(thk[isO], na.rm = TRUE),
                                     a_hz_thk = sum(thk[isA], na.rm = TRUE),
                                     b_hz_thk = sum(thk[isB], na.rm = TRUE),
                                     c_hz_thk = sum(thk[isC], na.rm = TRUE),
                                     r_hz_thk = sum(thk[isR], na.rm = TRUE))
  })

  # o surface thickness
  SPC_list <- lapply(SPC_list, FUN = function(x){
    o_surf_thk <- aqp::getSurfaceHorizonDepth(x,
                        pattern = "O")[, c(1,3)]
    colnames(o_surf_thk) <- c("peiid", "o_surf_thk")
    aqp::site(x) <- o_surf_thk
    return(x)
  })


  # texture by horizon
  SPC_list <- lapply(SPC_list, FUN = function(x){
    ssc_texcl <- cbind(x$phiid, aqp::texcl_to_ssc(x$texcl))
    colnames(ssc_texcl) <- c("phiid", "sand_texcl", "silt_texcl", "clay_texcl")
    aqp::horizons(x) <- ssc_texcl
    return(x)
  })

  # calculate best texture
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x$sand_best <- ifelse(is.na(x$sand), x$sand_texcl,
                                  x$sand)
    x$silt_best <- ifelse(is.na(x$silt), x$silt_texcl,
                                  x$silt)
    x$clay_best <- ifelse(is.na(x$clay), x$clay_texcl,
                                  x$clay)
    return(x)
  })



  # site level texture properties
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x <- x |> aqp::mutate_profile(o_sand_wtd = weighted.mean(sand_best[isO],
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
    return(x)
  })

  # pH
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x <- x |> aqp::mutate_profile(o_ph_wtd = weighted.mean(phfield[isO],
                                                                           thk[isO], na.rm = TRUE),
                                                  a_ph_wtd = weighted.mean(phfield[isA],
                                                                           thk[isA], na.rm = TRUE),
                                                  b_ph_wtd = weighted.mean(phfield[isB],
                                                                           thk[isB], na.rm = TRUE),
                                                  c_ph_wtd = weighted.mean(phfield[isC],
                                                                           thk[isC], na.rm = TRUE),
                                                  full_prof_ph_wtd = weighted.mean(phfield,
                                                                                   thk, na.rm = TRUE))
    return(x)
  })

  # frag vol.
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x <- x |> aqp::mutate_profile(o_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isO],
                                                                                     thk[isO], na.rm = TRUE),
                                                  a_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isA],
                                                                                     thk[isA], na.rm = TRUE),
                                                  b_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isB],
                                                                                     thk[isB], na.rm = TRUE),
                                                  c_frag_vol_tot_wtd = weighted.mean(total_frags_pct[isC],
                                                                                     thk[isC], na.rm = TRUE),
                                                  full_prof_frag_vol_tot_wtd = weighted.mean(total_frags_pct,
                                                                                             thk, na.rm = TRUE))
    return(x)
  })

  # depth
  SPC_list <- lapply(SPC_list, FUN = function(x){
    depth_class <- aqp::getSoilDepthClass(x)
    aqp::site(x) <- depth_class
    return(x)
  })

  # color - only from full horizon
  SPC_list[[1]][!is.na(SPC_list[[1]]$d_L) &
                  !is.na(SPC_list[[1]]$d_A) &
                  !is.na(SPC_list),]


  SPC_list[[1]]$m
}

