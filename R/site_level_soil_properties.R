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


#' @title Create site level properties from soil horizon data
#' @description
#' Calculate properties from soil horizon data that will be appended to the site table of a
#' SoilProfileCollection. The entire soil profile is used. Additionally,the byDepth argument allows
#' for calculations to be made on specified depth ranges.
#'
#' @param soil_data a soilDB SoilProfileCollection (SPC)
#' @param checkHzDepthLogic evaluate logic of horizon data. A warning or error will be issued depending on the stopOnHzLogicFail argument. aqp::checkHzDepthLogic() should be run on your data is logic errors exist and corrections should be made to data.
#' @param stopOnHzLogicFail will prevent this function from running if logic errors exist in horizon data.
#' @param byDepth a list of depth ranges to calculate properties across. Use NULL if depth ranges are not of interest.
#' @return an SPC with ecologically relevant properties appended to the site df
#' @export
#' @import data.table
#'
#' @examples
#'
#'
site_level_soil_properties <- function(soil_data,
                                       checkHzDepthLogic = TRUE,
                                       stopOnHzLogicFail = TRUE,
                                       byDepth = list(c(0,25), c(0, 50))){

  .SD <- NULL

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
  # This section calculates properties on entire soil profiles. In the
  # following section, the SoilProfileCollection is split into multiple
  # SoilProfileCollections where property by depth calculations are
  # performed (if requested by the user). The properties in this section
  # either make more sense to be calculated on the entire profile (e.g.,
  # depth) or feed into calculations that will later be performed on
  # SoilProfileCollections separated by depth (e.g., thickness of
  # horizon or master horizon id)

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
  LAB <- c("L", "A", "B")

  LAB_list <- lapply(seq_along(moisture), FUN = function(x){
    paste(moisture[x], LAB, sep = "_")
  })

  soil_dt <- data.table::setDT(aqp::horizons(soil_data))

  color_results <- list()

  color_results <- lapply(seq_along(color_list), FUN = function(x){

    soil_dt <- soil_dt[soil_dt[, rowSums(is.na(.SD)) == 0,
                               .SDcols = LAB_list[[x]]],]

    soil_dt <- soil_dt[soil_dt[, rowSums(.SD) > 0,
                               .SDcols = c("isB", "isC", "isR")],][
                                 , .SD[which.max(hzdept)], by = peiid,
                                 .SDcols = LAB_list[[x]]
                               ] |> data.table::setDF()

    colnames(soil_dt) <- c("peiid", paste(LAB_list[[x]], "lowest_mineral",
                                          sep = "_"))

    soil_dt

  })

  color_df <- do.call(merge, color_results)

  aqp::site(soil_data)  <-  color_df

  # depth
  depth_class <- aqp::getSoilDepthClass(soil_data)
  aqp::site(soil_data) <- depth_class

  aqp::site(soil_data) <- do.call('rbind', aqp::profileApply(soil_data, simplify = F, function(p) {
    restr <- aqp::restrictions(p)
    first.restr.depth <- suppressWarnings(min(restr$resdept, na.rm = TRUE)[1])
    if(is.finite(first.restr.depth)) {
      res <- restr[which(restr$resdept == first.restr.depth),]
      names(res) <- c(names(res)[1], paste0("first_", names(res)[2:length(names(res))]))
      return(res)
    } else {
      return(restr[0,])
    }
  }))

  soil_data$depth_best <- ifelse(is.na(soil_data$first_resdept),
                                 soil_data$first_resdept,
                                 soil_data$first_resdept) # !!!! this needs editting

  ################ Create a list of SPCs ###############################
  # SPC is divided into multiple SPCs, if user requests depth ranges.
  # All future actions are applied to
  byDepthNames <- c("full_profile",  lapply(byDepth, FUN = function(x){
    paste("cm", paste(x, collapse = "_"), sep = "_")
  }) |> unlist())
  byDepth <- c(list(c(0,999)), byDepth)

  SPC_list <- list()

  SPC_list <- lapply(seq_along(byDepth), FUN = function(x){
    SPC_sub <- soil_data |> aqp::subsetHz(!hzdept >= byDepth[[x]][2] &
                                            !hzdepb <= byDepth[[x]][1])
    SPC_sub$hzdepb <- ifelse(SPC_sub$hzdepb > byDepth[[x]][2],
                             byDepth[[x]][2],
                             SPC_sub$hzdepb)
    SPC_sub$hzdept <- ifelse(SPC_sub$hzdept < byDepth[[x]][1],
                             byDepth[[x]][1],
                             SPC_sub$hzdept)
    SPC_sub

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

  # surface frag tot perc.
  SPC_list <- lapply(SPC_list, FUN = function(x){
    x <- x |> aqp::mutate_profile(surf_frag_tot_perc = )
    return(x)
  })


  return(SPC_list)

}

