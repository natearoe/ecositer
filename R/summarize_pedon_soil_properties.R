#' Summarize pedon soil properties
#'
#' @description
#' This function provides summarized soil properties for pedons. Summarized properties
#' are calculated for the entire profile as well as for user specified depth ranges.
#' Properties include texture, pH, fragment volume,
#'
#'
#' @param SS
#' @param static_location
#' @param byDepth
#'
#' @return
#' @export
#'
#' @examples
summarize_pedon_soil_properties <- function(SS = TRUE,
                                            static_location = NULL,
                                            byDepth = list(c(0,25), c(0,50))){


  # static location must be provided if SS = FALSE
  if (SS == FALSE && is.null(static_location)) {
    stop("Static location must be provided if SS = FALSE.")
  }

  # class of byDepth must be list
  if (class(byDepth) != "list") {
    stop(
      "Class of byDepth must be list. Follow the following format -
         byDepth = list(c(0,25), c(0,50))"
    )
  }

  if (SS == TRUE && !is.null(static_location)) {
    stop(
      "Choose between accessing data by SS or static_location. Currently, static_location given and SS = TRUE."
    )
  }

  if (SS == TRUE) {
    # NASIS must be accessible if using SS = TRUE
    if (soilDB::local_NASIS_defined() == FALSE) {
      stop(
        "Could not find local NASIS odbc data source. Visit this tutorial - https://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html"
      )
    } else(
      soil_data <- soilDB::fetchNASIS(from = "pedons")
    )
  }


  if (SS == TRUE && soilDB::local_NASIS_defined() == FALSE) {
    stop(
      "Could not find local NASIS odbc data source. Visit this tutorial - https://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html"
    )
  }

  # QC on static_location
  if (!is.null(static_location)) {
    check_sqlite_path <-
      function(static_location, required_table = "site") {
        # Check if the file exists
        if (!file.exists(static_location)) {
          stop("Error: The specified file does not exist.")
        }

        # check if file exists but is not SQLite database
        conn <- tryCatch({
          DBI::dbConnect(RSQLite::SQLite(), dbname = static_location)
        }, warning = function(w) {
          stop("Error: The file exists but is not a valid SQLite database.")
        }, error = function(e) {
          stop("Error: The file exists but is not a valid SQLite database.")
        })

        # check if the database is valid by listing tables
        tables <- tryCatch({
          DBI::dbListTables(conn)
        }, warning = function(w) {
          DBI::dbDisconnect(conn)
          stop("Error in static_location: The file exists but is not a valid SQLite database.")
        }, error = function(e) {
          DBI::dbDisconnect(conn)
          stop("Error in static_location: The file exists but is not a valid SQLite database.")
        })

        # check for the required table
        if (!required_table %in% tables) {
          DBI::dbDisconnect(conn) # Clean up connection
          stop(
            sprintf(
              "Error in static_location: The database does not contain the required table '%s'.",
              required_table
            )
          )
        }

        # disconnect from the database if all checks pass
        DBI::dbDisconnect(conn)

      }

    check_sqlite_path(static_location = static_location)
    soil_data <- soilDB::fetchNASIS(from = "pedons",
                                    SS = FALSE,
                                    dsn = static_location)
  }


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
                                 soil_data$first_resdept) # !!!! this needs editing

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


  return(SPC_list)



}
