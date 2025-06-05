#' Site level soil properties
#'
#' @param pedon_data an aqp SoilProfileCollection of pedon data
#'
#' @return a site-level dataframe with aggregated soil properties
#' @export
#'
#' @examples
#' site_soils(pedon_data = my_peds)
site_soils <- function(pedon_data){
  ## horizon data
  my_horizons <- aqp::horizons(pedon_data)

  ## site data
  my_sites <- aqp::site(pedon_data)

  ## horizon thickness
  my_horizons$thickness <- my_horizons$hzdepb - my_horizons$hzdept

  ## O thickness
  # O total thickness
  my_horizons$isO <- grepl("O", my_horizons$hzname)
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_thick_total = sum(thickness)) |>
    dplyr::select(peiid, o_thick_total) |>
    dplyr::right_join(my_sites)
  my_sites$o_thick_total[is.na(my_sites$o_thick_total)] <- 0
  # O surface thickness
  o_thick_surf <- aqp::getSurfaceHorizonDepth(pedon_data,
                                              pattern = "O",
                                              hzdesgn = 'name') |>
    dplyr::rename(o_thick_surf = hzdepb) |> dplyr::right_join(my_sites)

  ## A thickness
  my_horizons$isA <- grepl("A", my_horizons$hzname)
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_thick_total = sum(thickness)) |>
    dplyr::select(peiid, a_thick_total) |>
    dplyr::right_join(my_sites)
  my_sites$a_thick_total[is.na(my_sites$a_thick_total)] <- 0

  ## B thickness
  my_horizons$isB <- grepl("B", my_horizons$hzname)
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_thick_total = sum(thickness)) |>
    dplyr::select(peiid, b_thick_total) |>
    dplyr::right_join(my_sites)
  my_sites$b_thick_total[is.na(my_sites$b_thick_total)] <- 0

  ## C thickness
  my_horizons$isC <- grepl("C", my_horizons$hzname)
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_thick_total = sum(thickness)) |>
    dplyr::select(peiid, c_thick_total) |>
    dplyr::right_join(my_sites)
  my_sites$c_thick_total[is.na(my_sites$c_thick_total)] <- 0

  ## Texture
  ssc_texcl <- aqp::texcl_to_ssc(my_horizons$texcl)
  my_horizons$clay_texcl <- ssc_texcl$clay
  my_horizons$silt_texcl <- ssc_texcl$silt
  my_horizons$sand_texcl <- ssc_texcl$sand

  # textures <- c("c", "s", "l", "sil")
  # tx_samples <- sample(textures, 50, replace = T)
  # aqp::texcl_to_ssc(tx_samples)

  # Best texture
  my_horizons$sand_best <- ifelse(!is.na(my_horizons$sand), my_horizons$sand, my_horizons$sand_texcl)
  my_horizons$silt_best <- ifelse(!is.na(my_horizons$silt), my_horizons$silt, my_horizons$silt_texcl)
  my_horizons$clay_best <- ifelse(!is.na(my_horizons$clay), my_horizons$clay, my_horizons$clay_texcl)


  ## O texture
  # sand wtavg
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_sand_wtavg = weighted.mean(sand_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, o_sand_wtavg) |>
    dplyr::right_join(my_sites)
  # silt wtavg
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_silt_wtavg = weighted.mean(silt_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, o_silt_wtavg) |>
    dplyr::right_join(my_sites)
  # clay wtavg
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_clay_wtavg = weighted.mean(clay_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, o_clay_wtavg) |>
    dplyr::right_join(my_sites)

  ## A texture
  # sand wtavg
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_sand_wtavg = weighted.mean(sand_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, a_sand_wtavg) |>
    dplyr::right_join(my_sites)
  # silt wtavg
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_silt_wtavg = weighted.mean(silt_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, a_silt_wtavg) |>
    dplyr::right_join(my_sites)
  # clay wtavg
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_clay_wtavg = weighted.mean(clay_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, a_clay_wtavg) |>
    dplyr::right_join(my_sites)

  ## B texture
  # sand wtavg
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_sand_wtavg = weighted.mean(sand_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, b_sand_wtavg) |>
    dplyr::right_join(my_sites)
  # silt wtavg
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_silt_wtavg = weighted.mean(silt_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, b_silt_wtavg) |>
    dplyr::right_join(my_sites)
  # clay wtavg
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_clay_wtavg = weighted.mean(clay_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, b_clay_wtavg) |>
    dplyr::right_join(my_sites)

  ## C texture
  # sand wtavg
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_sand_wtavg = weighted.mean(sand_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, c_sand_wtavg) |>
    dplyr::right_join(my_sites)
  # silt wtavg
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_silt_wtavg = weighted.mean(silt_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, c_silt_wtavg) |>
    dplyr::right_join(my_sites)
  # clay wtavg
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_clay_wtavg = weighted.mean(clay_best, thickness,
                                                                            na.rm = TRUE)) |>
    dplyr::select(peiid, c_clay_wtavg) |>
    dplyr::right_join(my_sites)

  ## Full profile texture
  # sand wtavg
  my_sites <- my_horizons |>
    dplyr::group_by(peiid) |> dplyr::summarise(sand_fullprof_wtavg = weighted.mean(sand_best, thickness,
                                                                                   na.rm = TRUE)) |>
    dplyr::select(peiid, sand_fullprof_wtavg) |>
    dplyr::right_join(my_sites)

  # silt wtavg
  my_sites <- my_horizons |>
    dplyr::group_by(peiid) |> dplyr::summarise(silt_fullprof_wtavg = weighted.mean(silt_best, thickness,
                                                                                   na.rm = TRUE)) |>
    dplyr::select(peiid, silt_fullprof_wtavg) |>
    dplyr::right_join(my_sites)

  # clay wtavg
  my_sites <- my_horizons |>
    dplyr::group_by(peiid) |> dplyr::summarise(clay_fullprof_wtavg = weighted.mean(clay_best, thickness,
                                                                                   na.rm = TRUE)) |>
    dplyr::select(peiid, clay_fullprof_wtavg) |>
    dplyr::right_join(my_sites)

  ## AWC
  # aqp::estimateAWC(texcl = my_horizons$texcl,
  #                  omcl = )

  ## pH
  # O pH wtavg
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_ph_wtavg = weighted.mean(phfield, thickness,
                                                                          na.rm = TRUE)) |>
    dplyr::select(peiid, o_ph_wtavg) |>
    dplyr::right_join(my_sites)
  # A pH wtavg
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_ph_wtavg = weighted.mean(phfield, thickness,
                                                                          na.rm = TRUE)) |>
    dplyr::select(peiid, a_ph_wtavg) |>
    dplyr::right_join(my_sites)
  # B pH wtavg
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_ph_wtavg = weighted.mean(phfield, thickness,
                                                                          na.rm = TRUE)) |>
    dplyr::select(peiid, b_ph_wtavg) |>
    dplyr::right_join(my_sites)
  # C pH wtavg
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_ph_wtavg = weighted.mean(phfield, thickness,
                                                                          na.rm = TRUE)) |>
    dplyr::select(peiid, c_ph_wtavg) |>
    dplyr::right_join(my_sites)
  # full profile pH
  my_sites <- my_horizons |>
    dplyr::group_by(peiid) |> dplyr::summarise(full_prof_ph_wtavg = weighted.mean(phfield, thickness,
                                                                                  na.rm = TRUE)) |>
    dplyr::select(peiid, full_prof_ph_wtavg) |>
    dplyr::right_join(my_sites)

  ## Frag volumes
  # O frag vol wtavg
  my_sites <- my_horizons |> dplyr::filter(isO == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(o_total_frags_pct_wtavg = weighted.mean(total_frags_pct, thickness,
                                                                                       na.rm = TRUE)) |>
    dplyr::select(peiid, o_total_frags_pct_wtavg) |>
    dplyr::right_join(my_sites)
  # A frag vol wtavg
  my_sites <- my_horizons |> dplyr::filter(isA == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(a_total_frags_pct_wtavg = weighted.mean(total_frags_pct, thickness,
                                                                                       na.rm = TRUE)) |>
    dplyr::select(peiid, a_total_frags_pct_wtavg) |>
    dplyr::right_join(my_sites)
  # B frag vol wtavg
  my_sites <- my_horizons |> dplyr::filter(isB == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(b_total_frags_pct_wtavg = weighted.mean(total_frags_pct, thickness,
                                                                                       na.rm = TRUE)) |>
    dplyr::select(peiid, b_total_frags_pct_wtavg) |>
    dplyr::right_join(my_sites)
  # C frag vol wtavg
  my_sites <- my_horizons |> dplyr::filter(isC == TRUE) |>
    dplyr::group_by(peiid) |> dplyr::summarise(c_total_frags_pct_wtavg = weighted.mean(total_frags_pct, thickness,
                                                                                       na.rm = TRUE)) |>
    dplyr::select(peiid, c_total_frags_pct_wtavg) |>
    dplyr::right_join(my_sites)
  # total frag vol wtavg
  my_sites <- my_horizons |>
    dplyr::group_by(peiid) |> dplyr::summarise(total_frags_pct_wtavg = weighted.mean(total_frags_pct, thickness,
                                                                                     na.rm = TRUE)) |>
    dplyr::select(peiid, total_frags_pct_wtavg) |>
    dplyr::right_join(my_sites)

  # Depth class
  my_sites$depth_class <- cut(my_sites$bedrckdepth, c(0, 25, 50, 100, 150, 999),
                              labels = c("VS", "S", "MD", "D", "VD"))

  my_sites$depth_class[is.na(my_sites$depth_class)] <- "VD"

  my_sites$depth_class_num <- dplyr::recode(
    my_sites$depth_class,
    VS = 12.5,
    S  = 37.5,
    MD = 75,
    D  = 125,
    VD = 200
  )

  return(my_sites)
}
