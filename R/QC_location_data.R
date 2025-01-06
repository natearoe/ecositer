#' QC site location data
#'
#' @param veg_df vegetation dataframe, commonly created using `ecositer::create_veg_df()`
#' @param coordinate_format by default, 'NULL' allowing for function to be interactive - otherwise, must be "UTM", "DMS", or "DD".
#'
#' @return dataframe uneeded coordinate format columns removed
#' @export
#'
#' @examples
#' QC_location_data(veg_df = my_veg_df)
QC_location_data <- function(veg_df, coordinate_format = NULL){
  # valid coordinate format, in not NULL
  valid_choices = c("UTM", "DMS", "DD")

  # error handle coordinate format
  if(!is.null(coordinate_format) && !coordinate_format %in% valid_choices){
    stop(sprintf("Invalid coordinate_format. Choose one of: %s", paste(valid_choices, collapse = ", ")))
  }



  # utm df
  utm <- veg_df[, c("horizdatnm", "utmzone", "utmeasting", "utmnorthing")]
  # utm completeness
  utm_compl <- sum(complete.cases(utm)) / nrow(utm) * 100
  message(sprintf("Note -> UTM has %s%% completeness", utm_compl))


  # lat long dms df
  lat_long_dms <- veg_df[, c("horizdatnm", "latdegrees", "latminutes", "latseconds",
                             "latdir", "longdegrees", "longminutes", "longseconds",
                             "longdir")]
  # lat_long_dms_completeness
  lat_long_dms_compl <- sum(complete.cases(lat_long_dms)) / nrow(lat_long_dms) * 100
  message(sprintf("Note -> Lat/long DMS has %s%% completeness", lat_long_dms_compl))



  # lat long dd
  lat_long_dd <- veg_df[, c("latstDD", "longstDD")]
  # lat long dd completeness
  lat_long_dd_compl <- sum(complete.cases(lat_long_dd)) / nrow(lat_long_dd) * 100
  message(sprintf("Note -> Standardized Lat/long DD has %s%% completeness. These columns in NASIS imply WGS84 datum is used,
                  therefore datum column is not inspected.", lat_long_dd_compl))


  choose_coordinate_format <- function() {
    # Display options to the user
    cat("What coordinate format do you want to use?\n")
    cat("1. UTM\n")
    cat("2. Lat/long DMS\n")
    cat("3. Lat/long DD\n")

    # Prompt user for input
    choice <- as.integer(readline(prompt = "Enter the number corresponding to your choice: "))

    # Check if the input is valid
    if (!choice %in% c(1, 2, 3)) {
      stop("Invalid choice. Please select 1, 2, or 3.")
    }

    # Execute code based on user choice
    if (choice == 1) {
      message("You selected UTM. Processing with UTM coordinates...")
      # remove non-UTM columns
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(lat_long_dms)[colnames(lat_long_dms) != "horizdatnm"],
                                               colnames(lat_long_dd))]
    } else if (choice == 2) {
      message("You selected Lat/long DMS. Processing with Lat/Long DMS coordinates...")
      # remove non-UTM columns
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(utm)[colnames(utm) != "horizdatnm"],
                                               colnames(lat_long_dd))]
    } else if (choice == 3) {
      message("You selected Lat/long DD. Processing with Lat/long DD coordinates...")
      # remove non-UTM columns
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(lat_long_dms)[colnames(lat_long_dms) != "horizdatnm"],
                                               colnames(utm)[colnames(utm) != "horizdatnm"])]
    }

    # Return the selected choice as a value
    return(veg_df)
  }

  # if coordinate_format == NULL, perform interactive functionality
  if(is.null(coordinate_format)){
    selected_format <- choose_coordinate_format()
    return(selected_format)
  }

  # if coordinate_format not NULL..
  if(coordinate_format %in% valid_choices){
    # UTM
    if(coordinate_format == "UTM"){
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(lat_long_dms)[colnames(lat_long_dms) != "horizdatnm"],
                                               colnames(lat_long_dd))]
    }
    # DMS
    if(coordinate_format == "DMS"){
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(utm)[colnames(utm) != "horizdatnm"],
                                               colnames(lat_long_dd))]
    }
    # DD
    if(coordinate_format == "DD"){
      veg_df <- veg_df[, !names(veg_df) %in% c(colnames(lat_long_dms)[colnames(lat_long_dms) != "horizdatnm"],
                                               colnames(utm)[colnames(utm) != "horizdatnm"])]
    }

    return(veg_df)

  }

  return(veg_df)

}
