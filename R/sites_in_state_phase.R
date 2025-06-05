#' Identify siteid(s)/vegplotid(s) in states/phases of STMs
#'
#'`sites_in_state_phase` is a way of determining what siteids are associated with
#' an ecosite state/phase.
#'
#' @param veg_df a formatted vegetation dataframe created using
#' \link[ecositer]{formatted_veg_df}
#'
#' @return a nested list of ecosites - states/phases - and the siteids associated
#' with each state/phase
#' @export
#'
#' @examples
#' example_data <- access_example_data()
#' veg_df <- formatted_veg_df(static_location = example_data)
#' # user-defined static location -
#' # veg_df <- formatted_veg_df(static_location =
#' "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite")
#' my_state_phase <- sites_in_state_phase(veg_df = veg_df)
#' head(my_state_phase)
#'
sites_in_state_phase <- function(veg_df){

  veg_df$akfieldecositeid <- ifelse(is.na(veg_df$akfieldecositeid),
                                           "undefined",
                                           veg_df$akfieldecositeid)
  STM <- list()
  for(i in unique(veg_df$ecositeid)){
    for(j in unique(veg_df |> dplyr::filter(ecositeid == i) |>
                    dplyr::pull(akfieldecositeid))){
      STM[[i]][[j]] <- veg_df |> dplyr::filter(ecositeid == i) |>
        dplyr::filter(akfieldecositeid == j) |>
        dplyr::select(akfieldecositeid, vegplotid) |>
        unique()
    }
  }

  return(STM)

}



