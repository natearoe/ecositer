#' Polar Scatter Plot of NASIS Site Data: Aspect, Elevation, and Ecologial Site
#' @description Polar Scatter Plot of NASIS Site Aspect and Elevation. Utilizes elevation, aspect, and site ecological site history data from NASIS site tables. Desired sites to plot need to be loaded into your NASIS selected set.
#' @param
#'
#' @return a polar scatterplot of site aspect and elevation - up to 10 ecosites
#' @export
#'
#' @examples asp_elev_plot()
asp_elev_plot <- function() {
  #
  #get_NASIS for site data
  site <- soilDB::get_site_data_from_NASIS_db(
    SS = TRUE,
    stringsAsFactors = default.stringsAsFactors(),
    dsn = NULL
  )
  asp_elev <- site[,c("site_id","ecositeid", "ecositenm","elev_field","aspect_field","ecositenm")]
  asp_elev <- asp_elev |>
    dplyr::mutate(tmp = match(ecositeid, unique(ecositeid)))
  #Create ecosite specific datframes: Change desired ecosite ID's
  asp_elev1 <- asp_elev[asp_elev$tmp == "1",]
  asp_elev2 <- asp_elev[asp_elev$tmp == "2",]
  asp_elev3 <- asp_elev[asp_elev$tmp == "3",]
  asp_elev4 <- asp_elev[asp_elev$tmp == "4",]
  asp_elev5 <- asp_elev[asp_elev$tmp == "5",]
  asp_elev6 <- asp_elev[asp_elev$tmp == "6",]
  asp_elev7 <- asp_elev[asp_elev$tmp == "7",]
  asp_elev8 <- asp_elev[asp_elev$tmp == "8",]
  asp_elev9 <- asp_elev[asp_elev$tmp == "9",]
  asp_elev10 <- asp_elev[asp_elev$tmp == "10",]
  asp_elev10 <- asp_elev[asp_elev$tmp == "10",]
  #Plot up to 3 site data sets based on ecosite
  fig <-
    plotly::plot_ly(
      type = 'scatterpolar',
      r = asp_elev1$elev_field,
      theta = asp_elev1$aspect_field,
      mode = 'markers',
      name = asp_elev1$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev2$elev_field,
      theta = asp_elev2$aspect_field,
      name = asp_elev2$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev3$elev_field,
      theta = asp_elev3$aspect_field,
      name = asp_elev3$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev4$elev_field,
      theta = asp_elev4$aspect_field,
      name = asp_elev4$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev5$elev_field,
      theta = asp_elev5$aspect_field,
      name = asp_elev5$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev6$elev_field,
      theta = asp_elev6$aspect_field,
      name = asp_elev6$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev7$elev_field,
      theta = asp_elev7$aspect_field,
      name = asp_elev7$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev8$elev_field,
      theta = asp_elev8$aspect_field,
      name = asp_elev8$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev9$elev_field,
      theta = asp_elev9$aspect_field,
      name = asp_elev9$ecositeid
    )
  fig <- fig |>
    plotly::add_trace(
      r = asp_elev10$elev_field,
      theta = asp_elev10$aspect_field,
      name = asp_elev10$ecositeid
    )|>
  plotly::layout(polar = list(
      angularaxis  = list(
        rotation = 90,
        direction = "clockwise"),
      radialaxis = list(
        rangemode = "normal",
        autorange = "reversed"
      )
    ))
  #Display plot
  fig}

