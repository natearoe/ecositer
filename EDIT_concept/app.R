#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(soilDB)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(xml2)
library(kableExtra)
library(Hmisc)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("EDIT prototype"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "ecositeid",
                label = "Ecological Site ID:",
                value = "F022AB100CA"),
      selectInput(inputId = "ecosite_property",
                  label = "Ecological Site Property:",
                  choices = list(Physiographic = c("elevation",
                                                   "slope")),
                  selected = "elevation"),
      textInput("nonModal",
                label = "List of non-modal cokey(s) (comma seperated)",
                value = "",
                width = '400px'),
      radioButtons(inputId = "acreage",
                   label = "Component acreage weighting?",
                   choices = c("No.", "Yes."))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Stats",
                           br(),
                           selectInput(inputId = 'DensityHisto',
                                       label = 'Figure type',
                                       choices = c("Density plot",
                                                   "Histogram"),
                                       selected = "Density plot"),
                           uiOutput("property_plot"),
                           br(),
                           htmlOutput("property_text"),
                           br(),
                           htmlOutput("highlow")),
                  tabPanel("Map",
                           leafletOutput("mapplot")),
                  tabPanel("About",
                           htmlOutput("about")))


    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  property_list <- list(elevation = c("elev_l", "elev_r", "elev_h"),
                        slope = c("slope_l", "slope_r", "slope_h"))

  numb_bins <- 10
  my_colors <- c("#B3E2CD", "#FDCDAC", "#CBD5E8")



  df <- reactive({

    # Write SQL query with reactive ecositeid term
    q  <- paste(
      "SELECT
        legend.lkey, mapunit.mukey, component.majcompflag,
        (CAST(comppct_r AS NUMERIC)/100 ) * CAST(muacres AS NUMERIC) AS compacres,
        component.compkind, component.cokey, coecoclass.ecoclassid, copm.pmkind,
        component.elev_l, component.elev_h, component.elev_r, component.slope_l,
        component.slope_r, component.slope_h, CAST(comppct_r AS NUMERIC) AS comppct,
        CAST(muacres AS NUMERIC) AS muacres
        FROM legend
        INNER JOIN mapunit ON legend.lkey = mapunit.lkey
        INNER JOIN component ON mapunit.mukey = component.mukey
        INNER JOIN coecoclass ON component.cokey = coecoclass.cokey
        INNER JOIN copmgrp ON component.cokey = copmgrp.cokey
        INNER JOIN copm ON copmgrp.copmgrpkey = copm.copmgrpkey
        WHERE ecoclassid =", paste0("'", input$ecositeid, "'"))

    # q  <- paste(
    #   "SELECT
    #     legend.lkey, mapunit.mukey, component.majcompflag,
    #     (CAST(comppct_r AS NUMERIC)/100 ) * CAST(muacres AS NUMERIC) AS compacres,
    #     component.compkind, component.cokey, coecoclass.ecoclassid, copm.pmkind,
    #     component.elev_l, component.elev_h, component.elev_r, component.slope_l,
    #     component.slope_r, component.slope_h, CAST(comppct_r AS NUMERIC) AS comppct
    #     FROM legend
    #     INNER JOIN mapunit ON legend.lkey = mapunit.lkey
    #     INNER JOIN component ON mapunit.mukey = component.mukey
    #     INNER JOIN coecoclass ON component.cokey = coecoclass.cokey
    #     INNER JOIN copmgrp ON component.cokey = copmgrp.cokey
    #     INNER JOIN copm ON copmgrp.copmgrpkey = copm.copmgrpkey
    #     WHERE ecoclassid =", paste0("'", "F022AB100CA", "'"))


    # q <- paste(
    #   "SELECT
    #     legend.lkey, mapunit.mukey, component.majcompflag,
    #     (CAST(comppct_r AS NUMERIC)/100 ) * CAST(muacres AS NUMERIC) AS compacres,
    #     component.compkind, component.cokey, coecoclass.ecoclassid, copm.pmkind,
    #     component.elev_l, component.elev_h, component.elev_r, component.slope_l,
    #     component.slope_r, component.slope_h
    #     FROM legend
    #     INNER JOIN mapunit ON legend.lkey = mapunit.lkey
    #     INNER JOIN component ON mapunit.mukey = component.mukey
    #     INNER JOIN coecoclass ON component.cokey = coecoclass.cokey
    #     INNER JOIN copmgrp ON component.cokey = copmgrp.cokey
    #     INNER JOIN copm ON copmgrp.copmgrpkey = copm.copmgrpkey
    #     WHERE ecoclassid =", paste0("'", "F022AB100CA", "'"))


    # Run SDA query
    res <- soilDB::SDA_query(q)

    # Remove coiid(s)/cokey(s) defined by user


    rem_fun <- if(input$nonModal == ""){
      ""
    } else { unlist(strsplit(x = input$nonModal,
                             split = ","))}

    rem_fun <- gsub(" ", "", rem_fun, fixed = TRUE)

    # Remove miscellaneous areas
    res <- res |> dplyr::filter(compkind != "miscellaneous area") |>
      dplyr::filter(!cokey %in% rem_fun)

    res

  })

  map <- reactive({
    my_map <- fetchSDA_spatial(input$ecositeid, by.col="ecoclassid") |>
      dplyr::left_join(df() |> dplyr::select(mukey, comppct))

  })

  # comp_acreage_unit <- reactive({
  #   # Determine component acreage unit
  #   min_acres <- min(df()$compacres[df()$compacres != 0], na.rm = TRUE)
  #
  #   ifelse(min_acres < 100, 1,
  #                               ifelse(min_acres < 1000, 10,
  #                                      ifelse(min_acres < 10000, 100,
  #                                             ifelse(min_acres <
  #                                                      100000, 1000, 10000))))
  # })



  cols <- reactive({
    property_list[[input$ecosite_property]]
  })

  sub.df <- reactive({
    my.df <- df() |> dplyr::select(cokey, cols(), compacres) |>
      tidyr::pivot_longer(
        names_to = "property",
        values_to = "value",
        cols = cols()) |>
      dplyr::rename(acres = compacres)

    my.df$property <- factor(my.df$property, levels = cols(),
                             labels = c("low", "rep.", "high"))

    my.df
  })

  triangle_sampling <- reactive({
    sapply(seq(nrow(df())), function(i)
      rtriangle(
        n = 1000,
        a = df()[i, cols()[1]],
        b = df()[i, cols()[3]],
        c = df()[i, cols()[2]]
      )) |> as.vector()
  })


  output$property <- renderPlotly({
    if(input$ecosite_property == "elevation"){

      # Create df, make universal, assign factor levels
      # sub.df <-
      #   df() |>  dplyr::select(cokey, elev_l, elev_r,  elev_h, compacres) |>
      #   tidyr::pivot_longer(
      #     names_to = "property",
      #     values_to = "value",
      #     cols = c("elev_l", "elev_r", "elev_h")) |>
      #   dplyr::rename(acres = compacres)

      # sub.df$property <- factor(sub.df$property, levels = c("elev_l", "elev_r", "elev_h"),
      #                        labels = c("low", "rep.", "high"))





      unit_denoms <- c(1, 25, 50, 100, 250, 500, 1000, 2500, 5000)
      unit_vector <- diff(range(min(sub.df()$value), max(sub.df()$value)))/unit_denoms
      # unit_vector <- diff(range(res$elev_h, res$elev_l))/unit_denoms

      target_index <- which(abs(unit_vector - numb_bins) == min(abs(unit_vector - numb_bins)))
      bin_width <- unit_denoms[target_index]

      my_seq <- seq(plyr::round_any(min(sub.df()$value),
                                    accuracy = bin_width,
                                    f = floor),
                    plyr::round_any(max(sub.df()$value),
                                    accuracy = bin_width,
                                    f = ceiling),
                    bin_width)

      dens_quantiles <- quantile(triangle_sampling(), p = c(0.05, 0.25, 0.5, 0.75, 0.95))

      # elev.df <-
      #   res |>  dplyr::select(cokey, elev_h, elev_l, elev_r, compacres) |>
      #   tidyr::pivot_longer(
      #     names_to = "elev",
      #     values_to = "value",
      #     cols = c("elev_h", "elev_l", "elev_r")) |>
      #   dplyr::rename(acres = compacres)
      if(input$DensityHisto == "Density plot"){
        mp <- ggplot(mapping = aes(triangle_sampling())) + ggplot2::geom_density() +
          ylab("Probability density") + xlab("Elevation (ft.)") +
          annotate(geom = "text", x = dens_quantiles, y = 0, label = names(dens_quantiles)) +
          geom_vline(xintercept = dens_quantiles, linetype = "longdash")
        plotly::ggplotly(mp)
      } else if (input$DensityHisto == "Histogram"){

        if(input$acreage == "No."){
          my_plot <-
            ggplot2::ggplot(sub.df(), ggplot2::aes(x = value, fill = property)) +
            ggplot2::geom_histogram(
              ggplot2::aes(text = paste("cokey(s):", cokey)),
              binwidth = bin_width,
              center = min(my_seq) + bin_width / 2,
              color = "black",
              closed = "left"
            ) +
            ggplot2::scale_fill_manual(values = my_colors) +
            ggplot2::ggtitle("Summary of Elevation Ranges") + ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5),
              axis.text.x = ggplot2::element_text(angle = 35)
            ) +
            ggplot2::ylab("# of components") + ggplot2::xlab("Elevation (ft.)") +
            ggplot2::scale_x_continuous(limits = c(min(my_seq), max(my_seq)),
                                        breaks = my_seq[seq(1, length(my_seq), by = 2)]) +
            ggplot2::labs(fill = "Component range")

          plotly::ggplotly(my_plot)
        } else if (input$acreage == "Yes.") {
          my_plot <-
            ggplot2::ggplot(sub.df(), ggplot2::aes(x = value, fill = property,
                                                   weight = acres)) +
            ggplot2::geom_histogram(
              ggplot2::aes(text = paste("cokey(s):", cokey)),
              binwidth = bin_width,
              center = min(my_seq) + bin_width / 2,
              color = "black",
              closed = "left"
            ) +
            ggplot2::scale_fill_manual(values = my_colors) +
            ggplot2::ggtitle("Summary of Elevation Ranges") + ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5),
              axis.text.x = ggplot2::element_text(angle = 35)
            ) +
            ggplot2::ylab("# of components") + ggplot2::xlab("Elevation (ft.)") +
            ggplot2::scale_x_continuous(limits = c(min(my_seq), max(my_seq)),
                                        breaks = my_seq[seq(1, length(my_seq), by = 2)]) +
            ggplot2::labs(fill = "Component range")

          plotly::ggplotly(my_plot)
        }
      }





    } else if (input$ecosite_property == "slope"){
      # Create df, make universal, assign factor levels
      # sub.df() <-
      #   df() |>  dplyr::select(cokey, slope_l, slope_r,  slope_h, compacres) |>
      #   tidyr::pivot_longer(
      #     names_to = "property",
      #     values_to = "value",
      #     cols = c("slope_l", "slope_r", "slope_h")) |>
      #   dplyr::rename(acres = compacres)
      #
      # sub.df()$property <- factor(sub.df()$property, levels = c("slope_l", "slope_r", "slope_h"),
      #                           labels = c("low", "rep.", "high"))



      unit_denoms <- c(1, 2, 5, 10, 15, 20)
      unit_vector <- diff(range(min(sub.df()$value), max(sub.df()$value)))/unit_denoms
      # unit_vector <- diff(range(res$elev_h, res$elev_l))/unit_denoms

      target_index <- which(abs(unit_vector - numb_bins) == min(abs(unit_vector - numb_bins)))
      bin_width <- unit_denoms[target_index]

      my_seq <- seq(plyr::round_any(min(sub.df()$value),
                                    accuracy = bin_width,
                                    f = floor),
                    plyr::round_any(max(sub.df()$value),
                                    accuracy = bin_width,
                                    f = ceiling),
                    bin_width)

      if(input$acreage == "No."){
        my_plot <-
          ggplot2::ggplot(sub.df(), ggplot2::aes(x = value, fill = property)) +
          ggplot2::geom_histogram(
            ggplot2::aes(text = paste("cokey(s):", cokey)),
            binwidth = bin_width,
            center = min(my_seq) + bin_width / 2,
            color = "black",
            closed = "left"
          ) +
          ggplot2::scale_fill_manual(values = my_colors) +
          ggplot2::ggtitle("Summary of Slope Ranges") + ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)) +
          ggplot2::ylab("# of components") + ggplot2::xlab("Slope (%)") +
          ggplot2::scale_x_continuous(limits = c(min(my_seq), max(my_seq)),
                                      breaks = my_seq[seq(1, length(my_seq), by = 2)]) +
          ggplot2::labs(fill = "Component range")

        plotly::ggplotly(my_plot)
      } else if (input$acreage == "Yes."){
        my_plot <-
          ggplot2::ggplot(sub.df(), ggplot2::aes(x = value, fill = property,
                                                 weight = acres)) +
          ggplot2::geom_histogram(
            ggplot2::aes(text = paste("cokey(s):", cokey)),
            binwidth = bin_width,
            center = min(my_seq) + bin_width / 2,
            color = "black",
            closed = "left"
          ) +
          ggplot2::scale_fill_manual(values = my_colors) +
          ggplot2::ggtitle("Slope representative") + ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)) +
          ggplot2::ylab("# of components") + ggplot2::xlab("Slope (%)") +
          ggplot2::scale_x_continuous(limits = c(min(my_seq), max(my_seq)),
                                      breaks = my_seq[seq(1, length(my_seq), by = 2)]) +
          ggplot2::labs(fill = "Component range")

        plotly::ggplotly(my_plot)
      }




    }
  })

  # TO DO: This error handling should probably be moved to a reactive() element
  #        that can then be applied into property_plot, property_text, and highlow
  output$property_plot <- renderUI({
    if(input$ecosite_property == "elevation"){
      if(all(is.na(df()$elev_h)) &
         all(is.na(df()$elev_l))){
        p("No elevation data populated.")
      } else if (all(is.na(df()$muacres)) & input$acreage == "Yes."){
        p("All mapunits missing acreage.")
      } else {plotlyOutput("property")}

    } else if (input$ecosite_property == "slope"){
      if(all(is.na(df()$slope_h)) &
         all(is.na(df()$slope_l))){
        p("No slope data populated.")
      } else if (all(is.na(df()$muacres)) & input$acreage == "Yes."){
        p("All mapunits missing acreage.")
      } else {plotlyOutput("property")}
    }







  })

  # output$property_text <- renderText({
  #   print(cols())
  # })

  output$property_text <- renderText({
    if (input$acreage == "No."){
      my_sumry <- summary(sub.df()$value)
      data.frame(Value = c(my_sumry[1],
                           quantile(sub.df()$value, probs = 0.05),
                           my_sumry[2],
                           my_sumry[3],
                           my_sumry[4],
                           my_sumry[5],
                           quantile(sub.df()$value, probs = 0.95),
                           my_sumry[6]), check.names = FALSE) |> t() |>
        knitr::kable("html", align = "l") |>
        kable_styling("striped", full_width = F)
    } else if (input$acreage == "Yes."){
      my_sumry <- wtd.quantile(sub.df()$value,
                               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1.0),
                               weights = sub.df()$acres, )
      my.df <- data.frame(Value = c(my_sumry[1],
                                    my_sumry[2],
                                    my_sumry[3],
                                    my_sumry[4],
                                    wtd.mean(sub.df()$value, weights = sub.df()$acres),
                                    my_sumry[5],
                                    my_sumry[6],
                                    my_sumry[7]))

      row.names(my.df) <- c("Min.", "5%", "1st Qu.", "Median", "Mean", "3rd Qu.",
                            "95%", "Max.")
      my.df |> t() |>
        knitr::kable("html", align = "l") |>
        kable_styling("striped", full_width = F)
    }


  })

  output$highlow <- renderText({
    if (input$acreage == "No."){
      my.df <- data.frame(Representative = c(quantile(sub.df()$value, probs = 0.2),
                                             quantile(sub.df()$value, probs = 0.8)),
                          Actual = c(quantile(sub.df()$value, probs = 0.05),
                                     quantile(sub.df()$value, probs = 0.95)))
      rownames(my.df) <- c("low", "high")
      my.df |>
        knitr::kable("html", align = "l") |>
        kable_styling("striped", full_width = F)
    } else if (input$acreage == "Yes."){
      my.df <- data.frame(Representative = c(wtd.quantile(sub.df()$value, probs = 0.2,
                                                          weights = sub.df()$acres),
                                             wtd.quantile(sub.df()$value, probs = 0.8,
                                                          weights = sub.df()$acres)),
                          Actual = c(wtd.quantile(sub.df()$value, probs = 0.05,
                                                  weights = sub.df()$acres),
                                     wtd.quantile(sub.df()$value, probs = 0.95,
                                                  weights = sub.df()$acres)))
      rownames(my.df) <- c("low", "high")
      my.df |>
        knitr::kable("html", align = "l") |>
        kable_styling("striped", full_width = F)
    }


  })

  output$about <- renderText({
    HTML(paste0("<b>","Basic description: ","</b>", "<br>", "This website is a demonstration of how a shinyapp can be used to querying Soil Data Access
                  and return NRCS Soil Survey map unit component data. Map unit components are correlated to ecological sites. Therefore,
                  all components correlated to an ecological site can be queried. The properties of those components are then aggregated into
                  visualizations and summaries to represent the properties of the ecological site. This method is capable of displaying information
                  on any ecological site with correlations in the SSURGO data model. It does not require that an ecological site is published in
                  EDIT, as this app has no connection with EDIT."))
  })

  output$mapplot <- renderLeaflet({
    m <- mapview::mapView(map(), zcol = "comppct", layer.name = "Component %")
    m@map
  })


}

# Run the application
shinyApp(ui = ui, server = server)
