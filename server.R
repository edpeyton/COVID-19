
#library(dplyr)
#library(lubridate)
#library(shiny)
#library(viridis)
#library(kableExtra)
#library(leaflet)
#library(shinyAce)
#library(shinythemes)


is.increasing = function(x) {

  x = x[!is.na(x)]

  if (all(x[2:length(x)]>=x[1:(length(x)-1)])) {
    return(1)
  } else {
    return(0)
  }

}


smooth_data = function(x) {


  for (i in unique(x$key_apple_mobility)) {

    data = x %>% dplyr::filter(key_apple_mobility==i)

    for (j in c("confirmed", "deaths", "vaccines", "hosp", "icu")) {

      if (!is.increasing(data %>% dplyr::pull(!!rlang:::sym(j)))) {

      }

    }


  }

}

#shiny::shinyApp(ui = ui,
#server =
function(input, output, session) {

  output$slider = shiny::renderUI({

    x = get_covdata()

    shiny::sliderInput("dates",
                       "Select date",
                       min = min(x$date),
                       max = max(x$date),
                       value = min(x$date),
                       step = 1,
                       width = '600px',
                       ticks = FALSE,
                       animate = shiny::animationOptions(interval = 300, loop = FALSE))

  })


  get_covdata = shiny::reactive({

    COVID19::covid19(country = "USA", level = 2) %>%
      dplyr::ungroup() %>%
      dplyr::select(date, vaccines, tests, confirmed, deaths, hosp, vent, icu, population, key_apple_mobility) %>%
      dplyr::mutate(key_apple_mobility = ifelse(key_apple_mobility=="Washington DC", "District of Columbia", key_apple_mobility)) %>%
      dplyr::rename(key_google_mobility = key_apple_mobility)

  })

  get_states = shiny::reactive({

    geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

  })

  data = shiny::reactive({

    shiny::validate(
      shiny::need(!is.null(input$type_choice), "Waiting for 'type_choice'"),
      shiny::need(!is.null(input$per_choice), "Waiting for 'per_choice'"),
      shiny::need(!is.null(input$dates), "Waiting for slider input")
    )

    covdata = get_covdata()
    states = get_states()

    shiny::validate(
      shiny::need(!is.null(covdata), ""),
      shiny::need(!is.null(states), "")
    )

    covdata = covdata %>% dplyr::ungroup()

    if (input$cumulative=="New") {

      covdata = covdata %>%
        dplyr::arrange(key_google_mobility, date) %>%
        dplyr::mutate(confirmed = pmax(confirmed - dplyr::lag(confirmed, default = 0), 0),
                      deaths = pmax(deaths - dplyr::lag(deaths, default = 0), 0),
                      hosp = pmax(hosp - dplyr::lag(hosp, default = 0), 0),
                      vent = pmax(vent - dplyr::lag(vent, default = 0), 0),
                      icu = pmax(icu - dplyr::lag(icu, default = 0), 0),
                      vaccines = pmax(vaccines - dplyr::lag(vaccines, default = 0), 0))

    }

    if (input$type_choice=="Confirmed cases") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = confirmed/population*10^6)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = confirmed)
      }
    } else if (input$type_choice=="Deaths") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = deaths/population*10^6)
      } else if (input$per_choice=="Per case") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = deaths/confirmed)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = deaths)
      }
    } else if (input$type_choice=="Hospitalizations") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = hosp/population*10^6)
      } else if (input$per_choice=="Per case") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = hosp/confirmed)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = hosp)
      }
    } else if (input$type_choice=="Ventilation") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vent/population*10^6)
      } else if (input$per_choice=="Per case") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vent/confirmed)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vent)
      }
    } else if (input$type_choice=="ICU cases") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = icu/population*10^6)
      } else if (input$per_choice=="Per case") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = icu/confirmed)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = icu)
      }
    } else if (input$type_choice=="Vaccinations") {
      if (input$per_choice=="Per capita") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vaccines/population*10^6)
      } else if (input$per_choice=="Per case") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vaccines/confirmed)
      } else if (input$per_choice=="Total") {
        covdata = covdata %>% dplyr::arrange(key_google_mobility) %>% dplyr::mutate(VAR = vaccines)
      }
    }

    covdata = covdata %>% dplyr::filter(!is.na(key_google_mobility))

    states@data = states@data %>%
      dplyr::select(id, name) %>%
      dplyr::left_join(covdata %>%
                         dplyr::filter(date==input$dates) %>%
                         dplyr::select(key_google_mobility, VAR) %>%
                         dplyr::rename(density = VAR),
                       by = c("name" = "key_google_mobility")) %>%
      dplyr::mutate(RANK = rank(density, na.last = "keep"))

    #if (input$type_choice %in% c("Recovered", "Vaccinations")) {
#
    #  ranks = rank(states@data$density, na.last = "keep")
    #  ranks = tryCatch({max(ranks, na.rm = TRUE) - ranks + min(ranks, na.rm = TRUE)}, warning = function(w) {assign("check", states@data, .GlobalEnv);assign("dates", input$dates, .GlobalEnv)})
#
    #  states@data = states@data %>%
    #    dplyr::mutate(RANK = ranks)
#
    #}

    if (input$cumulative=="New" & input$per_choice=="Total") {

      add0 = NULL
      d = log10(covdata$VAR)
      if (any(is.infinite(d))) {
        add0 = 0
      }
      d = d[is.finite(d)]

      rng = range(floor(d), ceiling(d))
      bins = 10^seq(rng[1], rng[2])
      bins = c(add0, bins)


    } else {

      if (input$per_choice=="Total") {
        bins = unique(floor(quantile(covdata$VAR, seq(0, 1, length.out = 10), na.rm = TRUE)))
        #bins = 10^seq(floor(log10(max(0, min(covdata$VAR, na.rm = TRUE)))), ceiling(log10(max(0, max(covdata$VAR, na.rm = TRUE)))))
      } else {
        bins = unique(quantile(covdata$VAR, seq(0, 1, length.out = 10), na.rm = TRUE))
      }

    }


    bins[1] = 0
    bins = unique(bins)

    return(list(states = states,
                bins = bins))

  })

  shiny::observe({

    if (input$type_choice=="Confirmed cases") {
      shiny::updateSelectInput(inputId = "per_choice", choices = c("Total", "Per capita"))
    } else {
      shiny::updateSelectInput(inputId = "per_choice", choices = c("Total", "Per capita", "Per case"))
    }

  })

  output$map = leaflet::renderLeaflet({

    leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE,
                                                       zoomControl = T,
                                                       minZoom = 2,
                                                       maxZoom = 6,
                                                       dragging = TRUE,
                                                       worldCopyJump  = NULL)) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -90, lat = 35, zoom = 3)

  })

  shiny::observe({

    data = data()

    pal = leaflet::colorBin("Reds", domain = data$states$density, bins = data$bins, reverse = FALSE)

    if (input$per_choice %in% c("Total", "Per capita")) {
      lab_dens = scales::comma(data$states$density, 1)
    } else {
      lab_dens = scales::number(data$states$density, 0.00000001)
    }

    labels = sprintf(paste0("<strong>%s</strong><br/>%s<br/><br/><strong>Rank:</strong> %s"), data$states$name, lab_dens, scales::number(data$states$RANK, accuracy = 1)) %>% lapply(htmltools::HTML)

    leaflet::leafletProxy("map", data = data$states) %>%
      leaflet::addPolygons(
        layerId = data$states$id,
        fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        #highlight = leaflet::highlightOptions(
        #  weight = 3,
        #  color = "#666",
        #  dashArray = "",
        #  fillOpacity = 0.7,
        #  bringToFront = TRUE)#,
        label = labels,
        labelOptions = leaflet::labelOptions(textOnly = FALSE, permanent = FALSE, noHide = FALSE, sticky = TRUE, clickable = FALSE, interactive = FALSE)
      ) %>%
      leaflet::addLegend("bottomright", pal = pal, values = data$states$density, opacity = 0.7, title = NULL, layerId = "colorLegend") %>%
      leaflet::setMaxBounds(lat1 = -70,
                            lat2 = 90,
                            lng1 = -240,
                            lng2 = 120
      )

  })

  output$citation = shiny::renderDataTable({

    COVID19::covid19cite(COVID19::covid19(country = "USA", level = 2), verbose = FALSE)

  })

  output$title = shiny::renderUI({

    shiny::validate(
      shiny::need(!is.null(input$type_choice), ""),
      shiny::need(!is.null(input$per_choice), ""),
      shiny::need(!is.null(input$dates), "")
    )

    shiny::h4(
      shiny::HTML(
      paste0(ifelse(input$cumulative, "Cumulative ", "New "), input$type_choice, " (", ifelse(input$per_choice=="Per capita", paste0("Per 10", tags$sup(6), " people"), input$per_choice), ") ", format(as.Date(input$dates), "%d %b %Y"))
    ))

  })

}
#)
