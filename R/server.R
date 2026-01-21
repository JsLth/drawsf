#' @rdname drawsf_ui
#' @export
drawsf_server <- function(input, output, session) {
  shapes <- shiny::reactiveVal()

  output$map <- leaflet::renderLeaflet({
    map <- leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::setView(14, 48, 5) |>
      leaflet.extras::addDrawToolbar(
        targetGroup = "draw",
        editOptions = leaflet.extras::editToolbarOptions(),
        polylineOptions = leaflet.extras::drawPolylineOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
            weight = 1,
            opacity = 1,
            fillOpacity = 1,
            color = "black"
          )
        ),
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
            weight = 1,
            opacity = 1,
            fillOpacity = 0.1,
            color = "black"
          )
        ),
        circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(
          weight = 4,
          opacity = 1,
          fillOpacity = 1,
          color = "black"
        ),
        circleOptions = leaflet.extras::drawCircleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
            weight = 1,
            opacity = 1,
            fillOpacity = 0.1,
            color = "black"
          )
        )
      ) |>
      leaflet.extras::addStyleEditor() |>
      leaflet.extras::addMeasurePathToolbar() |>
      leaflet.extras::enableMeasurePath() |>
      track_coordinates("mousemove")

    if (exists("geom", envir = shenv)) {
      geom <- get("geom", envir = shenv)
      geom_type <- unique(sf::st_geometry_type(geom))

      if (length(geom_type) > 1) {
        stop("Argument `geom` must contain only a single geometry type.")
      }

      map <- case_switch(
        geom_type %in% c("POINT", "MULTIPOINT") ~ map |>
          leaflet::addCircleMarkers(
            data = geom,
            radius = 2,
            stroke = FALSE,
            weight = 5,
            opacity = 1,
            fillOpacity = 1,
            color = "black",
            group = "draw"
          ),

        geom_type %in% c("LINESTRING", "MULTILINESTRING") ~ map |>
          leaflet::addPolylines(
            data = geom,
            weight = 5,
            opacity = 1,
            fillOpacity = 1,
            color = "black",
            group = "draw"
          ),

        geom_type %in% c("POLYGON", "MULTIPOLYGON") ~ map |>
          leaflet::addPolygons(
            data = geom,
            weight = 1,
            opacity = 1,
            fillOpacity = 0.1,
            color = "black",
            group = "draw"
          )
      )
    }

    map
  })


  output$coord_track <- shiny::renderUI({
    pt <- input$mousemove

    if (!is.null(pt)) {
      pt <- round(pt, 6)
    }

    shiny::div(
      shiny::tags$b("Lon:", pt[1] %||% "N/A"), shiny::tags$br(),
      shiny::tags$b("Lat:", pt[2] %||% "N/A"),
    )
  })

  shiny::observe({
    feats <- input$map_draw_all_features$features
    #ids <- vapply(feats, \(x) as.character(x$properties$`_leaflet_id`), character(1))
    coords <- lapply(feats, \(x) x$geometry$coordinates)
    types <- vapply(feats, \(x) x$geometry$type, character(1))

    utypes <- unique(types)
    names(utypes) <- utypes
    geoms <- lapply(utypes, function(type) {
      is_type <- which(types == type)
      if (length(is_type)) {
        coords <- coords[is_type]
        make_geom(coords, type)
      }
    })

    shapes(geoms)
  }) |>
    shiny::bindEvent(input$map_draw_stop, input$map_draw_editstop)

  out <- NULL
  shiny::observe({
    out <<- shapes()
  }) |>
    shiny::bindEvent(shapes())

  shiny::observe(stop_and_return(out)) |> shiny::bindEvent(input$close)
  shiny::onSessionEnded(function() stop_and_return(out))
}


stop_and_return <- function(out) {
  init_geom <- get0("geom", envir = shenv)

  if (length(out) == 1) out <- out[[1]]
  if (!length(out)) {
    out <- NULL

    if (!is.null(init_geom)) {
      out <- init_geom
    }
  }

  suppressWarnings(rm("geom", envir = shenv))
  shiny::stopApp(out)
}
