drawsf_server <- function(input, output, session) {
  shapes <- shiny::reactiveVal()

  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::setView(14, 48, 5) |>
      leaflet.extras::addDrawToolbar(
        targetGroup = "draw",
        editOptions = leaflet.extras::editToolbarOptions()
      ) |>
      leaflet.extras::addStyleEditor() |>
      leaflet.extras::addMeasurePathToolbar() |>
      leaflet.extras::enableMeasurePath() |>
      track_coordinates("mousemove")
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
    ids <- vapply(feats, \(x) as.character(x$properties$`_leaflet_id`), character(1))
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
  if (length(out) == 1) out <- out[[1]]
  if (!length(out)) out <- NULL
  shiny::stopApp(out)
}
