#' Track Leaflet coordinates
#' @description
#' Adds a Javascript Hook to Leaflet map widget that creates a input value
#' called `mousemove` which makes it possible to access map coordinates in a
#' server environment.
#'
#' @param map Leaflet map widget
#' @param id ID of the leaflet widget
#'
#' @noRd
track_coordinates <- function(map, id) {
  map$jsHooks[["render"]] <- c(
    map$jsHooks[["render"]],
    list(list(
      code = sprintf("function(el, x) {
        this.on('mousemove', function(e) {
          var lng = e.latlng.lng;
          var lat = e.latlng.lat;
          var coord = [lng, lat];
          Shiny.onInputChange('%s', coord)
        });
        this.on('mouseout', function(e) {
          Shiny.onInputChange('%s', null)
        })
      }", id, id),
      data = NULL
    ))
  )

  map
}
