#' Draw shapes
#' @description
#' Opens a Shiny app that allows you to draw shapes; then returns the shape
#' as an object of class \code{\link[sf:st_sfc]{sfc}}.
#'
#' @param geom An object of class \code{sf} or \code{sfc}. The geometry is
#'   imported to the app and can be edited. Imported geometries are shown with
#'   black outlines while newly created geometries are shown in blue outlines.
#'
#' @returns If multiple geometry types are drawn (points / lines / polygons),
#' returns a list containing \code{sfc} geometries with possible names
#' \code{Point}, \code{LineString}, and \code{Polygon}. If only a single
#' geometry type is drawn, returns a flat \code{sfc} object.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   draw()
#' }
draw <- function(geom = NULL) {
  if (!is.null(geom)) {
    if (!inherits(geom, c("sf", "sfc"))) {
      stop("Argument `geom` must be an object of class sf or sfc.")
    }

    if (is.na(sf::st_crs(geom))) {
      stop("Argument `geom` must have a defined")
    }

    geom <- sf::st_transform(geom, 4326)
    assign("geom", geom, envir = shenv)
  }

  shiny::runApp(list(ui = drawsf_ui, server = drawsf_server))
}


shenv <- new.env(parent = emptyenv())
