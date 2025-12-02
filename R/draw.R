#' Draw shapes
#' @description
#' Opens a Shiny app that allows you to draw shapes; then returns the shape
#' as an object of class \code{\link[sf:st_sfc]{sfc}}.
#'
#' @returns If multiple geometry types are drawn (points / lines / polygons),
#' returns a list containing \code{sfc} geometries with possible names
#' \code{Point}, \code{LineString}, and \code{Polygon}. If only a single
#' geometry type is drawn, returns a flat \code{sfc} object.
#'
#' @examples
#' if (interactive()) {
#'   draw()
#' }
draw <- function() {
  shiny::runApp(list(ui = drawsf_ui, server = drawsf_server))
}
