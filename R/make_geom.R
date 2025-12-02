make_geom <- function(coords, type) {
  geom <- switch(
    type,
    Point = lapply(coords, make_point),
    LineString = lapply(coords, make_line),
    Polygon = lapply(coords, make_poly)
  )

  sf::st_as_sfc(geom, crs = 4326)
}


make_point <- function(coords) {
  sf::st_point(unlist(coords))
}


make_line <- function(coords) {
  coords <- do.call(rbind, coords)
  coords <- matrix(as.numeric(coords), ncol = ncol(coords), nrow = nrow(coords))
  sf::st_linestring(coords)
}


make_poly <- function(coords) {
  coords <- do.call(rbind, coords[[1]])
  coords <- matrix(as.numeric(coords), ncol = ncol(coords), nrow = nrow(coords))
  sf::st_polygon(list(coords))
}
