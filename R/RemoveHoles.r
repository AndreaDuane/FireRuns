#' @title Remove holes in polygon but keep previous front
#' @description
#' Function for make a polygon without holes if the front is all within the polygon, so keeping the previous front 'hole'
#' #'
#' @param x spatVector object corresponding to the evaluated polygon
#' @param Front spatVector line corresponding to the previous front
#'
#' @return a spatVector object without holes except for the previous front
#' @import terra
#' @export
#'
#'
remove.holes.withinFront <- function(x, Front) {
  FrontAsHole <- terra::as.polygons(Front)
  GeomX <- terra::geom(x)
  GeomFront <- terra::geom(FrontAsHole)
  z <- rbind(
    cbind(object = 1, part = 1, GeomX[, 3:4], hole = 0),
    cbind(object = 1, part = 1, GeomFront[, 3:4], hole = 1)
  )
  colnames(z)[3:4] <- c("x", "y")
  p <- terra::vect(z, "polygons", atts = data.frame(id = 1))
  return(p)
}
