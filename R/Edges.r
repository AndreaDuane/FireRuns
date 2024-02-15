#' @title Create Fronts object
#' @description Function for create a Vector that returns the front between polygons in an isochrone map
#'
#' @param AllPols Isochrone map Spatvector polygon object
#' @param nameFeho Name of hour ID field in polygon file
#'
#' @return a Spatvector object with lines between polygons (fronts)
#' @import terra
#'
#' @export
#'
#'
Edges <- function(AllPols, nameFeho) {
  for (j in 1:nrow(AllPols)) {
    Polbuf <- terra::buffer(AllPols, width = 10)
    Polbuf <- terra::buffer(Polbuf, width = 0)
    qq <- AllPols[j, ]
    qq <- terra::as.lines(qq)
    lines <- terra::intersect(qq, Polbuf[!Polbuf[[nameFeho, drop = T]] %in% qq[[nameFeho, drop = T]], ])
    if (exists("lines")) {
      if (exists("finalLines")) {
        finalLines <- rbind(finalLines, lines)
      }
      if (!exists("finalLines")) {
        finalLines <- lines
      }
    }
  }
  return(finalLines)
}
