#' @title Get Direction from North
#' @description
#' Function for calculate the angle of the fire run
#' #'
#' @param x1 x coordinate of origin point
#' @param x2 x coordinate of destination point
#' @param y1 y coordinate of origin point
#' @param y2 y coordinate of destination point
#'
#' @return a direction value of a vector calculated from North
#' @export
#'
#' @examples
#' x1<-3
#' y1<-3
#' x2<-4
#' y2<-4
#' DegreesFireRun(x1,y1,x2,y2)
#'
DegreesFireRun <- function(x1, x2, y1, y2) {
  theta <- atan2(y2 - y1, x2 - x1)
  degrees <- ifelse(theta * 180 / pi < 0, theta * 180 / pi + 360, theta * 180 / pi)
  degreesNorth <- ifelse(degrees >= 0 & degrees < 90, -(degrees - 90),
                         ifelse(degrees >= 90 & degrees < 180, -(degrees - 180) + 270,
                                ifelse(degrees >= 180 & degrees < 270, -(degrees - 90) + 360,
                                       -(degrees - 180) + 270
                                )
                         )
  )
  return(degreesNorth)
}
