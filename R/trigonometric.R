# Reference to 369  Thomas Calculus Early Transcendentals 13th

#### Angles --------------------------------------------------------####

#' The length of an arc
#' 
#' @param radius The radius of a circle
#' @param theta the angle which the arc subtends at the centre of the circle, in radians
#' @export
arc_length <- function(radius, theta) {
  arc <- radius * theta
  arc
}


#' Convert radian to degree
#'
#' @param theta angle in radians
#'
#' @details 1 radian = 180/pi (about 57.3) degrees
#' @export
to_degree <- function(theta) {
  radian_1_in_degree <- 180 / pi
  radian_1_in_degree * theta
}


#' Convert degree to radian
#' 
#' @param angle in degrees
#' @details 1 degree = pi / 180 (about 0.017) radians
#' @export
to_radian <- function(theta) {
  degree_1_in_theta <- pi / 180
  degree_1_in_theta * theta
}




#### The Six Basic Trigonometric Functions ------------------------####

#' sine
#' 
#' @param y y coordinate in a circle or opposite of an acute angle in terms of a right triangle
#' @param r radius or hypotenuse of an acute angle in terms of a right triangle
#' @export
sine <- function(y, r) {
  y / r
}

#' cosecant
#' 
#' @inherit  sine
#' @export
cosecant <- function(r, y) {
  r / y
}


#' cosine
#' 
#' @param x x coordinate in a circle or adjacent of an acute angle in terms of a right triangle
#' @inheritParams sine 
#' @export
cosine <- function(x, r) {
  x / r
}

#' secant
#' 
#' @inherit cosine 
#' @export
secant <- function(r, x) {
  r / x
}

#' tangent
#' 
#' @inherit sine
#' @inherit cosine
#' @export
tangent <- function(y, x) {
  y / x
}

#' cotangent
#' 
#' @inherit tangent
#' @export
cotangent <- function(x, y) {
  x / y
}