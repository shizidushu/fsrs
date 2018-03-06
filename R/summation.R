#' Sample mean
#' 
#' @inheritParams base::mean
#' @export
sample_mean <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  val <- sum(x) / n
  val
}


#' Trimmed Mean
#' 
#' @inheritParams  base::mean
#' @description The sample median is an example of what is called atrimmed mean; it trims all but one or two values. The sample mean represents the other extreme: zero trimming.
#' 
#'  No specific amount of trimming is always best, but for various reasons, 20% trimming is often a good choice. This means that the smallest 20% and the largest 20% are trimmed and the average of the remaining data is computed.
#'  
#'  In symbols, first compute 0.2n, round down to the nearest integer, call this result g, in which case the 20\% trimmed mean is given by
#'  
#'  \deqn{\overline{X} =  \frac{1}{n - 2g} (X_{(g+1)} + \cdots + X_{(n-g)})}
#' @export
trimmed_mean <- function(x, trim = 0.2, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  g <- floor(0.2 * n)
  val <- 1/(n - 2*g) * sum( x[(g+1):(n-g)] )
  val
}

#' Sample Median
#' 
#' @inheritParams  base::mean
#' @export
sample_median <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  if ( (n %% 2) != 0 ) {
    m <- (n + 1) / 2
    val <- x[m]
  } else {
    m <- n/2
    val <- (x[m] + x[m+1]) / 2
  }
  val
}