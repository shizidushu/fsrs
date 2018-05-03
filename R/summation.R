#' Sample mean
#'
#' @inheritParams base::mean
#' @export
sample_mean <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
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
  if (na.rm)
    x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  g <- floor(0.2 * n)
  val <- 1 / (n - 2 * g) * sum(x[(g + 1):(n - g)])
  val
}

#' Sample Median
#'
#' @inheritParams  base::mean
#' @export
sample_median <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  if ((n %% 2) != 0) {
    m <- (n + 1) / 2
    val <- x[m]
  } else {
    m <- n / 2
    val <- (x[m] + x[m + 1]) / 2
  }
  val
}


#' Compute quartiles based on ideal fourths
#'
#' @param x numeric vector whose quantiles are wanted.
#' @inheritParams  stats::quantile
#' @return The lower quantile and upper quantile
#' @details 
#' The sample median divides the data into two parts: the lower half and the upper half after putting the observations in ascending order.
#' 
#'  \emph{Quartiles} are measures of location aimed at dividing data into four parts. This is done with two additional measures of location called the lower and upper quartiles. The median is sometimes called the middle quartile.
#'  
#'   Roughly, the \emph{lower quartile} is the median of the smaller half of the data and the \emph{upper quartile} is the median of the upper half. 
#' @export
idealf <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  # let j to be the integer portion of (n/4) + (5/12)
  j <- floor(length(x) / 4 + 5 / 12)
  # let h = n/4 + 5/12 - j
  h <- length(x) / 4 + 5 / 12 - j
  # sort x
  x <- sort(x)
  # get lower quantile
  ql <- (1 - h) * x[j] +  h * x[j + 1]
  # let k = n - j + 1
  k <- length(x) - j + 1
  # get upper quantile
  qu <- (1 - h) * x[k] +  h * x[k - 1]
  list(ql = ql, qu = qu)
}


#' Deviation Scores
#' 
#' @param x numeric vectors whose deviation scores are wanted
#' @inheritParams  stats::quantile
#' @details 
#' \emph{Deviation Scores} are just the difference between each observation and the sample mean
#' @export
deviation_scores <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  m <- mean(x)
  x - m
}



#' Sample Variance
#' 
#' @inheritParams stats::var
#' 
#' @export
sample_variance <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ( 1 / (length(x) - 1) ) * sum((x - mean(x))^2)
}

#' Sample Standard Deviation
#' 
#' @inheritParams stats::sd
#' 
#' @details 
#' The sample standard deviation is the (positive) square root of the variance
#' @export
sample_standard_deviation <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  sqrt(sample_variance(x))
}


#' Interquartile Range
#' 
#' @param x numeric vectors whose interquartile range is wanted
#' @inheritParams idealf
#' @details
#' For some purposes, it is important to measure the variability of the centrally located values.
#' 
#' One approach is the interquartile range, which is just \code{qu − ql} , the difference between the upper and lower quartiles.
#' 
#' Notice that the interquartile range is insensitive to the more extreme values.  The upper and lower quartiles are resistant to outliers, which means that the most extreme values do not affect the values of \code{qu} and \code{ql}. Consequently, the interquartile range is resistant to outliers as well.
#' @export
interquartile_range <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  quantiles <- idealf(x)
  
  quantiles$qu - quantiles$ql
}


#' Median Absolute Deviation
#' 
#' @param x numeric vectors whose median absolute deviation is wanted
#' @inheritParams  stats::quantile
#' @details 
#' MAD is computed by subtracting the median from each observation, taking the absolute value of each difference, and then computing the median.
#' 
#' Typically, \href{https://en.wikipedia.org/wiki/Median_absolute_deviation}{MAD} is divided by 0.6745. 
#' MADN = MAD / 0.6745
#' @export
mad <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  median(abs(x - median(x)))
}

