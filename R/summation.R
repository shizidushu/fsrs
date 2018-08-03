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
#'  No specific amount of trimming is always best, but for various reasons, 20\% trimming is often a good choice. This means that the smallest 20\% and the largest 20\% are trimmed and the average of the remaining data is computed.
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
  g <- floor(trim * n)
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
#' @examples 
#' x <- c(-29.6, - 20.9, - 19.7, - 15.4, - 12.3, - 8.0, - 4.3, 0.8, 2.0, 6.2, 11.2, 25.0)
#' idealf(x)
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

#' Compute the interquartile range using the ideal fourths
#' 
#' @inheritParams idealf
#' @seealso \code{\link{idealf}}
#' @export
idealfIQR <- function(x, na.rm = FALSE){
  if (na.rm) x <- x[!is.na(x)]
  res <- idealf(x)$qu-idealf(x)$ql
  res
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
#' @examples 
#' x <- c(12,45,23,79,19,92,30,58,132)
#' mad(x)
#' @export
mad <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  val <- median(abs(x - sample_median(x)))
  val
}

#' Median Absolute Deviation N
#' @inheritParams mad
#' @seealso \code{\link{mad}}
#' @export
madn <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  val <- mad(x) / 0.6745
}

#' Winsorized variance
#' 
#' @inheritParams base::mean 
#' @details 
#' When using a trimmed mean, certain types of analyses, require a Winsorized variance. 
#' 
#' The process of Winsorizing data by 20\% is related to 20\% trimming. Recall that when computing a 20\% trimmed mean, the \code{g} smallest and the \code{g} largest observations are removed, where \code{g = 0.2n} rounded down to the nearest integer, in which case the 20\% trimmed mean is the average of the remaining values.
#' 
#' Winsorizing the data by 20\% means that the \code{g} smallest values are not trimmed, but rather, they are set equal to the smallest value not trimmed. Similarly, the \code{g} largest values are set equal to the largest value not trimmed.
#' @examples 
#' x <- c(1, 2, 8, 9, 10, 16, 18, 22, 27, 29, 35, 42)
#' winvar(x)
#' @export
winvar <- function(x, trim = 0.2, na.rm=FALSE){
  if (na.rm) x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  g <- floor(trim * n)
  xbot <- x[g + 1]
  xtop <- x[length(x) - g]
  x <- ifelse(x <= xbot, xbot, x)
  x <- ifelse(x >= xtop, xtop, x)
  val <- sample_variance(x)
  val
}

#' Winsorized sample standard deviation
#' 
#' @inheritParams winvar
#' @examples 
#' x <- c(1, 2, 8, 9, 10, 16, 18, 22, 27, 29, 35, 42)
#' winsd(x)
#' @export
winsd <- function(x, trim = 0.2, na.rm = FALSE) {
  val = sqrt(winvar(x, trim = trim, na.rm = na.rm))
  val
}

#' Classic Outlier Detection Method
#' 
#' @inheritParams base::mean
#' @param crit how many standard deviations from the mean?
#' @param plotit Logical; Plot it or not?
#' @details 
#' In words, declare X an outlier if it is more than two standard deviations from the mean.
#' 
#' A commonly used variation of this rule is to declare a value an outlier if it is more than three standard deviations from the mean.
#' @examples 
#' x <- c(-158, 2, 8, 9, 10, 16, 18, -148, 22, 27, 29, 35, 42)
#' outms(x, plotit = TRUE)
#' @export
outms <- function(x, crit = 2, na.rm = TRUE, plotit = TRUE){
  if (na.rm) x <- x[!is.na(x)]
  
  z_score <- (x - mean(x, na.rm = na.rm)) / sample_standard_deviation(x, na.rm = na.rm)
  
  flag <- abs(z_score) >= crit
  
  n_out = sum(flag)
  
  out_value <- x[flag]
  
  keep_value <- x[!flag]
  
  out_id <- (1:length(x))[flag]
  
  keep_id <- (1:length(x))[!flag]
  
  if (plotit) {
    p <- ggplot2::qplot(1:length(x), x, color = ifelse(flag, "outlier", "not outlier")) + 
      ggplot2::scale_color_manual(values = c("cyan3", "red2")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Flag", title.position = "top"))
    
    print(p)
  }
  
  list(n =length(x), n_out = n_out, out_id = out_id, out_value = out_value, keep_id = keep_id, keep_value = keep_value)
  
}


#' Detect outlier using Boxplot rule
#' 
#' @details 
#' It is based on the fundamental strategy of avoiding masking by replacing the mean and standard deviation with measures of location and dispersion that are relatively insensitive to outliers.
#' 
#' In particular, the boxplot rule declares the value X an outlier if
#' 
#' X < ql − 1.5(qu − ql )
#' 
#' or
#' 
#' X > qu − 1.5(qu − ql )
#' 
#' So, the rule is based on the lower and upper quartiles, as well as the interquartile range, which provide resistance to outliers.
#' 
#' The ideal fourths are used to estimate the quartiles.
#' @inheritParams base::mean
#' @param mbox Logical; Using the modification of the boxplot rule suggested by Carling (2000). Defaults to FALSE.
#' 
#' See \url{https://www.researchgate.net/publication/4894204_Resistant_outlier_rules_and_the_non-Gaussian_case} for more details
#' @param gval Length of the whiskers as multiple of IQR. Defaults to 1.5
#' @param plotit Logical; Plot it or not?
#' @examples 
#' x<- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 100, 500)
#' outbox(x)
#' @export
outbox <- function(x,
                   mbox = FALSE,
                   gval = NA,
                   plotit = TRUE) {
  x <- x[!is.na(x)]
  
  n <- length(x)
  
  temp <- idealf(x)
  
  if (mbox) {
    if (is.na(gval)) {
      gval <- (17.63 * n - 23.64) / (7.74 * n - 3.71)
    }
    
    cl <- median(x) - gval * (temp$qu - temp$ql)
    cu <- median(x) + gval * (temp$qu - temp$ql)
  }
  
  if (!mbox) {
    if (is.na(gval)) {
      gval <- 1.5
    }
    
    cl <- temp$ql - gval * (temp$qu - temp$ql)
    cu <- temp$qu + gval * (temp$qu - temp$ql)
  }
  
  flag <- NA
  
  out_id <- NA
  
  vec <- c(1:n)
  
  for (i in 1:n) {
    flag[i] <- (x[i] < cl || x[i] > cu)
  }
  
  if (sum(flag) == 0) {
    out_id <- NULL
  }
  
  if (sum(flag) > 0) {
    out_id <- vec[flag]
  }
  
  keep_id <- vec[!flag]
  
  out_value <- x[flag]
  
  n_out <- sum(length(out_id))
  
  if (plotit) {
     df <- data.frame(x = 1, y = x)
     p <- df %>%
       ggplot2::ggplot(aes(x = x, y = y)) +
       ggplot2::geom_boxplot() +
       ggplot2::coord_cartesian(ylim = 
                         c(ifelse(cl < min(x), cl - 5, min(x) - 5), ifelse(cu > max(x), cu + 5, max(x) + 5) )) 

     gg <- ggplot2::ggplot_build(p)
     
     gg$data[[1]]$outliers[[1]] <- out_value
     gg$data[[1]]$ymin <- cl
     gg$data[[1]]$lower <- temp$ql
     gg$data[[1]]$upper <- temp$qu
     gg$data[[1]]$ymax<- cu
     
     plot(ggplot2::ggplot_gtable(gg))
  }
  
  list(out_value = out_value, out_id = out_id, keep_id = keep_id, n = n, n_out = n_out, cl = cl, cu = cu)
  
}

#' Detect outlier using the MAD–Median Rule
#' 
#' @inheritParams outms
#' @examples 
#' x <- c(360, 400, 75, 70, 40, 50, 16, 14, 23, 25, 30, 15, 15, 20, 6, 5)
#' outpro(x)
#' @export
outpro <- function(x, na.rm = TRUE, plotit = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  
  flag <- (abs(x - median(x)) / madn(x)) > 2.24
  
  n_out = sum(flag)
  
  out_value <- x[flag]
  
  keep_value <- x[!flag]
  
  out_id <- (1:length(x))[flag]
  
  keep_id <- (1:length(x))[!flag]
  
  if (plotit) {
    p <- ggplot2::qplot(1:length(x), x, color = ifelse(flag, "outlier", "not outlier")) + 
      ggplot2::scale_color_manual(values = c("cyan3", "red2")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Flag", title.position = "top"))
    
    print(p)
  }
  
  list(n =length(x), n_out = n_out, out_id = out_id, out_value = out_value, keep_id = keep_id, keep_value = keep_value)
}