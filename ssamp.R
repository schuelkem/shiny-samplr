skew <- function(x) {
  n <- length(x)
  x <- scale(x, scale = FALSE)
  sqrt(n) * sum(x^3) / (sum(x^2)^(3/2))
}

kurtosis <- function(x) {
  n <- length(x)
  x <- scale(x, scale = FALSE)
  r <- n * sum(x^4) / (sum(x^2)^2) - 3
}

ssamp <- function(x, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt"), ...) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = mean(x, ...)), 
         "median" = c("median" = median(x, ...)), 
         "sd"     = c("sd" = sd(x, ...)), 
         "var"    = c("var" = var(x, ...)), 
         "skew"   = c("skew" = skew(x)), 
         "kurt"   = c("kurt" = kurtosis(x)),
         "all"    = {
           all <-   c("mean", "median", "sd", "var", "skew", "kurt")
           stats <- vapply(all, function(statistic, x, ...) ssamp(x, statistic, ...), numeric(1), x, ...)
           names(stats) <- all
           stats
         }
  )
}
