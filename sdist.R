sbeta <- function(shape1, shape2, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = shape1 / (shape1 + shape2)), 
         "median" = c("median" = qbeta(0.5, shape1, shape2)), 
         "sd"     = c("sd" = sqrt(sbeta(shape1, shape2, "var"))), 
         "var"    = c("var" = (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))), 
         "skew"   = c("skew" = (2 * (shape2 - shape1) * sqrt(shape1 + shape2 + 1)) / ((shape1 + shape2 + 2) * sqrt(shape1 * shape2))), 
         "kurt"   = c("kurt" = (6 * ((shape1 - shape2)^2 * (shape1 + shape2 + 1) - shape1 * shape2 * (shape1 + shape2 + 2))) / (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))),
         "all"    = {
           all <- rlang::eval_tidy(formals(sbeta)[["statistic"]])[-1]
           vapply(all, function(statistic, shape1, shape2) sbeta(shape1, shape2, statistic), numeric(1), shape1, shape2)
         }
  )
}



sbinom <- function(size, prob, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = size * prob), 
         "median" = c("median" = sbinom(size, prob, "mean")), 
         "sd"     = c("sd" = sqrt(sbinom(size, prob, "var"))), 
         "var"    = c("var" = size * prob * (1 - prob)), 
         "skew"   = c("skew" = (1 - 2 * prob) / (sqrt(size * prob * (1 - prob)))), 
         "kurt"   = c("kurt" = (1 - 6 * prob * (1 - prob)) / (size * prob * (1 - prob))),
         "all"    = {
           all <- rlang::eval_tidy(formals(sbinom)[["statistic"]])[-1]
           vapply(all, function(statistic, size, prob) sbinom(size, prob, statistic), numeric(1), size, prob)
         }
  )
}



schisq <- function(df, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = df), 
         "median" = c("median" = df * (1 - 2 / (9 * df))^3), 
         "sd"     = c("sd" = sqrt(schisq(df, "var"))), 
         "var"    = c("var" = 2 * df), 
         "skew"   = c("skew" = sqrt(8 / df)), 
         "kurt"   = c("kurt" = 12 / df),
         "all"    = {
           all <- rlang::eval_tidy(formals(schisq)[["statistic"]])[-1]
           vapply(all, function(statistic, df) schisq(df, statistic), numeric(1), df)
         }
  )
}



sexp <- function(rate = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = 1 / rate), 
         "median" = c("median" = rate^(-1) * log(2)), 
         "sd"     = c("sd" = sqrt(sexp(rate, "var"))), 
         "var"    = c("var" = rate^(-2)), 
         "skew"   = c("skew" = 2), 
         "kurt"   = c("kurt" = 6),
         "all"    = {
           all <- rlang::eval_tidy(formals(sexp)[["statistic"]])[-1]
           vapply(all, function(statistic, rate) sexp(rate, statistic), numeric(1), rate)
         }
  )
}



sgamma <- function(shape, rate = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = shape / rate), 
         "median" = c("median" = qgamma(0.5, shape, rate)), 
         "sd"     = c("sd" = sqrt(sgamma(shape, rate, "var"))), 
         "var"    = c("var" = shape / rate^2), 
         "skew"   = c("skew" = 2 / sqrt(shape)), 
         "kurt"   = c("kurt" = 6 / shape),
         "all"    = {
           all <- rlang::eval_tidy(formals(sgamma)[["statistic"]])[-1]
           vapply(all, function(statistic, shape, rate) sgamma(shape, rate, statistic), numeric(1), shape, rate)
         }
  )
}



slnorm <- function(meanlog = 0, sdlog = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = exp(meanlog + sdlog^2 / 2)), 
         "median" = c("median" = exp(meanlog)), 
         "sd"     = c("sd" = sqrt(slnorm(meanlog, sdlog, "var"))), 
         "var"    = c("var" = (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2)), 
         "skew"   = c("skew" = (exp(sdlog^2) + 2) * sqrt(exp(sdlog^2) - 1)), 
         "kurt"   = c("kurt" = exp(4 * sdlog^2) + 2 * exp(3 * sdlog^2) + 3 * exp(2 * sdlog^2) - 6),
         "all"    = {
           all <- rlang::eval_tidy(formals(slnorm)[["statistic"]])[-1]
           vapply(all, function(statistic, meanlog, sdlog) slnorm(meanlog, sdlog, statistic), numeric(1), meanlog, sdlog)
         }
  )
}



snorm <- function(mean = 0, sd = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = mean), 
         "median" = c("median" = snorm(mean, sd, "mean")), 
         "sd"     = c("sd" = sd), 
         "var"    = c("var" = sd^2), 
         "skew"   = c("skew" = 0), 
         "kurt"   = c("kurt" = 0),
         "all"    = {
           all <- rlang::eval_tidy(formals(snorm)[["statistic"]])[-1]
           vapply(all, function(statistic, mean, sd) snorm(mean, sd, statistic), numeric(1), mean, sd)
         }
  )
}



spois <- function(lambda, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = lambda), 
         "median" = c("median" = lambda + 1 / 3 - 0.02 / lambda), 
         "sd"     = c("sd" = sqrt(spois(lambda, "var"))), 
         "var"    = c("var" = lambda), 
         "skew"   = c("skew" = lambda^(-1 / 2)), 
         "kurt"   = c("kurt" = 1 / lambda),
         "all"    = {
           all <- rlang::eval_tidy(formals(spois)[["statistic"]])[-1]
           vapply(all, function(statistic, lambda) spois(lambda, statistic), numeric(1), lambda)
         }
  )
}



st <- function(df, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = ifelse(df > 1, 0, NA)), 
         "median" = c("median" = 0), 
         "sd"     = c("sd" = sqrt(st(df, "var"))), 
         "var"    = c("var" = ifelse(df > 2, df / (df - 2), ifelse(df > 1, Inf, NA))), 
         "skew"   = c("skew" = ifelse(df > 3, 0, NA)), 
         "kurt"   = c("kurt" = ifelse(df > 4, 6 / (df - 4), ifelse(df > df, Inf, NA))),
         "all"    = {
           all <- rlang::eval_tidy(formals(st)[["statistic"]])[-1]
           vapply(all, function(statistic, df) st(df, statistic), numeric(1), df)
         }
  )
}



sunif <- function(min = 0, max = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)
  
  switch(statistic, 
         "mean"   = c("mean" = (min + max) / 2), 
         "median" = c("median" = sunif(min, max, "mean")), 
         "sd"     = c("sd" = sqrt(sunif(min, max, "var"))), 
         "var"    = c("var" = (1 / 12) * (max - min)^2), 
         "skew"   = c("skew" = 0), 
         "kurt"   = c("kurt" = -6 / 5),
         "all"    = {
           all <- rlang::eval_tidy(formals(sunif)[["statistic"]])[-1]
           vapply(all, function(statistic, min, max) sunif(min, max, statistic), numeric(1), min, max)
         }
  )
}
