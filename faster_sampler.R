
pdist <- function(f, q, lower = -Inf) {
  integrate(f, lower = lower, upper = q)$value
}

qdist_inv <- function(f, q.min = -10, q.max = 10, acc = 100, lower = -Inf) {
  probs <- sapply(seq(0, 1, length.out = acc)[-c(1,acc)], 
                  function(x) {
                    uniroot(f = function(y) pdist(f, 
                                                  y, 
                                                  lower = -Inf) - x, 
                            interval = c(q.min, q.max))$root
  })
  data.frame(x = seq(0, 1, length.out = acc)[-c(1, acc)],
             y = probs)
}
f <- function(x) 1/sqrt(2 * pi) * exp(-x^2/2)
q_dist_vec <- qdist_inv(f, acc = 1000)



library(data.table)
q_dist <- data.table(x = q_dist_vec$x,
                     y = q_dist_vec$y,
                     val = q_dist_vec$x)
setkey(q_dist, "x")
setattr(q_dist, "sorted", "x")

fast_samplr <- function(f, num_samples, acc = 1000) {
  my_sample <- runif(num_samples, 0, 1)
  a_below <- q_dist[J(my_sample), roll = -Inf]
  a_above <- q_dist[J(my_sample), roll = Inf]
  lambdas <- (a_below$x - a_below$val)/(a_above$val - a_below$val)
  lambdas*a_above$y + (1 - lambdas) * a_below$y
}

hist(rnorm(100))
hist(fast_samplr(f, num_samples = 100))
shapiro.test(fast_samplr(f, num_samples = 1000))