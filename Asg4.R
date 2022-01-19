#Q1
#library(ggplot2)
#library(ggpubr)
#library(dplyr)

x = seq(-2, 2, len = 1000)

boxcar = ggplot(mapping = aes(x = x, y = 1/2*(abs(x)<=1))) +
  geom_line() +
  labs(y = "", x = "", title = "Boxcar")

gaussian = ggplot(mapping = aes(x = x, y = 1/sqrt(2*pi)*exp(-x^2)/2)) +
  geom_line() +
  labs(y = "", x = "", title = "Gaussian")

epanechikov = ggplot(mapping = aes(x = x, y = 3/4*(1-x^2)*(abs(x)<=1))) +
  geom_line() +
  labs(y = "", x = "", title = "Epanechikov")

tricube = ggplot(mapping = aes(x = x, y = 70/81*(1-abs(x)^3)^3*(abs(x)<=1))) +
  geom_line() +
  labs(y = "", x = "", title = "Tricube")

ggarrange(boxcar, gaussian, epanechikov, tricube)

#Q2a
set.seed(1)
alpha = 1.5
beta = 2
n = length(x)
x = rgamma(n, alpha, beta)

neg_log_like_Gamma <- function(theta, x) {
  log_like <- (alpha-1) * sum(log(x)) - beta * sum(x) + n * alpha * log(beta) - n * log(gamma(alpha))
  -log_like 
}
neg_log_like_Gamma(theta = c(0.5, 1), x = x)

#2b
optim(par = runif(2, 0, 1), f = neg_log_like_Gamma, x = x, method = "BFGS")

#Q3a
set.seed(1)
beta <- c(0.3, 0.5, -0.5)
n <- 10000
p <- length(beta) - 1
X <- cbind(1, matrix(runif(n * p), nrow = n, ncol = p))
y <- rpois(n, exp(X %*% beta))

neg_log_like_Pois = function(beta, X, y) {
  log_like_Pois = sum((-exp(t(beta))*X[1])+(t(beta)*x[1]*y[1]))
  -log_like_Pois
}
neg_log_like_Pois(beta = c(0.5, 1, 0.5), X = X, y = y)

#3b
optim(par = runif(2, 0, 1), f = neg_log_like_Pois, X = X, y = y)

#Q4 
dedf = function(x, data) {
  n = length(data)
  fn = 1/n * sum(ifelse (data <= x, 1, 0))
  return(fn)
}

obs = c(10, 3, 5, 1)
dedf(0, obs)
dedf(1, obs)
dedf(2, obs)
dedf(10, obs)
dedf(12, obs)

#Q5
ks_ts = function(X, Y) {
  n = length(X)
  m = length(Y)
  threshold = c(X, Y)
  lis_D = rep(0, n+m)
  for (i in 1:(m+n)) {
    fn = 1/n * sum(ifelse (X <= threshold[i], 1, 0))
    gn = 1/m * sum(ifelse (Y <= threshold[i], 1, 0))
    lis_D[i] = abs(fn - gn)
  }
  return(max(lis_D))
}

set.seed(362)
X = rnorm(100, 0, 1)
Y = rnorm(100, 0, 1)
ks.test(X, Y)
ks_ts(X, Y)

#Q6
plot_edf = function(data){
  n = length(data)
  x = sort(data)
  y = seq(from = 1/n, to = 1, by = 1/n)
  fig = ggplot() +
    geom_point(mapping = aes(x = x, y= y), shape = 16, size = 1.5) +
    geom_point(mapping = aes(x = x[-1], y = y[-n]), shape = 1, size = 1.5)+
    geom_segment(mapping = aes(x = x[-n], y = y[-n], xend = x[-1], yend = y[-n]))+
    geom_segment(mapping = aes(x = x[n], y = y[n], xend = x[n] + 1, yend = y[n]))+
    geom_point(mapping = aes(x = x[1], y = 0), shape = 1, size = 1.5)+
    geom_segment(mapping = aes(x = x[1]-1, y=0, xend = x[1], yend = 0))+
    labs(y= expression(F[n](x)), title = "Empirical Distribution Function") +
    ylim(values = c(0,1))
  return(fig)
}

plot_edf(data = c(1, 5, 10, 2, 4))
  
#Q7 
x = c(171.6, 191.8, 178.3, 184.9, 189.1)
t.test(x, alternative = "less", mu = 185)

#Q8 
x = c(142600, 167800, 136500, 108300, 126400, 133700, 162000, 149400)
t.test(x, conf.level = 0.95)

#Q9
prop.test(35, 400)

#Q10
detergent_A = c(232, 260, 197)
detergent_B = c(168, 240, 203)
total_pop = c(400, 500, 400)
prop.test(detergent_A, total_pop)
prop.test(detergent_B, total_pop)

