#Q1
my_LS = function(X,Y){
  ans1 = solve(t(X)%*%X)%*%t(X)%*%Y
  return(as.vector(ans1))
}
X = cbind(rep(1,10),1:10)
Y = seq(1,30,length = 10)
my_LS(X,Y)

#Q2
lambda = 1
I = diag(1, 2, 2)
my_ridge = function(X,Y){
  ans1 = solve(t(X)%*%X+lambda*I)%*%t(X)%*%Y
  return(as.vector(ans1))
}
X <-cbind(rep(1, 10), 1:10)
Y <-seq(1,30, length = 10)
my_ridge(X,Y)

#Q3
n = 5
m = 6
x = 1:n
y = 1:m
my_sum = function(x,y) { 
  sum = 0
  for (x in 1:n) {
    for (y in 1:m) {
      sum = sum + x^2*y/(x+y)
    }
  }
  return(as.numeric(sum))
}
my_sum(x,y)

#Q4 
n = 5
m = 6
x = matrix(1:n, nrow=n, ncol=m)
y = matrix(1:m, nrow=n, ncol=m, byrow = TRUE)
my_sum2 = function(x,y) {
  f_sum = sum(x^2*y/(x+y))
  return(as.numeric(f_sum))
}
my_sum2(x,y)

#Q5
round(runif(1,min = 1, max = 5))

#Q6
sample(c(0,1), size = 10, replace = TRUE, prob = c(0.4,0.6))

#Q7a
abs(sum(seq(1, 1000, by = 3)))

#Q7b
abs(sqrt(sum(seq(1, 1000, by = 3)^2)))

#Q8
sum(c(1:25)[c(1:25)%%2==1])

#Q9
X9 = pgeom(c(1:10), 0.3)
plot(X9, type = "l", xlab = "X Values", ylab = "Probability Density", main = "X Values vs Probability Density")

#Q10
no_sim = 10000 # number of simulation
above_and_below = rep(0, no_sim)
for (j in 1:no_sim) {
  price = 100*exp(cumsum(rnorm(40, mean = 0.0002, sd = 0.015)))
  above_and_below[j] = min(price[1:20]) < 95 & max(price) > 101
}
mean(above_and_below)


