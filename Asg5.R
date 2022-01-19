#Q1a
# library(readr)
# iris_test <- read_csv("Downloads/iris_test.csv")
iris_test_mod = iris_test[-(5)]
# View(iris_test)
# iris_train <- read_csv("Downloads/iris_train.csv")
iris_train_mod = iris_train[-(5)]
# View(iris_train)

iris_train_n = iris_train_mod
iris_test_n = iris_test_mod

train_min <- apply(iris_train_mod, 2, min)
train_max <- apply(iris_train_mod, 2, max)

for (i in 1:ncol(iris_train_mod)) {
  iris_train_n[, i] <- (iris_train_mod[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  iris_test_n[, i] <- (iris_test_mod[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

#Q1b
#library(class)
iris_test_labels = iris_test$Species
iris_train_labels = iris_train$Species
knn_predicted = knn(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3)

#Q1c
table(iris_test_labels, knn_predicted)

#Q1d
# prob outputs the different classes as proportions, with the largest class being 1.00
knn_predicted = knn(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3, prob = TRUE)

#Q2
train = iris_train_n
test = iris_test_n
cl = iris_train_labels
k = 3

knn_L1 <- function(train, test, cl, k){
  test_length = nrow(test)
  knn_output = data.frame(class = rep(0,test_length), prob = rep(0, test_length))
  for (i in 1:nrow(test)) {
    distance = apply(train, 1, FUN = function(x) sum(abs(test[i,] - x)))
    top_k = sort(distance)[1:k]
    res = cl[which(distance %in% top_k)]
    dist = table(res)
    max_dist = which(dist == max(dist))
    knn_output[i,1] = paste(names(max_dist), collapse=",")
    knn_output[i,2] = length(which(res == names(max_dist)[1])) / length(res)
  }
  return(knn_output)
}

#Q3 
# 1b redone:
knn_L1_predicted = knn_L1(train = iris_train_n, test = iris_test_n, cl = iris_train_labels, k = 3)

# 1c redone:
table(iris_test_labels, knn_L1_predicted$class)

#Q4
# wbcd_test <- read_csv("Downloads/wbcd_test.csv")
# wbcd_train <- read_csv("Downloads/wbcd_train.csv")
# library(C50)
wbcd_train_mod = wbcd_train[-(1:2)]
wbcd_test_mod = wbcd_test[-(1:2)]

wbcd_train_labels = wbcd_train$diagnosis

wbcd_train_n = wbcd_train_mod
wbcd_test_n = wbcd_test_mod
train_min = apply(wbcd_train_mod, 2, min)
train_max = apply(wbcd_train_mod, 2, max)

for (i in 1:ncol(wbcd_train_mod)) {
  wbcd_train_n[, i] <- (wbcd_train_mod[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  wbcd_test_n[, i] <- (wbcd_test_mod[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

knn_L1(train = wbcd_train_n, test = wbcd_test_n, cl = wbcd_train_labels, k = 11)

knn(train = wbcd_train_n, test = wbcd_test_n, cl = wbcd_train_labels, k = 11)

wbcd_ct <- C5.0(x = wbcd_train[, -2], y = as.factor(wbcd_train$diagnosis))


#Q5a
# rm(list=ls())

#Q5b
# command + return on mac

#Q5c
# command + s on mac

#Q6a
# library(dplyr)
# BTCUSDT_day <- read_csv("Downloads/BTCUSDT_day.csv")
# BTCUSDT_minute <- read_csv("Downloads/BTCUSDT_minute.csv")
# ETHUSDT_day <- read_csv("Downloads/ETHUSDT_day.csv")
# ETHUSDT_minute <- read_csv("Downloads/ETHUSDT_minute.csv")

BTC_day_c = BTCUSDT_day %>% filter(!`Volume USDT`=="0")
BTC_min_c = BTCUSDT_minute %>% filter(!`Volume USDT`=="0")
ETH_day_c = ETHUSDT_day %>% filter(!`Volume USDT`=="0")
ETH_min_c = ETHUSDT_minute %>% filter(!`Volume USDT`=="0")

#Q6b
BTC_day_c$log_return = log(BTC_day_c$close / BTC_day_c$open)
BTC_min_c$log_return = log(BTC_min_c$close / BTC_min_c$open)
ETH_day_c$log_return = log(ETH_day_c$close / ETH_day_c$open)
ETH_min_c$log_return = log(ETH_min_c$close / ETH_min_c$open)

#Q6c
fit = lm(BTC_day_c$log_return ~ ETH_day_c$log_return)
summary(fit)

#Q6d
fit = lm(BTC_min_c$log_return ~ ETH_min_c$log_return)
summary(fit)

#Q6e
# in the 1-minute log-return, the intercept values are extremely small, though they both have very small p-values

#Q7a
# library(ggplot2)
# library(ggpubr)
BTC_min_graph = ggplot(BTC_min_c, aes(x = log_return)) +
  geom_density(colour = "red")
ETH_min_graph = ggplot(ETH_min_c, aes(x = log_return)) +
  geom_density(colour = "blue")
ggarrange(BTC_min_graph, ETH_min_graph)
# they are both not close to a normal ditribution

#Q7b
set.seed(1)
x = sample(ETH_min_c$log_return, size = 100)
shapiro.test(x)

#Q8a
BTC_day_c_100 = BTC_day_c[-c(101:1307), ]
ggplot(BTC_day_c_100, aes(x = `Volume USDT`, y = log_return)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm")

#Q8b
BTC_day_c_100 = BTC_day_c[-c(101:1307), ]
BTC_day_c_100$log_return = abs(BTC_day_c_100$log_return)
ggplot(BTC_day_c_100, aes(x = `Volume USDT`, y = log_return)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm")

#Q8c
# in 8b there is a stronger correlation than in 8a

#Q9a
n = nrow(BTC_day_c)
day = 30
cor_return = rep(0, n - day + 1)
for ( i in 1:(n - day + 1)) {
  cor_return[i] = cor(BTC_day_c$log_return[i:(i + day - 1)],
                      ETH_day_c$log_return[i:(i + day - 1)])
}

#Q9b
date_label = rev(BTC_day_c$date[seq(1, n - day + 1, by = 100)])
date_label = gsub(pattern = "0:00", replacement = "", x = date_label)
ggplot(mapping = aes(x = 1:(n - day + 1), y = rev(cor_return))) + 
  geom_line() +
  scale_x_continuous(name = "Date", breaks = seq(1, n - day + 1, by = 100),
                     labels = date_label) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  labs(y = "Correlation")

#Q10
# there is a great decrease in correlation from late December of 2017 to early-mid 2018
