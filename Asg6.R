# Q1a
normalize <- function(train, test) {
  train_n <- train
  test_n <- test
  train_min <- apply(train, 2, min)
  train_max <- apply(train, 2, max)
  for (i in 1:ncol(train)) {
    train_n[, i] <- (train[, i] - train_min[i]) / (train_max[i] - train_min[i])
    test_n[, i] <- (test_n[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
  }  
  return(list(train = train_n, test = test_n))
}
train_test_n = normalize(asg_6_train, asg_6_test)
train_n = train_test_n$train
test_n = train_test_n$test

set.seed(1)
train_ANN = neuralnet(y ~ ., data = train_n, hidden = c(2, 2))

# Q1b
predicted = neuralnet::compute(train_ANN, test_n[, 2:4])
predicted_net_result = predicted$net.result[, 1]
cor(test_n$y, predicted_net_result)

# Q1c
train_lm = lm(y ~ ., data = train_n)

# Q1d
prob = predict(train_lm, test_n[, 2:4])
cor(test_n$y, prob)

# Q1e
# 1b performs better as it is able to give a higher correlation value
