sales <- c(6,6,4,2,3)
salesmatrix <- matrix(c(1,1,1,1,1,6,6,4,2,3), nrow=5, ncol=2)
cars <- c(20,18,10,6,11)
plot(sales, cars)
model <- lm(cars ~ sales)
summary(model)
abline(model)
w <- solve(t(salesmatrix)%*%salesmatrix)%*%t(salesmatrix)%*%cars
w

newdata = data.frame(sales=5)
predict(model, newdata, interval="predict")
predict(model, newdata, interval="confidence")
confint(model)

0.125+3.125*t(sales)
cars - (0.125+3.125*t(sales))
mean((cars - (0.125+3.125*t(sales)))^2)