data <- CDP
linmod <- lm(data=data, price ~ age + net)
print(linmod)
summary(linmod)

linmod2 <- lm(data=data, price ~ old + net)
print(linmod2)
summary(linmod2)
summary(data$price)
