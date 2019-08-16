#Problem 1
sombrero <- c(-10, -7.5, -4.16, -2.5, 0, 0.83, 3.33, 3.33, 5.83, 10.83)
nrep <- 10000
compare <- replicate(nrep,sample(sombrero, 1, replace = F))
hist(compare)
abline(v=10.83, col = "blue", lwd = 2)
t.test(compare, alternative = "greater", mu = 0)
t.test(c(31, 35, 38), c(47, 44), alternative = "less", var.equal = FALSE)

t <- (mean(c(31, 35, 38))-mean(c(47, 44)))/sqrt((sd(c(31, 35, 38))^2/3)+(sd(c(47, 44))^2/2))
t


#Problem 2
one <- c(166.2, 157.3, 166.7, 161.1)
nine<- c(162.8, 142.4, 162.8, 162.4)
sombrero <- c(166.2, 157.3, 166.7, 161.1, 162.8, 142.4, 162.8, 162.4)
means <- c(162.825, 163.25, 158.15, 163.25, 163.15, 161.85000000000002, 156.75, 161.85000000000002, 161.75, 157.175, 162.275, 162.175, 157.175, 157.075, 162.175, 164.2, 159.1, 164.2, 164.1, 159.525, 164.625, 164.525, 159.52499999999998, 159.42499999999998, 164.525, 158.125, 163.225, 163.125, 158.125, 158.02499999999998, 163.125, 158.55, 158.45, 163.55, 158.45000000000002, 161.97500000000002, 156.875, 161.97500000000002, 161.875, 157.3, 162.4, 162.3, 157.3, 157.2, 162.3, 155.9, 161.0, 160.9, 155.89999999999998, 155.79999999999998, 160.9, 156.325, 156.225, 161.32500000000002, 156.22500000000002, 158.25, 163.35, 163.25, 158.25, 158.14999999999998, 163.25, 158.675, 158.575, 163.675, 158.57500000000002, 157.27499999999998, 157.17499999999998, 162.275, 157.175, 157.60000000000002)
diffs <- c(5.224999999999966, 6.074999999999989, -4.125, 6.074999999999989, 5.875000000000028, 3.275000000000034, -6.925000000000011, 3.275000000000034, 3.0749999999999886, -6.074999999999989, 4.125000000000028, 3.9250000000000114, -6.074999999999989, -6.275000000000006, 3.9250000000000114, 7.974999999999966, -2.2250000000000227, 7.974999999999966, 7.775000000000006, -1.3749999999999716, 8.825000000000017, 8.625000000000028, -1.375, -1.575000000000017, 8.625000000000028, -4.175000000000011, 6.025000000000006, 5.824999999999989, -4.175000000000011, -4.375000000000028, 5.824999999999989, -3.3249999999999886, -3.525000000000034, 6.675000000000011, -3.5250000000000057, 3.525000000000034, -6.675000000000011, 3.525000000000034, 3.3249999999999886, -5.824999999999989, 4.375000000000028, 4.175000000000011, -5.824999999999989, -6.025000000000006, 4.175000000000011, -8.624999999999972, 1.575000000000017, 1.3750000000000284, -8.625, -8.825000000000017, 1.3750000000000284, -7.775000000000006, -7.974999999999994, 2.2250000000000227, -7.974999999999966, -3.9250000000000114, 6.275000000000006, 6.074999999999989, -3.9250000000000114, -4.125000000000028, 6.074999999999989, -3.0749999999999886, -3.275000000000034, 6.925000000000011, -3.2750000000000057, -5.875000000000028, -6.075000000000017, 4.125, -6.074999999999989, -5.224999999999966)
hist(diffs, breaks=20)
abline(v=5.224999999999966, col = "blue", lwd = 2)
t.test(means, alternative = "greater", mu = 157.60000000000002)
t.test(nine, alternative = "less", mu = 160.2125)





#Problem 3
good <- c(20.8, 18.7, 19.9, 20.6, 21.9, 23.4, 22.8, 24.9, 22.2, 20.3, 24.9, 22.3, 27, 20.5, 22.2, 24, 21.2, 22.1, 22, 22.7)
bad <- c(18, 19.1, 19.2, 18.8, 18.4, 19, 18.5, 16.1, 16.8, 14, 17, 13.6, 17.5, 20, 20.2, 18.8, 18, 23.2, 18.2, 19.4)
hist(good)
hist(bad)
t.test(good, bad)

sombrero <- c(good, bad)
nrep=10000
compare=replicate(nrep,mean(sample(sombrero,20,replace=F))>=mean(good))
nyes=sum(compare)
p=(sum((compare))+1)/(nrep+1)
nyes
p

#Problem 4
diff <- c(0, -7, 7, -2, -12, 14, -1, 2, -2, -2, -13, 9, 1, 6, -12, 3, 3, 4, 2, -5, 0, 7, 17, 3, -14, -1, 8, -4, 8, -9, -1, 6, 2, -3, 2, 4, -2, 2, -2)
length(diff)
hist(diff)
t.test(diff)

helium=c(25, 16, 25, 14, 23, 29, 25, 26, 22, 26, 12, 28, 28, 31, 22, 29, 23, 26, 35, 24, 31, 34, 39, 32, 14, 28, 30, 27, 33, 11, 26, 32, 30, 29, 30, 29, 29, 30, 26)
air=helium - diff
AveDiff=mean(helium-air)
NRep=10000
compare=replicate(NRep,mean((helium-air)*sample(c(-1,1),39,replace=T))>=AveDiff)
pvalue=sum(compare)/NRep
pvalue





#Problem 5
hist(data$Rate)
plot(data)
lines(lowess(data), col="blue")
abline(47.0522, -0.6957, col="red")
summary(lm(formula = data$Rate ~ data$Age))
summary(data$Age)
newx <- seq(0.10, 36, by=0.05)
conf_interval <- predict(lm(formula = data$Rate ~ data$Age), newdata=data.frame(QUET=newx), interval="confidence", level=0.95)
lines(newx, conf_interval[,2], col="orange", lty=2)
lines(newx, conf_interval[,3], col="orange", lty=2)


library(ggplot2)
#data$Rate = log(data$Rate)
model1 <- lm(formula = data$Rate ~ data$Age)
temp_var <- predict(model1, interval="prediction")
new_df <- cbind(data, temp_var)

g <- ggplot(new_df, aes(Age, Rate))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)+
  scale_x_continuous(breaks = seq(0, 36, 1), limits = c(0, 36))+
  scale_y_continuous(labels=function(y)floor(exp(y)), breaks = seq(0, 4.5, 0.1))+
  geom_vline(xintercept = 1, colour="green", show.legend="1")+
  geom_vline(xintercept = 2, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 4, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 6, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 9, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 12, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 15, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 18, colour="green", show.legend=TRUE)+
  geom_vline(xintercept = 24, colour="green", show.legend=TRUE)

summary(model1)
g


#Problem 6
library(ggplot2)

#dataset$Wine = log(dataset$Wine)
#dataset$Mortality = log(dataset$Mortality)

model1 <- lm(formula = dataset$Mortality ~ dataset$Wine)
temp_var <- predict(model1, interval="prediction")
new_df <- cbind(dataset, temp_var)

g <- ggplot(new_df, aes(Wine, Mortality))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)


summary(model1)
g

res = resid(model1)

plot(dataset$Wine, res)
abline(0, 0)