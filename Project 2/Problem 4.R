library(car)
air <- data4$Airplane
heli <- data4$Helicopter
plot(air, heli)
model <- lm(air ~ heli)
abline(model)
summary(model)
linearHypothesis(model, "heli = 0")
linearHypothesis(model, "heli = 1")
model <- lm(air ~ -1+heli)
summary(model)
linearHypothesis(model, "heli = 1")