model <- lm(heart ~ ck, data=cklevel)
model
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model1 <- glm(heart ~ ck, data=cklevel)
model1
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))
