stocks <- stockton4
stocks$lnsprice <- log(stocks$sprice)
stocks$livarea2 <- stocks$livarea**2
stocks$age2 <- stocks$age**2
stocks$livareabeds <- stocks$livarea*stocks$beds
stocks$livarea2beds <- stocks$livarea2*stocks$beds
stocks$agebeds <- stocks$age*stocks$beds
stocks$age2beds <- stocks$age2*stocks$livarea
model1 <- lm(lnsprice ~ livarea + livarea**2 + age + age**2 + beds + 
               livarea*beds + (livarea**2)*beds + age*beds + (age**2)*beds, 
             data = stocks)
summary(model1)
model2 <- lm(lnsprice ~ livarea + livarea2 + age + age2 + beds + 
               livareabeds + livarea2beds + agebeds + age2beds, 
             data = stocks)
summary(model2)
model3 <- lm(lnsprice ~ livarea + livarea2 + age + age2 + 
               livareabeds + age2beds, 
             data = stocks)
summary(model3)
AIC(model2, model3)
BIC(model2, model3)
