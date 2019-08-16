dia <- Diamonds
dia$Caratsq <- Diamonds$Carat**2
dia$logPrice <- log(dia$TotalPrice)
linmod <- lm(logPrice ~ Carat + Caratsq, data=dia)
summary(linmod)
new = data.frame(Carat=0.5,
                  Caratsq = 0.25)
predict(linmod, new, interval = "prediction")
predict(linmod, new, interval = "confidence")
exp(predict(linmod, new, interval = "prediction"))
exp(predict(linmod, new, interval = "confidence"))
exp(predict(linmod, new))
