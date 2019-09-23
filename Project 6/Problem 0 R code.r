meta$Gastfem <- meta$Gastric*meta$Sex
fit1 <- lm(Metabol ~ Gastric + Gastfem +0, data=meta)
summary(fit1)
est1 <- 1.9278/(1.9278 - 1.2021)
est1

newmeta <- meta[-c(31, 32), ]
fit2 <- lm(Metabol ~ Gastric + Gastfem +0, data=newmeta)
summary(fit2)
est2 <- 1.5989/(1.5989 - 0.8732)
est2
cov(newmeta$Gastric, newmeta$Gastfem, use = "everything")
vcov(fit2)

