library("readxl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cocaine <- read_excel("Problem 4 Cocaine Dataset.xls")
linmod <- lm(cocaine$price ~ cocaine$quant + cocaine$qual + cocaine$trend)
print(linmod)
summary(linmod)

library(car)

linearHypothesis(linmod, "cocaine$qual = 0")

tval <- (-0.05997)/0.01018
tval
pval <- 0.0000
