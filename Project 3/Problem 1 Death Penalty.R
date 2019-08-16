data <- matrix(data = c(53, 11, 0, 4, 53, 15, 414, 37, 16, 139, 430, 176), ncol = 2, nrow = 6, dimnames = list(c('white-white',
                                                                                                                 'white-black', 'black-white',
                                                                                                                 'black-black', 'total-white',
                                                                                                                 'total-black'),c('yes', 'no')))
data

wprop <- 53/(53 + 430)
bprop <- 15/(15 + 176)
wprop
bprop

wwprop <- 53/(53 + 414)
wbprop <- 11/(11 + 37)
bwprop <- 0/(0 + 16)
bbprop <- 4/(4 + 139)
wwprop
wbprop
bwprop
bbprop

library(questionr)
library("vcd")

new_data <- c(53, 414, 11, 37, 0, 16, 4, 139)
new_data <- array(new_data, dim=c(2,2,2))
dimnames(new_data) <- list(DeathPen=c("yes","no"),
  Defendant=c("white","black"),
  Victim=c("white","black"))

ftable(new_data, row.vars=c("Victim","Defendant"),col.vars="DeathPen")

AC <- margin.table(new_data, c(2,1))
chisq.test(AC)
assocstats(AC)
oddsratio(AC, log=FALSE)
exp(confint(oddsratio(AC)))

oddsratio(new_data, 3, log=FALSE)
lor=oddsratio(new_data,3) 
exp(confint(lor))
summary(lor)

conditional <- matrix(c(53, 11, 414, 37), ncol = 2)
odds.ratio(conditional)

marginal <- matrix(c(53, 15, 430, 176), ncol = 2)
odds.ratio(marginal)
