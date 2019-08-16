library(gtable)
library(grid)
library(gridExtra)

data = data2
p1 <- qplot(seq_along(data$Heart), data$Heart)
p2 <- qplot(seq_along(data$Walk), data$Walk)
p3 <- qplot(seq_along(data$Talk), data$Talk)
p4 <- qplot(seq_along(data$Bank), data$Bank)
grid.arrange(p1, p2, p3, p4, nrow=2)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot #2: same as above, but add less smoother in lower and correlation in upper
pairs(~Bank+Talk+Walk+Heart, data=data2,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Scatterplot Matrix")

model <- lm(Heart ~ Walk+Talk+Bank, data=data2)
summary(model)
plot(model)
pureErrorAnova(model)
