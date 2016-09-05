library("Hmisc")
x <- c(26.3, 33.2, 59.5)
y1 <- c(31.8, 40.75, 93.518)
y2 <- c(29.61, 44.1, 99.23)
erry1 <- c(8.3, 8.2, 8.5)
erry2 <- c(7.6, 7.19, 7.10)

plot(x, y1, type="p", xlim=c(20, 75), ylim=c(0, 100), xlab = "Energy", ylab = "channel", pch=4)
with (
  data = data.frame(x,y1,erry1)
  , expr = errbar(x, y1, y1+erry1, y1-erry1, type="n",add=T, pch=1, cap=.015, log="x")
)
fm1 <- lm(y1 ~ x, weighted=1/erry1^2)
summary(fm1)
abline(fm1, col = "red")

plot(x, y2, xlim=c(20, 75), ylim=c(0, 105), xlab = "Energy", ylab = "channel",pch=4)
with (
  data = data.frame(x,y2,erry2)
  , expr = errbar(x, y2, y2+erry2, y2-erry2, type="n",add=T, pch=1, cap=.015, log="x")
)
fm2 <- lm(y2 ~ x)
summary(fm2)
abline(fm2, col = "red")