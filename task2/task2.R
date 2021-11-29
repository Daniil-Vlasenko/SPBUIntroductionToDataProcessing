if (!require("latex2exp")) install.packages("latex2exp")
if (!require("dplyr")) install.packages("dplyr")
library("latex2exp")
library("dplyr")

# 1. Rn has a limit distribution for n -> inf 
RNorm <- function(x, a, mean = 0, sd = 1) {
  emp.cdf <- ecdf(x)
  n = length(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pnorm = pnorm(x, mean, sd))
  res <- (abs(df$emp.cdf - df$pnorm) / df$pnorm)[df$pnorm > a] %>% max() * sqrt(n)
}
RnNorm <- function(n, a, mean = 0, sd = 1) {
  x <- sapply(10:n, rnorm, mean, sd)
  res <- sapply(x, RNorm, a, mean, sd)
}
RExp <- function(x, a, r) {
  emp.cdf <- ecdf(x)
  n = length(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pexp = pexp(x, r))
  res <- (abs(df$emp.cdf - df$pexp) / df$pexp)[df$pexp > a] %>% max() * sqrt(n)
}
RnExp <- function(n, a, r) {
  x <- sapply(10:n, rexp, r)
  res <- sapply(x, RExp, a, r)
}

pdf(file="1.pdf")
par(mfrow=c(2,2))
hist(RnNorm(100, 1/2), breaks = 10, xlim = c(0, 3), col = "cyan1",  main = "n = 100", xlab = "Rn")
hist(RnNorm(500, 1/2), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "n = 500", xlab = "Rn")
hist(RnNorm(1000, 1/2), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "n = 1000", xlab = "Rn")
hist(RnNorm(5000, 1/2), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "n = 5000", xlab = "Rn")
dev.off()
# 2. Asymptotic distribution of Rn is independent of the distribution function F(x).
pdf(file="2.pdf")
par(mfrow=c(3,1))
hist(RnNorm(3000, 1/2), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "N(0, 1)", xlab = "Rn")
hist(RnNorm(3000, 1/2, 50, 4), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "N(50, 4)", xlab = "Rn")
hist(RnExp(3000, 1/2, 1), breaks = 15, xlim = c(0, 3), col = "cyan1", main = "EXP(1)", xlab = "Rn")
dev.off()
# 3. Check statement lim_{n -> inf}{P(sqrt(a / (1 - a))R_{n}^{+} < x)} = 2Ф(x) - 1
RPlusNorm <- function(n, a) {
  x <- rnorm(n)
  emp.cdf <- ecdf(x)
  n = length(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pnorm = pnorm(x))
  res <- ((df$emp.cdf - df$pnorm) / df$pnorm)[df$pnorm > a] %>% max() * sqrt(n)
}
distributionFunction <- function(y, n, m, a) {
  res <- sapply(rep(n , m), RPlusNorm, a)
  emp.cdf <- ecdf(res)
  emp.cdf(y)
}
pdf(file="3.pdf")
par(lwd = 2)
x <- seq(0, 4, length=100)
y1 <- pnorm(x) * 2 - 1
y2 <- sapply(x * 1, distributionFunction, 10, 1000, 1/2)
y3 <- sapply(x * 1, distributionFunction, 50, 1000, 1/2)
y4 <- sapply(x * 1, distributionFunction, 100, 1000, 1/2)
y5 <- sapply(x * 1, distributionFunction, 500, 1000, 1/2)
plot(x,y1, type = "l", main = TeX('$\\lim_{n->inf}(P(\\sqrt{a / (1 - a)}R_{n}^{+} < x)) = 2\\Phi(x) - 1$'),
     xlab = "x", ylab = "probability")
lines(x,y2, col = "cyan1")
lines(x,y3, col = "brown1")
lines(x,y4, col = "green")
lines(x,y5, col = "darkgoldenrod1")
legend(3.05, 0.4, legend=c("n = 10", "n = 50", "n = 100", "n = 500"),
       col=c("cyan1", "brown1", "green", "darkgoldenrod1"), lty=1, bg = "whitesmoke")
dev.off()
