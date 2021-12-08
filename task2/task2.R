library("latex2exp")
library("dplyr")

a <- 1/10
stRenyiCriterionUnif <- function(n, a, right = 0, left = 1) {
  x <- runif(n, right, left)
  emp.cdf <- ecdf(x)
  df <- data.frame(emp.cdf = emp.cdf(x), punif = punif(x, right, left)) %>% filter(punif > a)
  (abs(df$emp.cdf - df$punif) / df$punif) %>% max() * sqrt(n)
}
stRenyiCriterionNorm <- function(n, a, mean = 0, sd = 1) {
  x <- rnorm(n, mean, sd)
  emp.cdf <- ecdf(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pnorm = pnorm(x, mean, sd)) %>% filter(pnorm > a)
  (abs(df$emp.cdf - df$pnorm) / df$pnorm) %>% max() * sqrt(n)
}
stRenyiCriterionExp <- function(n, a, y = 1) {
  x <- rexp(n, y)
  emp.cdf <- ecdf(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pexp = pexp(x, y)) %>% filter(pexp > a)
  (abs(df$emp.cdf - df$pexp) / df$pexp) %>% max() * sqrt(n)
}

# 1.
pdf(file="1.pdf")
plot(density(replicate(500, stRenyiCriterionUnif(10, a))), xlim = c(0, 6), ylim = c(0, 0.5),
     main = TeX("$R_n$ has a limiting distribution at $n \\rightarrow \\infty$"), col = "black", 
     xlab = "statistics of the Renyi criterion")
lines(density(replicate(500, stRenyiCriterionUnif(50, a))), col = "brown")
lines(density(replicate(500, stRenyiCriterionUnif(100, a))), col = "chartreuse4")
lines(density(replicate(500, stRenyiCriterionUnif(500, a))), col = "darkgoldenrod3")
lines(density(replicate(500, stRenyiCriterionUnif(1000, a))), col = "blueviolet")
legend(4, 0.5, legend=c("n = 10", "n = 50", "n = 100", "n = 500", "n = 1000"),
       col=c("black", "brown", "chartreuse4", "darkgoldenrod3", "blueviolet"), 
       lty=1, bg = "whitesmoke")
dev.off()
# 2.
pdf(file="2.pdf")
plot(density(replicate(500, stRenyiCriterionUnif(1000, a))), xlim = c(0, 6), ylim = c(0, 0.5), 
     main = TeX("The asymptotic distribution of $R_n$ does not depend on the distribution function $F(x)$"), col = "black", 
     xlab = "statistics of the Renyi criterion")
lines(density(replicate(500, stRenyiCriterionExp(1000, a))), col = "brown")
lines(density(replicate(500, stRenyiCriterionNorm(1000, a))), col = "chartreuse4")
legend(3.5, 0.5, legend=c("uniform", "exponential", "normal"),
       col=c("black", "brown", "chartreuse4"), 
       lty=1, bg = "whitesmoke")
dev.off()
# 3.
stRenyiCriterionUnifPlus <- function(n, a, right = 0, left = 1) {
  x <- runif(n, right, left)
  emp.cdf <- ecdf(x)
  df <- data.frame(emp.cdf = emp.cdf(x), punif = punif(x, right, left)) %>% filter(punif > a)
  ((df$emp.cdf - df$punif) / df$punif) %>% max() * sqrt(n)
}

pdf(file="3.pdf")
par(lwd = 2)
x <- seq(0, 3.5, length = 300)
y1 <- ecdf(replicate(300, sqrt(a/(1-a)) * stRenyiCriterionUnifPlus(10, a)))
y2 <- ecdf(replicate(300, sqrt(a/(1-a)) * stRenyiCriterionUnifPlus(100, a)))
y3 <- ecdf(replicate(300, sqrt(a/(1-a)) * stRenyiCriterionUnifPlus(1000, a)))
plot(x, pnorm(x) * 2 - 1, type = "l", xlim = c(0, 3.5), ylab = "y",
     main = TeX('Demonstration of the equation $\\lim_{n->inf}(P(\\sqrt{a / (1 - a)}R_{n}^{+} < x)) = 2\\Phi(x) - 1$'))
lines(x, y1(x), col = "brown")
lines(x, y2(x), col = "chartreuse4")
lines(x, y3(x), col = "darkgoldenrod3")
legend(2.5, 0.2, legend=c("n = 10", "n = 100", "n = 1000"),
       col=c("brown", "chartreuse4", "darkgoldenrod3"), 
       lty=1, bg = "whitesmoke")
dev.off()