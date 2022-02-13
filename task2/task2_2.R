library("latex2exp")
library("dplyr")

a <- 1/10
distribution <- c("uniform" = punif, "exponential" = pexp, "normal" = pnorm)
rand <- c("uniform" = runif, "exponential" = rexp, "normal" = rnorm)

stRenyiCriterion <- function(x, a, distribution, ...) {
  length <- length(x)
  df <- data.frame(edf = (c(1:length) - 1) / length, distribution = distribution(sort(x), ...)) %>% filter(edf > a)
  (abs(df$edf - df$distribution) / df$edf) %>% max() * sqrt(length(x))
}
stRenyiCriterionPlus <- function(x, a, distribution, ...) {
  length <- length(x)
  df <- data.frame(edf = (c(1:length) - 1) / length, distribution = distribution(sort(x), ...)) %>% filter(edf > a)
  ((df$edf - df$distribution) / df$edf) %>% max() * sqrt(length(x))
}
# 1.
pdf(file="1.pdf")
plot(density(replicate(500, stRenyiCriterion(runif(10), a, distribution$uniform))), 
     xlim = c(0, 6), ylim = c(0, 0.8),
     main = TeX("$R_n$ has a limiting distribution at $n \\rightarrow \\infty$"), col = "black", 
     xlab = "statistics of the Renyi criterion")
lines(density(replicate(500, stRenyiCriterion(rand$uniform(50), a, distribution$uniform))), col = "brown")
lines(density(replicate(500, stRenyiCriterion(rand$uniform(100), a, distribution$uniform))), col = "chartreuse4")
lines(density(replicate(500, stRenyiCriterion(rand$uniform(500), a, distribution$uniform))), col = "darkgoldenrod3")
lines(density(replicate(500, stRenyiCriterion(rand$uniform(1000), a, distribution$uniform))), col = "blueviolet")
legend(4.5, 0.8, legend=c("n = 10", "n = 50", "n = 100", "n = 500", "n = 1000"),
       col=c("black", "brown", "chartreuse4", "darkgoldenrod3", "blueviolet"), 
       lty=1, bg = "whitesmoke")
dev.off()
# 2.
pdf(file="2.pdf")
plot(density(replicate(500, stRenyiCriterion(rand$uniform(1000), a, distribution$uniform))), 
     xlim = c(0, 10), ylim = c(0, 0.5), 
     main = TeX("The asymptotic distribution of $R_n$ does not depend on the distribution function $F(x)$"), col = "black", 
     xlab = "statistics of the Renyi criterion")
lines(density(replicate(500, stRenyiCriterion(rand$exponential(1000), a, distribution$exponential))), 
      col = "brown")
lines(density(replicate(500, stRenyiCriterion(rand$normal(1000), a, distribution$normal))), 
      col = "chartreuse4")
legend(7.5, 0.5, legend=c("uniform", "exponential", "normal"),
       col=c("black", "brown", "chartreuse4"), 
       lty=1, bg = "whitesmoke")
dev.off()
# 3.
pdf(file="3.pdf")
par(lwd = 2)
x <- seq(0, 3.5, length = 300)
plot(x, pnorm(x) * 2 - 1, type = "l", xlim = c(0, 3.5), ylab = "y",
     main = TeX('Demonstration of the equation $\\lim_{n->inf}(P(\\sqrt{a / (1 - a)}R_{n}^{+} < x)) = 2\\Phi(x) - 1$'))
x <- replicate(300, sqrt(a/(1-a)) * stRenyiCriterionPlus(rand$uniform(10), a, distribution$uniform)) %>% sort()
lines(x, (c(1:300) - 1) / 300, col = "brown")
x <- replicate(300, sqrt(a/(1-a)) * stRenyiCriterionPlus(rand$uniform(100), a, distribution$uniform)) %>% sort()
lines(x, (c(1:300) - 1) / 300, col = "chartreuse4")
x <- replicate(300, sqrt(a/(1-a)) * stRenyiCriterionPlus(rand$uniform(1000), a, distribution$uniform)) %>% sort()
lines(x, (c(1:300) - 1) / 300, col = "darkgoldenrod3")
legend(2.5, 0.2, legend=c("n = 10", "n = 100", "n = 1000"),
       col=c("brown", "chartreuse4", "darkgoldenrod3"), 
       lty=1, bg = "whitesmoke")
dev.off()
