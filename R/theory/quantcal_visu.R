
set.seed(200)
n <- 1000

# Daten generieren
mu <- rnorm(n, 0, 1)
t <- 2
tau <- sample(c(-t, t), n, replace = TRUE)
Y <- rnorm(n, mean = mu, sd = 1)

# Vorhersage-CDF
F_mix <- function(y, mu, tau) {
  0.5 * pnorm(y, mean = mu, sd = 1) +
    0.5 * pnorm(y, mean = mu + tau, sd = 1)
}
quantile_mix <- function(alpha, mu,tau) {
  f <- function(x) F_mix(x, mu, tau) - alpha
  uniroot(f, lower = mu - 6, upper = mu + 6)$root
}

# PIT-Werte
U <- mapply(F_mix, Y, mu, tau)

# Histogramm
hist(U, breaks = 15, col = "skyblue", main = "PIT Histogramm", xlab = "F(Y)")
abline(h = n/20, col = "red", lwd = 2)

alpha <- 0.75
#quantile_mix(alpha, mu, tau)
q_alpha <- mapply(quantile_mix, alpha = alpha, mu = mu, tau = tau)


alpha <- 0.50
#quantile_mix(alpha, mu, tau)
q_alpha <- mapply(quantile_mix, alpha = alpha, mu = mu, tau = tau)
quant_plot(q_alpha,Y,alpha=alpha,resampling = TRUE,size_annotate = 5,size_points = 0.8, alpha_points = 0.5, color_points = "grey60")$plot+
  coord_fixed(ylim = c(min(Y),max(Y)),xlim=c(min(Y),max(Y)),clip="on")+ #oder carthesian
  scale_x_continuous(breaks = seq(round(min(Y),0)-1, round(max(Y),0)+1, by = 1)) +
  scale_y_continuous(breaks = seq(round(min(Y),0)-1, round(max(Y),0)+1, by = 1)) +
  theme_bw(base_size = 15.4)


  
  




