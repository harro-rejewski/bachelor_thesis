
# Parameter
set.seed(200)
n <- 500
size <- 100
prob <- 0.3

Y <- rnorm(n,mean=5, sd=sqrt(25))


fcast1 <- list(
  F = function(x) {
    pnorm(x, ,mean=5,sd=sqrt(8))
  },
  sample = function(cases){
    rnorm(length(cases), mean=5, sd = sqrt(8))
  }
)
pit_under <- pitdiag(fcast1,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_under_hist <- pithist(fcast1,Y)

(p_under <- margreldiag(fcast1,Y))

#overdispersive
Z2 <- pnorm(Y,mean=5,sd=sqrt(50))
hist(Z2, breaks = 25, main="PIT overdispersive forecaster", col="navy")
fcast2 <- list(
  F = function(x) {
    pnorm(x, ,mean=5,sd=sqrt(50))
  },
  sample = function(x){
    rnorm(length(x), mean=5, sd = sqrt(50))
  }
)
pit_over <- pitdiag(fcast2,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_over_hist <- pithist(fcast2,Y)
(p_over <- margreldiag(fcast2,Y))

#negative biased forecaste
#Z3 <- pnorm(Y,mean=3,sd=sqrt(25)) hist(Z3, breaks = 25, main="PIT overdispersive forecaster", col="green")
fcast3 <- list(
  F = function(x) {
    pnorm(x, ,mean=3,sd=sqrt(25))
  },
  sample = function(x){
    rnorm(length(x), mean=3, sd = sqrt(25))
  }
)
pit_neg <- pitdiag(fcast3,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_neg_hist <- pithist(fcast3,Y)
p_neg <- margreldiag(fcast3,Y)


#positive biased forecaster
#Z4 <- pnorm(Y,mean=10,sd=sqrt(25)) hist(Z4, breaks = 25, main="PIT overdispersive forecaster", col="green")
fcast4 <- list(
  F = function(x) {
    pnorm(x, ,mean=7,sd=sqrt(25))
  },
  sample = function(x){
    rnorm(length(x), mean=7, sd = sqrt(25))
  }
)
pit_pos <- pitdiag(fcast4,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_pos_hist <- pithist(fcast4,Y)
p_pos <- margreldiag(fcast4,Y)


#negative biased and underdispersive
#Z5 <- pnorm(Y,mean=0,sd=sqrt(5)) hist(Z5, breaks = 25, main="PIT underdispersive forecaster", col="brown")
fcast5 <- list(
  F = function(x) {
    pnorm(x, ,mean=3,sd=sqrt(8))
  },
  sample = function(x){
    rnorm(length(x), mean=3, sd =sqrt(8))
  }
)
pit_und_neg <- pitdiag(fcast5,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_und_neg_hist <- pithist(fcast5,Y)
p_under_neg <- margreldiag(fcast5,Y)

#negative biased and overdispersive
#Z5 <- pnorm(Y,mean=-5,sd=sqrt(50))hist(Z5, breaks = 25, main="PIT underdispersive forecaster", col="brown")

fcast6 <- list(
  F = function(x) {
    pnorm(x, ,mean=3,sd=sqrt(50))
  },
  sample = function(x){
    rnorm(length(x), mean=3, sd = sqrt(50))
  }
)
pit_over_neg <- pitdiag(fcast6,Y,consistency_bands = TRUE)+labs(y=expression("PIT-Werte" <= alpha))
pit_over_neg_hist <-pithist(fcast6,Y)

p_over_neg <- margreldiag(fcast6,Y)


#positive biased and underdispersiver
#Z5 <- pnorm(Y,mean=7,sd=sqrt(8)) hist(Z5, breaks = 25, main="PIT underdispersive forecaster", col="brown")

fcast8 <- list(
  F = function(x) {
    pnorm(x, ,mean=7,sd=sqrt(8))
  },
  sample = function(x){
    rnorm(length(x), mean=7, sd =sqrt(8))
  }
)

pit_und_pos <- pitdiag(fcast8,Y,consistency_bands = TRUE) +labs(y=expression("PIT-Werte" <= alpha))
pit_und_pos_hist <- pithist(fcast8,Y)

p_under_pos <- margreldiag(fcast8,Y)


#positive biased and underdispersive
#Z5 <- pnorm(Y,mean=7,sd=sqrt(50))hist(Z5, breaks = 25, main="PIT underdispersive forecaster", col="brown")

fcast7 <- list(
  F = function(x) {
    pnorm(x, ,mean=7,sd=sqrt(50))
  },
  sample = function(x){
    rnorm(length(x), mean=7, sd = sqrt(50))
  }
)
pit_over_pos <- pitdiag(fcast7,Y,consistency_bands = TRUE) + labs(y=expression("PIT-Werte" <= alpha))
pit_over_pos_hist <- pithist(fcast7,Y)
p_over_pos <- margreldiag(fcast7,Y)


#11x11 portrait
(pit_under | pit_under_hist | pit_over | pit_over_hist) /
  (pit_neg | pit_neg_hist | pit_pos | pit_pos_hist) /
  (pit_und_neg | pit_und_neg_hist | pit_over_pos | pit_over_pos_hist) /
  (pit_und_pos | pit_und_pos_hist | pit_over_neg | pit_over_neg_hist ) +
  plot_annotation(tag_levels = "a")  &
  theme_bw(base_size = 15) &
  
rel_val <- .75
(p_under+labs(subtitle="unterdispersiv")+ theme(plot.subtitle = element_text(size = rel(rel_val)))
  | p_over+labs(subtitle="überdispersiv")+ theme(plot.subtitle = element_text(size = rel(rel_val)))
  | p_neg+labs(subtitle="negativer Bias")+ theme(plot.subtitle = element_text(size = rel(rel_val)))
  | p_pos+labs(subtitle="positiver Bias")+ theme(plot.subtitle = element_text(size = rel(rel_val)))
  ) /
  (p_under_neg+labs(subtitle="unterdispersiv \n+ negativer Bias")+ theme(plot.subtitle = element_text(size = rel(rel_val)))
   | p_under_pos+labs(subtitle="unterdispersiv \n+ positiver Bias")+theme(plot.subtitle = element_text(size = rel(rel_val)))
   | p_over_neg+labs(subtitle="überdispersiv \n+ negativer Bias")+theme(plot.subtitle = element_text(size = rel(rel_val)))
   |p_over_pos+labs(subtitle="überdispersiv \n+ positiver Bias")+theme(plot.subtitle = element_text(size = rel(rel_val)))
   )+
  plot_annotation(tag_levels = "a") &
  theme_bw(base_size = 15) 
