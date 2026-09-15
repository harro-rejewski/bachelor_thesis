# 1. statistical postprocessing ####
if(postprocess){
coef_names_mean_emos <- c("b_0", "b_1", "c", "d")
n_coef_mean_emos <- length(coef_names_mean_emos)
coef_array_mean_emos <- matrix(NA, nrow = num_verification_days, ncol = n_coef_mean_emos)
colnames(coef_array_mean_emos) <- coef_names_mean_emos

j <- 1
for (position_nov in start_i:last_day_index) {
  start_day <- position_nov - window_size
  print(start_day:(position_nov-1))
  print(length(start_day:(position_nov-1)))
  fit_mean_emos <- full %>% filter(day_number %in% start_day:(position_nov-1)) %>% crch(
    y ~ ens_mean| ens_sd,
    data = .,
    dist = "gaussian",
    type = "crps" #"crps" "ml" das geht auch 
  )
  coefs <-coef(fit_mean_emos)
  coef_array_mean_emos[j, ] <- as.numeric(coefs)
  j <- j+1
}

# 2. create forecast ####
verification <- verification %>%
  mutate(
    mu = coef_array_mean_emos[day_number-(start_i-1), 1] + coef_array_mean_emos[day_number-(start_i-1), 2] * ens_mean,
    sd = exp(coef_array_mean_emos[day_number-(start_i-1), 3]+coef_array_mean_emos[day_number-(start_i-1), 4]*ens_sd)
  )

fcast_glob_emos_24 <- list(
  F = function(x) {
    pnorm(x, mean = verification$mu, sd = verification$sd)
  },
  F_mg = function(x){
    sapply(x,function(t) mean(fcast_glob_emos_24$F(t)))
  },
  d_mg = function(x) {
    sapply(x, function(xx) {
      mean(dnorm(xx,mean = verification$mu,sd   = verification$sd))
    })
  }
)
}

fcast_quant_emos_glob_24 <- list(
  quantile = function(x){
    qnorm(x, mean = verification$mu, sd = verification$sd)
  }
)

#3. marginal calibration ####
print("marginal calibration emos_glob_24")
marg_emos_glob_24 <- margreldiag(fcast_glob_emos_24,verification$y,resampling=FALSE)

#4. probabilistic calibration ####
print("probabilistic calibration emos_glob_24")
pit_emos_glob_24 <- pitdiag(fcast_glob_emos_24,verification$y)
pit_hist_emos_glob_24 <- pithist(fcast_glob_emos_24,verification$y, fill="navy")

#5. add crps to verification df ####
print("calc CRPS emos_glob_24")
verification <- verification %>% 
  mutate(crps_emos_glob = crps_norm(verification$y,mean = verification$mu, sd = verification$sd))

#6. Quantilcalibration ####
if(quant_cal){
print("quantil calibration emos_glob_24")
print("quantil 25%")
# 25% - Quantil
X_25 <-qnorm(0.25,verification$mu,verification$sd)
q_25 <- quant_plot(X_25
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.25)
X_25_rc <- q_25[[1]]

emos_glob_q25_24 <- q_25[[2]] #Quantcal Plot
emos_glob_decomp_q25_24 <- qs_decomposition(X_25, X_25_rc,verification$y,alpha=0.25)
# 50% - Quantil
print("quantil 50%")
X_50 <-qnorm(0.5,verification$mu,verification$sd)
q_50 <- quant_plot(X_50
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.5)
X_50_rc <- q_50[[1]]

emos_glob_q50_24 <- q_50[[2]] #Quantcal Plot
emos_glob_decomp_q50_24 <- qs_decomposition(X_50, X_50_rc,verification$y,alpha=0.5) 
# 75% - Quantil
print("quantil 75%")
X_75 <- qnorm(0.75,verification$mu,verification$sd)
q_75 <- quant_plot(X_75
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.75)
X_75_rc<- q_75$x_rc 

emos_glob_q75_24 <- q_75$plot

emos_glob_decomp_q75_24 <- qs_decomposition(X_75, X_75_rc,verification$y,alpha=0.75) 
}


