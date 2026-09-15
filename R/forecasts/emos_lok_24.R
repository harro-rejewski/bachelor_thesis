# 1. statistical postprocessing ####
if(postprocess){
coef_list <- create_coef_list_lok(full,adjacent_cells,n_cells,start_i,window_size,num_verification_days)
mu_sd_lok <- calc_mu_sd_lok(verification,coef_list,num_verification_days,n_cells,start_i)

verification <- verification %>% 
  mutate(
    day_number = as.numeric(day_number),
    mu_lok = mu_sd_lok$mu,
    sd_lok = mu_sd_lok$sd
  )

#2. create forecast ####
fcast_lok_emos <- list(
  F = function(x) {
    pnorm(x, mean = verification$mu_lok, sd = verification$sd_lok)
  },
  d_mg = function(x) {
    sapply(x,function(t) mean(dnorm(t, mean =  verification$mu_lok, sd = verification$sd_lok)))
  }
)
}

fcast_quant_emos_lok_24 <- list(
  quantile = function(x){
    qnorm(x, mean = verification$mu_lok, sd = verification$sd_lok)
  }
)

#3. add crps to verfication df ####
print("add CRPS emos_lok_24")
verification <- verification %>% 
  mutate(crps_lok_emos = crps_norm(verification$y,mean = mu_lok, sd = sd_lok))

#4. marginal calibration ####
print("marginal calibration emos_lok_24")
marg_emos_lok_24 <- margreldiag(fcast_lok_emos,verification$y,resampling=FALSE,color="cyan3")

#5. probabilistic calibration ####
print("probabilistic calibration emos_lok_24")
pit_emos_lok_24 <- pitdiag(fcast_lok_emos,verification$y,color="cyan3")
pit_hist_emos_lok_24 <- pithist(fcast_lok_emos,verification$y, fill="cyan3")

#6. quantile calibration ####
if(quant_cal){
print("quantile calibration emos_lok_24")
print("quantil 25%")
  
X_25 <-qnorm(0.25,verification$mu_lok,verification$sd_lok)
q_25 <- quant_plot(X_25
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.25,color="cyan3")
X_25_rc <- q_25[[1]]
emos_lok_q25_24 <-q_25[[2]] 
emos_lok_decomp_q25_24 <- qs_decomposition(X_25, X_25_rc,verification$y,alpha=0.25)

# 50% - Quantil
print("quantil 50%")
X_50 <-qnorm(0.5,verification$mu_lok,verification$sd_lok)
q_50 <- quant_plot(X_50,verification$y, resampling = FALSE,alpha = 0.5,color="cyan3")
X_50_rc <- q_50[[1]]
#X_50_mg <- quantile(verification$y,p=0.5,type=1)
emos_lok_q50_24 <- q_50[[2]] #Quantcal Plot

emos_lok_decomp_q50_24 <- qs_decomposition(X_50, X_50_rc,verification$y,alpha=0.5) 

# 75% - Quantil
print("quantil 75%")
X_75 <- qnorm(0.75,verification$mu_lok,verification$sd_lok)
q_75 <- quant_plot(X_75
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.75,color="cyan3")
X_75_rc<- q_75$x_rc 
emos_lok_q75_24 <- q_75$plot
emos_lok_decomp_q75_24 <- qs_decomposition(X_75, X_75_rc,verification$y,alpha=0.75) 
}