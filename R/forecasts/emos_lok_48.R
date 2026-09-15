# 1. statistical postprocessing ####
if(postprocess){
print("postprocess emos_lok_48")
coef_list <- create_coef_list_lok(full_48,
                                  adjacent_cells,
                                  n_cells,start_i,
                                  window_size,
                                  num_verification_days,
                                  shift = 2)
mu_sd_lok <- calc_mu_sd_lok(verification_48,coef_list,num_verification_days,n_cells,start_i)

#2. create forcast ####
verification_48 <- verification_48 %>% 
  mutate(
    mu_lok = mu_sd_lok$mu,
    sd_lok = mu_sd_lok$sd
  )

fcast_lok_emos <- list(
  F = function(x) {
    pnorm(x, mean = verification_48$mu_lok, sd = verification_48$sd_lok)
  },
  d_mg = function(x) {
    sapply(x,function(t) mean(dnorm(t, mean =  verification_48$mu_lok, sd = verification_48$sd_lok)))
  }
)
}

fcast_quant_emos_lok_48 <- list(
  quantile = function(x){
    qnorm(x, mean = verification_48$mu_lok, sd = verification_48$sd_lok)
  }
)

#3. add crps to verfication df ####
print("add CRPS emos_lok_48")
verification_48 <- verification_48 %>% 
  mutate(crps_lok_emos = crps_norm(verification_48$y,mean = mu_lok, sd = sd_lok))

#4. marginal calibration ####
print("marginal calibration emos_lok_48")
marg_emos_lok_48 <- margreldiag(fcast_lok_emos,verification_48$y,resampling=FALSE,color="cyan3")

#5. probabilistic calibration ####
print("probabilistic calibration emos_lok_48")
pit_emos_lok_48 <- pitdiag(fcast_lok_emos,verification_48$y,color="cyan3")
pit_hist_emos_lok_48 <- pithist(fcast_lok_emos,verification_48$y, fill="cyan3")

#6. quantile calibration ####
if(quant_cal){
print("quantile calibration emos_lok_48")
print("quantil 25%")
  
X_25 <-qnorm(0.25,verification_48$mu_lok,verification_48$sd_lok)
q_25 <- quant_plot(X_25,verification_48$y, resampling = FALSE, alpha = 0.25,color="cyan3")
X_25_rc <- q_25[[1]]

emos_lok_q25_48 <-q_25[[2]] #Quantcal Plot

emos_lok_decomp_q25_48 <-qs_decomposition(X_25, X_25_rc,verification_48$y,alpha=0.25) #decompositon

# 50% - Quantil
print("quantil 50%")
X_50 <-qnorm(0.5,verification_48$mu_lok,verification_48$sd_lok)
q_50 <- quant_plot(X_50,verification_48$y, resampling = FALSE,alpha = 0.5,color="cyan3")
X_50_rc <- q_50[[1]]
#X_50_mg <- quantile(verification_48$y,p=0.5,type=1)

emos_lok_q50_48 <- q_50[[2]] #Quantcal Plot

emos_lok_decomp_q50_48 <- qs_decomposition(X_50, X_50_rc,verification_48$y,alpha=0.5) 


# 75% - Quantil
print("quantil 75%")
X_75 <- qnorm(0.75,verification_48$mu_lok,verification_48$sd_lok)
q_75 <- quant_plot(X_75,verification_48$y, resampling = FALSE, alpha = 0.75,color="cyan3")
X_75_rc<- q_75$x_rc 

emos_lok_q75_48 <- q_75$plot

emos_lok_decomp_q75_48 <- qs_decomposition(X_75, X_75_rc,verification_48$y,alpha=0.75) 
}

