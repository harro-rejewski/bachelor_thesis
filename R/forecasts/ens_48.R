#1. create forecast ####
print("create ens_48 forecast")
ens_data_48 <- verification_48 %>% select(., ens_1:ens_20)
ens_data_48 <- as.matrix(ens_data_48)

fcast_ecdf_48 <- list(
  F = function(x) {
    rowMeans(ens_data_48 <= x)
  },
  left = function(x){
    rowMeans(ens_data_48 < x)
  },
  right = function(x){
    rowMeans(ens_data_48 <= x)
  }
)
#2. add crps to verification_48 df ####
print("calc CRPS ens_48")
verification_48 <- verification_48 %>% 
  mutate(crps_emp = crps_sample(verification_48$y,ens_data_48))

#3. marginal calibration ####
print("marginal calibration ens_48")
marg_emp_48 <- margreldiag(fcast_ecdf_48,verification_48$y,resampling = FALSE,color="darkorange")

#4. probabilistic calibration ####
print("probabilistic calibration ens_48")
pit_emp_48 <- pitdiag(fcast_ecdf_48,verification_48$y,random_pit = TRUE,color="darkorange",)
pit_hist_emp_48 <- pithist(fcast_ecdf_48,verification_48$y,random_pit = TRUE,fill="darkorange")


#5. Quantilcalibration ####
# 25% - Qauntil
if(quant_cal){
print("quantil calibration ens_24")
print("quantil 25%")
X_25 <-rowQuantiles(ens_data_48, probs = 0.25, type = 1)
q_25 <- quant_plot(X_25,verification_48$y, resampling = FALSE,alpha = 0.25,color = "darkorange")
X_25_rc <- q_25[[1]]

q_25_emp_48 <- q_25[[2]] #Quantcal Plot

emp_decomp_q25_48 <- qs_decomposition(X_25, X_25_rc,verification_48$y,alpha=0.25)
#mean(verification_48$y <= X_25_rc)

# 50% - Quantil
print("quantil 50%")
X_50 <-rowQuantiles(ens_data_48, probs = 0.5, type = 1)
q_50 <- quant_plot(X_50
           ,verification_48$y, 
           resampling = FALSE,
           alpha = 0.5,color = "darkorange")
X_50_rc <- q_50[[1]]
#X_50_mg <- quantile(verification_48$y,p=0.5,type=1)

q_50_emp_48 <- q_50[[2]] #Quantcal Plot

emp_decomp_q50_48 <- qs_decomposition(X_50, X_50_rc,verification_48$y,alpha=0.5) 

# 75% - Quantil
print("quantil 75%")
X_75 <-rowQuantiles(ens_data_48, probs = 0.75, type = 1)
q_75 <- quant_plot(X_75
           ,verification_48$y, 
           resampling = FALSE,
           alpha = 0.75,color = "darkorange")
X_75_rc <- q_75$x_rc 

q_75_emp_48 <- q_75$plot #Quantcal Plot

emp_decomp_q75_48 <- qs_decomposition(X_75, X_75_rc,verification_48$y,alpha=0.75)
}
