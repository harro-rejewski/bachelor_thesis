#1. create forecast ####
print("create ens_24 forecast")
ens_data_24 <- verification %>% select(., ens_1:ens_20)
ens_data_24 <- as.matrix(ens_data_24)

fcast_ecdf_24 <- list(
  F = function(x) {
    rowMeans(ens_data_24 <= x)
  },
  left = function(x){
    rowMeans(ens_data_24 < x)
  },
  right = function(x){
    rowMeans(ens_data_24 <= x)
  }
)
#2. add crps to verification df####
print("calc CRPS ens_24")
verification <- verification %>% 
  mutate(crps_emp = crps_sample(verification$y,ens_data_24))

#3. marginal calibration ####
print("marginal calibration ens_24")
marg_emp_24 <- margreldiag(fcast_ecdf_24,verification$y,resampling = FALSE,color="darkorange")

#4. probabilistic calibration ####
print("probabilistic calibration ens_24")
pit_emp_24 <- pitdiag(fcast_ecdf_24,verification$y,random_pit = TRUE,color="darkorange",)
pit_hist_emp_24 <- pithist(fcast_ecdf_24,verification$y,random_pit = TRUE,fill="darkorange")


#5. Quantilcalibration ####
# 25% - Qauntil
if(quant_cal){
print("quantil calibration ens_24")

print("quantil 25%")
X_25 <-rowQuantiles(ens_data_24, probs = 0.25, type = 1)
q_25 <- quant_plot(X_25,verification$y, resampling = FALSE,alpha = 0.25,color = "darkorange")
X_25_rc <- q_25[[1]]

q_25_emp_24 <- q_25[[2]] #Quantcal Plot

emp_decomp_q25_24 <- qs_decomposition(X_25, X_25_rc,verification$y,alpha=0.25) #decoposition
#mean(verification$y <= X_25_rc)

# 50% - Quantil
print("quantil 50%")
X_50 <-rowQuantiles(ens_data_24, probs = 0.5, type = 1)
q_50 <- quant_plot(X_50
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.5,color = "darkorange")
X_50_rc <- q_50[[1]]
#X_50_mg <- quantile(verification$y,p=0.5,type=1)

q_50_emp_24 <- q_50[[2]] #Quantcal Plot

emp_decomp_q50_24 <- qs_decomposition(X_50, X_50_rc,verification$y,alpha=0.5) #decomposition

# 75% - Quantil
print("quantil 75%")
X_75 <-rowQuantiles(ens_data_24, probs = 0.75, type = 1)
q_75 <- quant_plot(X_75
                   ,verification$y, 
                   resampling = FALSE,
                   alpha = 0.75,color = "darkorange")
X_75_rc <- q_75$x_rc 

q_75_emp_24 <- q_75$plot #Quantcal Plot

emp_decomp_q75_24 <- qs_decomposition(X_75, X_75_rc,verification$y,alpha=0.75) #decomposition
}

