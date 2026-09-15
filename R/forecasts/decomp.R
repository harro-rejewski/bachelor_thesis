#uses functions from decomp_plot_funs.R
#1. Conduct CRPS-Decomposition (this takes a lot of time)####
res_emp <- crps_qs_decomp_emp_future(ens_data_24,verification$y)
res_emp_48 <- crps_qs_decomp_emp_future(ens_data_48,verification_48$y) 
res_emos <- crps_qs_decomp_future(fcast_quant_emos_glob_24,verification$y,n=25)
res_emos_48 <- crps_qs_decomp_future(fcast_quant_emos_glob_48,verification_48$y,n=25)
res_emos_lok <- crps_qs_decomp_future(fcast_quant_emos_lok_24,verification$y,n=25)
res_emos_lok_48 <- crps_qs_decomp_future(fcast_quant_emos_lok_48,verification_48$y,n=25)

#2. create decomposition data sets for [crps, qs 5,25,50,75,95] ####
decomp_crps <- make_decomp_df(
  decomp_list = list(res_emp,res_emos,res_emos_lok,res_emp_48,
                     res_emos_48,res_emos_lok_48
  ),
  factor=1,
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)

# QS Decomp
#for 25%,50%,75% decompositon already exists (because quantile calibration daigramm)
# 25%
decomp_q25 <- make_decomp_df(
  decomp_list = list(emp_decomp_q25_24,
                     emos_glob_decomp_q25_24,
                     emos_lok_decomp_q25_24,
                     emp_decomp_q25_48,
                     emos_glob_decomp_q25_48,
                     emos_lok_decomp_q25_48
  ),
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)
#50 % 
decomp_q50 <- make_decomp_df(
  decomp_list = list(emp_decomp_q50_24,
                     emos_glob_decomp_q50_24,
                     emos_lok_decomp_q50_24,
                     emp_decomp_q50_48,emos_glob_decomp_q50_48,emos_lok_decomp_q50_48
  ),
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)
#75%
decomp_q75 <- make_decomp_df(
  decomp_list = list(emp_decomp_q75_24,
                     emos_glob_decomp_q75_24,
                     emos_lok_decomp_q75_24,
                     emp_decomp_q75_48,
                     emos_glob_decomp_q75_48,emos_lok_decomp_q75_48
  ),
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)
#5%
# make decomposition
emp_decomp_q05_24 <- full_qs_alpha_decomp(ens_data = ens_data_24, y = verification$y, alpha = .05)
emos_glob_decomp_q05_24 <- full_qs_alpha_decomp(fcast = fcast_quant_mean_emos, y = verification$y, alpha = .05)
emos_lok_decomp_q05_24 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_lok, y = verification$y, alpha = .05)
emp_decomp_q05_48 <- full_qs_alpha_decomp(ens_data = ens_data_48, y = verification$y, alpha = .05)
emos_glob_decomp_q05_48 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_glob_48, y = verification$y, alpha = .05)
emos_lok_decomp_q05_48 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_lok_48, y = verification$y, alpha = .05)

decomp_q05 <- make_decomp_df(
  decomp_list = list(emp_decomp_q05_24,emos_glob_decomp_q05_24,emos_lok_decomp_q05_24,emp_decomp_q05_48,emos_glob_decomp_q05_48,emos_lok_decomp_q05_48
  ),
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)

#95%
# make decomposition
emp_decomp_q95_24 <- full_qs_alpha_decomp(ens_data = ens_data_24, y = verification$y, alpha = .95)
emos_glob_decomp_q95_24 <- full_qs_alpha_decomp(fcast = fcast_quant_mean_emos, y = verification$y, alpha = .95)
emos_lok_decomp_q95_24 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_lok, y = verification$y, alpha = .95)

emp_decomp_q95_48 <- full_qs_alpha_decomp(ens_data = ens_data_48, y = verification$y, alpha = .95)
emos_glob_decomp_q95_48 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_glob_48, y = verification$y, alpha = .95)
emos_lok_decomp_q95_48 <- full_qs_alpha_decomp(fcast = fcast_quant_emos_lok_48, y = verification$y, alpha = .95)

decomp_q95 <- make_decomp_df(
  decomp_list = list(emp_decomp_q95_24,emos_glob_decomp_q95_24,emos_lok_decomp_q95_24,emp_decomp_q95_48,emos_glob_decomp_q95_48,emos_lok_decomp_q95_48),
  model_labels = c("emp_ens_24","emos_glob_24","emos_lok_24",
                   "emp_ens_48","emos_glob_48","emos_lok_48")
)