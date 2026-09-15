#### Functions that help to calculate the LOKAL EMOS POSTPROCESSING ####

calc_mu_sd_lok <- function(verification,coef_list,num_verification_days,n_cells,start_i) {
#calculate mu, sd from the coefficent list fast
mu_lok <- matrix(NA_real_, nrow = num_verification_days * n_cells, ncol = 4)
row_i <- 1
for(day in 1:num_verification_days){
  ens_day <- verification %>% filter(day_number == day + (start_i - 1)) %>%
    mutate(cell_index=as.numeric(cell_index)) %>% arrange(cell_index)
  mu <- coef_list[[day]][,1] + coef_list[[day]][,2] * ens_day$ens_mean
  
  t <- exp(coef_list[[day]][,3] + coef_list[[day]][,4] * ens_day$ens_sd)
  sd <- pmin(t, 4)
  rows <- row_i:(row_i + n_cells - 1)
  mu_lok[rows, ] <- cbind(mu,sd,day + (start_i - 1),seq_len(n_cells))
  row_i <- row_i + n_cells
  print(day)
}
mu_lok <- as.data.frame(mu_lok)
names(mu_lok) <- c("mu", "sd", "day_number", "cell_index")
mu_lok
}


#here would be smarter to parallelize over the cells not the sliding windows but fast enougth 
create_coef_list_lok <- function(full_df,adjacent_cells,n_cells,start_i,window_size,num_verification_days,shift=1){
  #calculates the LOKAL EMOS Regression for each sliding window and grid cell
  #works parrallel with future
  future::plan(multisession, workers = parallel::detectCores() - 1)
  handlers(global = TRUE)
  options(future.stdout = TRUE)
  handlers("progress")
  options(future.globals.maxSize=500*1024^2) #500 MB max pro worker
  options(future.debug = FALSE) #ganz hilfreich

  positions <- start_i:(num_verification_days+start_i-1)

  coef_names_local_mean_emos <- c("b_0", "b_1", "c", "d")
  n_coef_local_mean_emos <- length(coef_names_local_mean_emos)

  adjacent_list <- lapply(
    seq_len(n_cells),
    function(cell) adjacent_cells[cell, ]
  )
  full_df %<>% select(ens_mean,ens_sd,day_number,cell_index,y) %>% tibble()
  
  coef_list <- future_map(
    positions,
    function(position_nov){
      start_day <- position_nov - window_size - shift+1
      coef_mat <- matrix(NA,nrow = n_cells,ncol = n_coef_local_mean_emos)
     
       print(start_day:(position_nov-shift))
      print(length(start_day:(position_nov-shift)))
      
      colnames(coef_mat) <- coef_names_local_mean_emos
      for(cell in seq_len(n_cells)){
        dat <- full_df  %>% 
          filter(day_number %in% start_day:(position_nov-shift),cell_index %in% adjacent_list[[cell]])
        fit <- crch(y ~ ens_mean | ens_sd,data = dat, dist = "gaussian",type = "crps")
        coef_mat[cell, ] <- coef(fit)
      }
      message("Finished day ", position_nov)
      coef_mat
    },
    .progress =TRUE
  )
  gc
  future::plan(sequential)
  future::nbrOfWorkers()
  coef_list
}