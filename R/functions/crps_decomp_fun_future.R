#parallel::detectCores()
#future::availableCores()
#future::supportsMulticore()
#options(future.debug = FALSE) #ganz hilfreich

#fcast has fun fcast$quantile(alpha)

crps_qs_decomp_future <- function(fcast,y,n=25){
  #calc values for numerical integration
  plan(multisession)
  options(future.globals.maxSize=500*1024^2) #500 MB max pro worker
  future::plan()
  
  gl <- gaussLegendre(n, 0, 1)
  stütz <- gl$x
  gew <- gl$w
  rm(gl)
  
  #hier was entspanters 
  quantil_mat <- purrr::map_dfc(stütz, ~ fcast$quantile(.x)) %>% as.matrix()
  res <- future_map(
    seq_along(stütz),
    function(i){
      x <- quantil_mat[, i] 
      ranking <- match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE), method = "radix"))
      x_rc <- gpava(ranking,y,solver = weighted.fractile,p = stütz[i],ties = "primary")$x
      #Calc and save the quantile Score
      c(s = mean(qs_quantiles(y, x, stütz[i])),
        s_rc = mean(qs_quantiles(y, x_rc, stütz[i])),
        s_mg = mean(qs_quantiles(y, quantile(y, stütz[i], type = 1),stütz[i])))
    },
    .progress = TRUE
  )
  res <- do.call(rbind, res)
  
  #now calc the hole crps
  crps <- 2*sum(gew * res[,1])
  crps_rc <- 2*sum(gew * res[,2])
  crps_mg_qs <-  2*sum(gew * res[,3]) #mg direct from the quantile integral approx
  y <- sort(y)
  n <- length(y)
  crps_mg <- sum((2 * seq_len(n) - n - 1) * y) / n^2  #unc_0
  #decompositon
  crps_mcb <- crps - crps_rc
  crps_dsc <- crps_mg - crps_rc
  crps_unc <- crps_mg
  
  #return decomp
  res <- list()
  res[[1]] <- c(crps=crps, crps_rc = mean(crps_rc), crps_mg = crps_unc)
  res[[2]] <- c(mcb=crps_mcb, dsc=crps_dsc, unc=crps_unc)
  crps_skill <- (crps_dsc- crps_mcb) / crps_unc
  res[[3]] <- crps_skill
  res[[4]] <- c(unc_0=crps_mg, unc_qs=crps_mg_qs)
  names(res) <- c("mean_scores", "components", "skill","unc")
  
  gc()
  plan(sequential)
  return(res)
}



crps_qs_decomp_emp_future <- function(ens_data,y){
  plan(multisession)
  options(future.globals.maxSize=500*1024^2) #500 MB max pro worker
  future::plan()
  #calc values for numerical integration
  m <- ncol(ens_data) #ensemble size
  z <- t(apply(ens_data, 1, sort))
  alpha <- (2*(1:m) - 1)/(2*m)
  crps <- (2/m) * rowSums(sweep((y <= z), 2, alpha, "-") *sweep(z, 1, y, "-"))
  
  # z_rc <- matrix(NA, nrow = n, ncol = m)
  # for(k in 1:m){
  #   x <- z[, k]   # k-tes Ensemblequantil
  #   ranking <- match(seq_along(x), order(x, y,decreasing = c(FALSE, TRUE),method = "radix"))
  #   z_rc[,k] <- gpava(ranking_for_k,y,solver = weighted.fractile,p = alpha[k],ties = "primary")$x
  # }
  with_progress({
    
    p <- progressor(steps = m)
    z_rc <- future_sapply(seq_len(m),function(k) {
        x <- z[, k]
        ranking <- match(seq_along(x), order(x, y,decreasing = c(FALSE, TRUE),method = "radix"))
        gpava(ranking,y,solver = weighted.fractile,p = alpha[k],ties = "primary")$x
    }
  )
  })
  
  crps_rc <- (2/m) * rowSums(sweep((y <= z_rc), 2, alpha, "-") *sweep(z_rc, 1, y, "-"))
  quantile(y, alpha, type = 1)
  z_mg <- matrix(rep(quantile(y, alpha, type = 1), each = nrow(ens_data)),nrow = nrow(ens_data),byrow = FALSE)
  crps_mg_qs <- (2/m) * rowSums(sweep((y <= z_mg), 2, alpha, "-") *sweep(z_mg, 1, y, "-"))
  y <- sort(y)
  n <- length(y)
  crps_mg <- sum((2 * seq_len(n) - n - 1) * y) / n^2  #unc_0
  
  #decompositon
  crps_mcb <- mean(crps) - mean(crps_rc)
  crps_dsc <- mean(crps_mg) - mean(crps_rc)
  crps_unc <- mean(crps_mg)
  
  #return decomp
  res <- list()
  res[[1]] <- c(crps=mean(crps), crps_rc = mean(crps_rc), crps_mg = crps_unc)
  res[[2]] <- c(mcb=crps_mcb, dsc=crps_dsc, unc=crps_unc)
  crps_skill <- (crps_dsc- crps_mcb) / crps_unc
  res[[3]] <- crps_skill
  res[[4]] <- c(unc_0=crps_mg, unc_qs=mean(crps_mg_qs))
  names(res) <- c("mean_scores", "components", "skill","unc")
  gc()
  plan(sequential)
  return(res)
}

#nicht vergessen das dann wieder zurückzusetzen




#fcast has fun fcast$quantile(alpha)
crps_qs_decomp <- function(fcast,y,n=25){
  #calc values for numerical integration
  gl <- gaussLegendre(n, 0, 1)
  stütz <- gl$x
  gew <- gl$w
  rm(gl)
  
  res <- c()
  for (alpha_it in stütz){
    x <- fcast$quantile(alpha_it)
    ranking <- match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE)))
    x_rc <- gpava(ranking,y,solver = weighted.fractile,p = alpha_it,ties = "primary")$x
    
    #Calc and save the quantile Score
    s <- mean(qs_quantiles(y,x,alpha_it))
    s_rc <- mean(qs_quantiles(y,x_rc,alpha_it))
    s_mg <- mean(qs_quantiles(y,quantile(y,alpha_it,type = 1),alpha_it))
    res <- rbind(res,c(s,s_rc,s_mg))
  }
  #now calc the hole crps
  crps <- 2*sum(gew * res[,1])
  crps_rc <- 2*sum(gew * res[,2])
  crps_mg_qs <-  2*sum(gew * res[,3]) #mg direct from the quantile integral approx
  y <- sort(y)
  n <- length(y)
  crps_mg <- sum((2 * seq_len(n) - n - 1) * y) / n^2  #unc_0
  #decompositon
  crps_mcb <- crps - crps_rc
  crps_dsc <- crps_mg - crps_rc
  crps_unc <- crps_mg
  
  #return decomp
  res <- list()
  res[[1]] <- c(crps=crps, crps_rc = mean(crps_rc), crps_mg = crps_unc)
  res[[2]] <- c(mcb=crps_mcb, dsc=crps_dsc, unc=crps_unc)
  crps_skill <- (crps_dsc- crps_mcb) / crps_unc
  res[[3]] <- crps_skill
  res[[4]] <- c(unc_0=crps_mg, unc_qs=crps_mg_qs)
  names(res) <- c("mean_scores", "components", "skill","unc")
  return(res)
}


crps_qs_decomp_emp <- function(ens_data,y){
  #calc values for numerical integration
  m <- ncol(ens_data) #ensemble size
  n <- nrow(ens_data)
  z <- t(apply(ens_data, 1, sort))
  alpha <- (2*(1:m) - 1)/(2*m)
  crps <- (2/m) * rowSums(sweep((y <= z), 2, alpha, "-") *sweep(z, 1, y, "-"))
  
  z_rc <- matrix(NA, nrow = n, ncol = m)
  for(k in 1:m){
    x <- z[, k]   # k-tes Ensemblequantil
    ranking <- match(seq_along(x), order(x, y,decreasing = c(FALSE, TRUE),method = "radix"))
    z_rc[,k] <- gpava(ranking,y,solver = weighted.fractile,p = alpha[k],ties = "primary")$x
  }
  
  crps_rc <- (2/m) * rowSums(sweep((y <= z_rc), 2, alpha, "-") *sweep(z_rc, 1, y, "-"))
  quantile(y, alpha, type = 1)
  z_mg <- matrix(rep(quantile(y, alpha, type = 1), each = nrow(ens_data)),nrow = nrow(ens_data),byrow = FALSE)
  crps_mg_qs <- (2/m) * rowSums(sweep((y <= z_mg), 2, alpha, "-") *sweep(z_mg, 1, y, "-"))
  y <- sort(y)
  n <- length(y)
  crps_mg <- sum((2 * seq_len(n) - n - 1) * y) / n^2  #unc_0
  
  #decompositon
  crps_mcb <- mean(crps) - mean(crps_rc)
  crps_dsc <- mean(crps_mg) - mean(crps_rc)
  crps_unc <- mean(crps_mg)
  
  #return decomp
  res <- list()
  res[[1]] <- c(crps=mean(crps), crps_rc = mean(crps_rc), crps_mg = crps_unc)
  res[[2]] <- c(mcb=crps_mcb, dsc=crps_dsc, unc=crps_unc)
  crps_skill <- (crps_dsc- crps_mcb) / crps_unc
  res[[3]] <- crps_skill
  res[[4]] <- c(unc_0=crps_mg, unc_qs=mean(crps_mg_qs))
  names(res) <- c("mean_scores", "components", "skill","unc")
  return(res)
}
