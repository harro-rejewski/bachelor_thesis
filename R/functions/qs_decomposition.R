qs_decomposition <- function(x,x_rc,y, alpha){
  res <- list()
  x_mg <- quantile(y,probs=alpha,type=1)
  qs <- mean(qs_quantiles(y,x,alpha = alpha))
  qs_rc <- mean(qs_quantiles(y,x_rc,alpha = alpha))
  qs_mg <- mean(qs_quantiles(y,x_mg,alpha = alpha))
  
  res[[1]] <- c(qs,qs_rc,qs_mg)
  #Zerlegung
  mcb <- qs - qs_rc
  dsc <- qs_mg - qs_rc
  unc <- qs_mg
  
  res[[2]] <- c(mcb,dsc,unc)
  # skill score
  qs_skill <- (dsc- mcb) / qs_mg
  res[[3]] <- qs_skill
  names(res) <- c("mean_scores", "components", "skill")
  names(res[[1]]) <- paste0(c("qs", "qs_rc", "qs_mg"), "_", alpha)
  names(res[[2]]) <- paste0(c("mcb", "dsc", "unc"), "_", alpha)
  names(res[[3]]) <- paste0(c("qs_skill"),"_",alpha)
  return(res)
}
