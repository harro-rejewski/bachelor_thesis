#create reliability diagramm for probabilitic calibration ####
pitdiag <- function(fcast,y,alpha = 0.95,random_pit=FALSE,color="navy",consistency_bands=FALSE){
  n <- length(y)
  t <- seq(0,1,0.001)
  
  if(random_pit){
    u <- runif(length(y))
    pit <- fcast$left(y) + u * (fcast$right(y) - fcast$left(y))
    dist_PIT <- ecdf(pit)
  }
  else{
    dist_PIT <- ecdf(fcast$F(y))
  }
  
  lower_binom <- qbinom((1-alpha)/2, n, t)/n
  upper_binom <- qbinom(1-(1-alpha)/2, n, t)/n
  
  p <-  ggplot()
  if(consistency_bands){
    p <- p + geom_ribbon(aes(x=t, ymin = lower_binom, ymax = upper_binom), alpha = 0.6, fill="lightblue")
      
  }
  p<- p +
    #geom_ribbon(aes(x=t, ymin = dist_resamples_t_sorted[low,], ymax = dist_resamples_t_sorted[up,]), alpha = 0.4, fill = "gold")+
    geom_line(aes(x=t, y=dist_PIT(t)),linewidth = 0.6, color=color)+
    geom_abline(slope = 1, intercept = 0,, linetype = "dashed",color="grey40")+
    labs(y=expression("Anteil PIT-Werte" <= alpha), x=expression(alpha))+
    theme_bw()
  p
}

# only calcutlate PIT-Values ####
pitval <- function(fcast,y,random_pit=FALSE){
  # only calcutlate PIT-Values
  n <- length(y)
  t <- seq(0,1,0.001)
  
  if(random_pit){
    u <- runif(length(y))
    pit <- fcast$left(y) + u * (fcast$right(y) - fcast$left(y))
    dist_PIT <- ecdf(pit)
  }
  else{
    dist_PIT <- ecdf(fcast$F(y))
  }
  dist_PIT
}

coverage <- function(qcast,y,alpha){
  mean(qcast$quantile(alpha) >= y) 
}

#create the pit histogramm ####
pithist <- function(fcast,y,random_pit=FALSE, bins=20,fill="lightblue"){
  #create the pit histogramm
  if(random_pit){
    u <- runif(length(y))
    pit <- fcast$left(y) + u * (fcast$F(y) - fcast$left(y))
  }else{
    pit <- fcast$F(y)
  }
  ggplot()+
    geom_histogram(aes(x=pit,y=after_stat(density)),bins = bins,fill=fill,color="black")+
    labs(x="PIT-Werte",y="Dichte")+
    theme_bw()
}

#create reliability diagramm for marginal calibration ####
margreldiag <- function(fcast,y,alpha = 0.95,resampling=TRUE,n_resamples=500,color="navy",
                        linewidth=0.5){
  #create reliability diagramm for marginal calibration
  #y-observed points
  #t-selection of points for plotting
  n_pts <- 1000 # max number of points to determine the curve for plotting
  y <- sort(y)
  n <- length(y)
  if(n > n_pts){
    t <- sort(c(head(y,1),base::sample(y,n_pts),tail(y,1)))
  }else{
    t <- y
  }
  #prediction
  #fixws t for fcast ( mean(F_1(t),...,F_n(t)) for all 1)
  avg_fcast_y = sapply(t,function(x) mean(fcast$F(x)))
  #estimation of empirical marginal dist
  marg_dist_y = ecdf(y)(t)
  #consistency bands with resampling
  if(resampling){
    #+1 because r index starts at 1
    low <- floor(n_resamples * (1-alpha)/2)+1
    up <- n_resamples - low+1
    # Resampling from the average forecast distribution is the same as sampling from randomly drawn forecasts (cases)
    resampled_cases <- matrix(base::sample(1:length(y),size = n_resamples*length(y),replace = TRUE),ncol = n_resamples)
    resamples <- apply(resampled_cases,2,function(cases) fcast$sample(cases))
    marg_dist_y_resamples <- apply(resamples, 2, function(s) ecdf(s)(t))
    marg_dist_y_resamples_sorted = apply(marg_dist_y_resamples,1,sort) # sorted pointwise
    lower_quant <- marg_dist_y_resamples_sorted[low,]
    upper_quant <- marg_dist_y_resamples_sorted[up,]
  }
  p <- ggplot()
  if(resampling){
    p <- p+ geom_ribbon(aes(x=avg_fcast_y, ymin = lower_quant, ymax = upper_quant), alpha = 0.6, fill="lightblue")
  }
  (p<- p+ geom_line(aes(x=avg_fcast_y, y=marg_dist_y), linewidth = linewidth, color=color)+
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",color="grey40",linewidth=linewidth)+
      labs(x=expression(hat(F)[mg](y)), y = expression(widehat(P)(Y <= y)))+
      theme_bw())
}
