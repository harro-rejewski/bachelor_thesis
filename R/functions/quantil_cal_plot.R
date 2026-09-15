#quantile reliability diagrams
quant_plot <- function(x,y, alpha=0.5,
                       resampling = TRUE,points=TRUE,n_resamples = 100, region_level = 0.95,color="navy",
                       size_annotate=7,size_points=0.1,alpha_points=0.15,color_points="grey80",linewidth=0.7){
  pava = function(x,y){
    #Use ranking of predictor values and break ties by ordering the corresponding instances in order of decreasing observations
    ranking = match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE)))
    return(gpava(ranking,y,solver = weighted.fractile,p = alpha,ties = "primary")$x)
  }
  ord_x <- order(x)
  x <- x[ord_x]
  y <- y[ord_x]

  #recalibrated values
  x_rc <- pava(x,y)
  
  #residuals
  res = y - x
  x_sub <- x
  
  #lower_sim <- mean(x)
  #upper_sim <- mean(x)
  if(resampling){
    low <- floor(n_resamples * (1-region_level)/2)+1
    up <- n_resamples - low +1
    marg <- function(x) quantile(x,alpha,type = 1)
    #paper alg. 4
    c <- marg(res)
    resamples <- sapply(1:n_resamples,function(i) x + sample(res,length(x),replace = TRUE))
    resamples <- resamples - c
    x_rc_resamples <- apply(resamples, 2, function(y) pava(x,y))
    x_rc_resamples_sorted <- apply(x_rc_resamples,1,sort)
    lower_sim <- x_rc_resamples_sorted[low,]
    upper_sim <- x_rc_resamples_sorted[up,]
  }

  p <- ggplot()
  
  label <- paste0(
    "atop('Überdeckung' == ", round(mean(y <= x), 3),
    ", 'Überdeckung(rekal.)' == ", round(mean(y <= x_rc), 3),
    ")"
  )
  

  if(resampling){
    #hier jetzt x_sub
    p <- p+geom_ribbon(aes(x=x_sub, ymin = lower_sim, ymax = upper_sim), alpha = 0.6, fill="lightblue")
  }
  if(points){
    p <- p+geom_point(aes(x=x, y=y),alpha=alpha_points,size=size_points,color=color_points)
  }
  #max_y <- max(x_rc)
  (p <- p+
      geom_line(aes(x=x, y=x_rc),linewidth=linewidth,color=color)+
      geom_abline(slope = 1, intercept = 0,, linetype = "dashed",color="grey40",linewidth = linewidth)+
      labs(x=paste0("vorhergesagtes ", sprintf("%.0f", alpha*100),"% Quantil"),y="rekalibrierte Werte")+
      annotate(
        "label",
        x = -Inf,
        y = Inf,
        label = label,
        hjust = -0.05,
        vjust = 1.05,
        size = size_annotate,
        parse = TRUE
      )+
      theme_bw())
  result <- list()
  result[[1]] <- x_rc[order(ord_x)]
  result[[2]] <- p 
  names(result) <- c("x_rc", "plot")
  return(result)
}

