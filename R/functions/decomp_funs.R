#create the df for a decomposition ####
make_decomp_df <- function(decomp_list, model_labels, factor = 2) {
  decomp_df <- do.call(
    rbind,
    lapply(decomp_list, function(x) {
      factor * c(x$mean_scores[1], x$components)
    })
  ) |>
    data.frame() |>
    rename_with(~ str_remove(.x, "_.*")) |>
    mutate(unc = mean(unc)) |>
    rownames_to_column(var = "model") |>
    mutate(
      model = model_labels,
      across(where(is.numeric), ~ round(.x, 2)),
      skill =  round((dsc- mcb) / unc,2)
    ) |> 
    mutate(horizon = str_extract(model, "\\d+$"),
           model = str_remove(model, "_\\d+$"),
           model = factor(
             model,
             levels = c("emp_ens", "emos_glob", "emos_lok")
           ))
  decomp_df
}

#create the decompostion plot ####
plot_decomp <- function(decomp_df, iso, xmin, xmax, ymin, ymax) {
  ggplot(data = decomp_df) +
    geom_abline(
      data = iso,aes(intercept = intercept, slope = slope),color = "lightgray",alpha = 0.6,size = 0.8
    )+
    geom_labelabline(
      data = iso,
      aes(intercept = intercept, slope = slope, label = label),
      color = "gray50",
      hjust = 0.85,
      size = 12 * 0.36,
      text_only = TRUE,
      boxcolour = NA,
      straight = TRUE
    ) +
    geom_point(
      aes(x = mcb, y = dsc, fill = model, shape = horizon),
      alpha = 0.7,
      size = 5
      )+
    scale_fill_manual(values = c(
      "emp_ens"="darkorange",
      "emos_glob"="navy",
      "emos_lok"="cyan3"),
      labels = c("empirisch","global","lokal"))+
    scale_shape_manual(values=c("24"=21,"48"=23))+
    labs(x = "MCB", y = "DSC",fill="Vorhersage",shape="Horizont") +
    guides(
      fill = guide_legend(
        override.aes = list(shape = 22,colour = "black",alpha = .7,order=1)
      ),
      shape = guide_legend(order=2)
    )+
    annotate(
      "label",
      x = Inf,
      y = Inf,
      label = paste0("UNC = ", round(mean(decomp_df$unc), 2)),
      hjust = 1,
      vjust = 1
    ) +
    coord_fixed(
      ratio = 0.5,
      xlim = c(max(0,xmin), xmax),
      ylim = c(max(0,ymin), ymax)
    )+
    scale_y_continuous(n.breaks = 6)+
    theme_bw(base_size = 15) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      aspect.ratio = 1,
      legend.position = "bottom"
    )
}


#Decompositon of the Quantile score ####
full_qs_alpha_decomp <- function(fcast=NULL,ens_data=NULL,y,alpha){
  if(is.null(fcast)){
    x <-rowQuantiles(ens_data, probs = alpha, type = 1)
  }
  else{
    x <- fcast$quantile(alpha)
  }
  ranking <- match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE)))
  x_rc <- gpava(ranking,y,solver = weighted.fractile,p = alpha,ties = "primary")$x
  qs_decomposition(x,x_rc,y,alpha)
}

