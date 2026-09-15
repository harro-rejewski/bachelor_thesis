#Quantile_Error_Crurve ####
#1. calc quantile score for all forecasts ####
alpha_seq <- seq(from=.01,to=.999,by=0.01)
qs_plot_lok_24 <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(verification$y,fcast_quant_emos_lok_24$quantile(x),x))
})
qs_plot_lok_48 <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(verification$y,fcast_quant_emos_lok_48$quantile(x),x))
})

qs_plot_glob_24 <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(verification$y,fcast_quant_emos_glob_24$quantile(x),x))
})

qs_plot_glob_48 <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(verification$y,fcast_quant_emos_glob_48$quantile(x),x))
})

rq <- rowQuantiles(ens_data_24, probs = alpha_seq, type = 1)
qs_plot_ens_24 <- sapply(seq_along(alpha_seq), function(i) {
  alpha <- alpha_seq[i]
  2 * mean(qs_quantiles(
    verification$y,
    rq[, i],
    alpha
  ))
})

rq <- rowQuantiles(ens_data_48, probs = alpha_seq, type = 1)
qs_plot_ens_48 <- sapply(seq_along(alpha_seq), function(i) {
  alpha <- alpha_seq[i]
  2 * mean(qs_quantiles(
    verification$y,
    rq[, i],
    alpha
  ))
})

#2. prepare df for plotting ####
qs_plot <- list(
  "lokal 24"  = qs_plot_lok_24,
  "lokal 48"  = qs_plot_lok_48,
  "global 24" = qs_plot_glob_24,
  "global 48" = qs_plot_glob_48,
  "empirisch 24"    = qs_plot_ens_24,
  "empirisch 48"    = qs_plot_ens_48
) |>
  imap_dfr(\(score, methode)
           tibble(
             methode = methode,
             quantilniveau = alpha_seq,
             score = score
           )
  ) |>
  separate(
    methode,
    into = c("verfahren", "horizont"),
    sep = " (?=24|48$)"
  ) |> 
  mutate(
    verfahren = factor(verfahren, levels = c("empirisch","global","lokal"))
  )

#3. create and save the plot ####
png("plots/qs_error_curve_Rplot.png", width=1400, height=800,res = 175)
print(ggplot(qs_plot)+
  aes(x=quantilniveau,y=score,color=verfahren,
      linetype=horizont)+
  scale_color_manual(values = c("darkorange","navy","cyan3"))+
  geom_line(linewidth = 0.7)+
  theme_bw(base_size=16)+
  theme(
    legend.position = "bottom"
  )+
  labs(y="doppelter Quantilscore",x="Qauntilniveau",color="Vorhersage",linetype="Horizont"))
dev.off()  

#check if this really apporximates the crps 
qs_plot |> group_by(verfahren,horizont) |> summarise(mean=mean(score))

#clean up
rm(qs_plot,rq,
   qs_plot_lok_24,qs_plot_lok_48,
   qs_plot_glob_24,qs_plot_glob_48,
   qs_plot_ens_24,qs_plot_ens_48)



