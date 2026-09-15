#Example for Score decompositon and quantile error curve ####
set.seed(200)
n <- 3000


# data generation ####
mu <- rnorm(n, 0, 1)
Y <- rnorm(n, mean = mu, sd = 1)
t <- 2
tau <- sample(c(-t, t), n, replace = TRUE)

# unfoc vorhersage
F_mix <- function(y, mu, tau) {
  0.5 * pnorm(y, mean = mu, sd = 1) +
    0.5 * pnorm(y, mean = mu + tau, sd = 1)
}

quantile_mix <- function(alpha, mu,tau) {
  f <- function(x) F_mix(x, mu, tau) - alpha
  uniroot(f, lower = mu - 6, upper = mu + 6)$root
}

#unfocussed forecast
fcast_unfoc <- list(
  quantile = function(alpha) {
    mapply(quantile_mix, alpha = alpha, mu = mu, tau = tau)
  }
)
#marginal forecast
fcast_mg <- list(
  quantile = function(alpha) {
    qt <- qnorm(p=alpha,mean=0,sd=sqrt(2))
    rep(qt,n)
  }
)
#informed forecast
fcast_inf <- list(
  quantile = function(alpha) {
    qnorm(alpha,mean=mu,sd=1)
  }
)


# CRPS over OS Plot ####
alpha_seq <- seq(from=.01,to=.999,by=0.02)
qs_plot_unfoc <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(Y,fcast_unfoc$quantile(x),x))
})
qs_plot_inf <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(Y,fcast_inf$quantile(x),x))
})

qs_plot_marg <- sapply(alpha_seq,FUN=function(x){
  2*mean(qs_quantiles(Y,fcast_mg$quantile(x),x))
})

qs_plot <- list(
  "unfokussiert"  = qs_plot_unfoc,
  "ideal"  = qs_plot_inf,
  "marginal" = qs_plot_marg
) |>
  imap_dfr(\(score, vorhersage)
           tibble(
             vorhersage = vorhersage,
             quantilniveau = alpha_seq,
             score = score
           )
  ) |> 
  mutate(
    verfahren = factor(vorhersage, levels = c("ideal","marginal","unfokussiert"))
  )

#800x500
ggplot(qs_plot)+
  aes(x=quantilniveau,y=score,color=verfahren)+
  scale_color_viridis_d(alpha=.8)+
  geom_line(linewidth = 0.7)+
  theme_bw(base_size=15)+
  theme(
    legend.position = "bottom"
  )+
  labs(y ="doppelter Quantilscore",x="Qauntilniveau",color="Vorhersage")




#CRPS Decomposition Plot ####

#informed forecast
res_inf <- crps_qs_decomp(fcast_inf,Y,n=30) #eigentlich müsste mcb 0 sein
#marginal forecast
res_mg <- crps_qs_decomp(fcast_mg,Y,n=30)
#unfocussed forecast
res_unfoc <-  crps_qs_decomp(fcast_unfoc,Y,n=30)

decomp_df <- rbind(res_mg$components,res_inf$components,res_unfoc$components) %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>% 
  mutate(.,model = str_remove(model, "^res_"),
         model = case_when(model==1 ~ "marginal",
                           model==2 ~ "ideal",
                           model==3 ~ "unfokussiert"))
scores <- c(res_mg$mean_scores["crps"],res_inf$mean_scores["crps"], res_unfoc$mean_scores["crps"])
margin <- 0.7
step <- 0.05
scores <- round(seq(max(scores)-margin,min(scores)+margin, by=step),2)

#plotting ####
iso <- tibble(
  score = scores,
  unc = unique(decomp_df$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )

ggplot(data = decomp_df) +
  geom_abline(
    data = iso, aes(intercept = intercept, slope = slope), color = "lightgray", alpha = 0.5,
    size = 0.8
  ) +
  geom_labelabline(
    data = iso, aes(intercept = intercept, slope = slope, label = label), color = "gray50",
    hjust = 0.85, size = 11 * 0.36, text_only = TRUE, boxcolour = NA, straight = TRUE
  ) +
  geom_point(aes(x = mcb, y = dsc, color = model), size = 3) +
  geom_text_repel(aes(x = mcb, y = dsc, label = model),
                  max.overlaps = NA, size = 11.5 * 0.40, nudge_x = 0, nudge_y=0.01,
                  direction = "both", segment.color = "transparent", box.padding = 0.25, force = 1, point.padding = 0.75,
                  seed = 4
  ) +
  labs(x="MCB",y="DSC")+
  annotate(
    "label",
    x = Inf,
    y = Inf,
    label = paste0("UNC = ", round(mean(decomp_df$unc),3)),
    hjust = 1,
    vjust = 1
  )+
  scale_color_viridis_d()+
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1,
    legend.position = "none"
  )
