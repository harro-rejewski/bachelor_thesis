library(scoringRules)
library(tidyverse)
library(patchwork)
library(latex2exp)

#CRPS QSot
# 1. Funktionen definieren
indicator <- function(x,cut_off=0) ifelse(x >= cut_off, 1, 0)
cdf_norm <- function(x,mu=0,sd=1) pnorm(x,mu,sd)

# 2. Datenframe erstellen (wir berechnen die Werte vorab)
x_vals <- seq(-2, 2, length.out = 1000)
df <- data.frame(
  x = x_vals,
  y_cdf = cdf_norm(x_vals),
  y_ind = indicator(x_vals),
  y_cdf2 = cdf_norm(x_vals,mu=0.25,sd=0.5),
  y_ind2 = indicator(x_vals,.25)
)


# 3. Plot mit geom_ribbon
df_long <- df %>%
  pivot_longer(
    cols = c(y_cdf, y_cdf2,y_ind2), 
    names_to = "forcast", 
    values_to = "y_cdf_val"
  ) |> mutate(crps=(y_cdf_val-y_ind)^2)

crps_vals <- tibble(
  forcast = unique(df_long$forcast),
  CRPS = c(
    crps_norm(0),
    crps_norm(0, mean = 0.25, sd = 0.5),
    crps_sample(0, .25)
  ),
  x = -1.9,
  y = 0.95 * max(df_long$crps)
)


#noch legende und so hübsch machen und crps einfügen
my_label <- as_labeller(c(
  y_cdf  = "Vorhersage: N(0,1)",
  y_cdf2 = "Vorhersage: N(0.25,0.5)",
  y_ind2 = "Punktvorhersage: 0.25"
))
p1 <- ggplot(df_long, aes(x = x)) +
  geom_line(aes(y = y_ind, color = "Beobachtung"), linewidth = 1,alpha=0.8) +
  geom_line(aes(y = y_cdf_val, color = "Vorhersage"), linewidth = 1,alpha=0.8) +
  geom_ribbon(aes(ymin = y_cdf_val, ymax = y_ind), 
              fill = "lightgray", 
              alpha = 0.5) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Vorhersage" = "cyan2",
      "Beobachtung" = "red3"
    )
  ) +
  facet_wrap(~forcast,labeller =my_label)+
  theme_bw(base_size = 15)+
  labs(y="Wert / Wahrscheinlichkeit", x="x",
       subtitle = "Verteilungsfunktion der Vorhersage und Beobachtung")+
  theme(legend.position = "bottom",    
        strip.background = element_blank(),
        strip.text = element_text(size=rel(.9)))
p2 <- ggplot(df_long, aes(x = x, y = crps)) +
  geom_area(fill = "lightgray", alpha = 0.5) +
  geom_line(color = "darkmagenta", linewidth = .8) +
geom_label(
  data = crps_vals,
  aes(x = x-0.5, y = y,   label = sprintf("CRPS = %.3f", CRPS)),
  inherit.aes = FALSE,
  hjust = 0,
  linewidth = 0.5,
  size=4.5,
  fill = "white",
  color = "black",
  label.size = 0.3
)+
  facet_wrap(~forcast)+
  labs(y="quadrierte Differenz",
       subtitle = "Integrand des CRPS")+
  theme_bw(base_size = 15) +
  theme(strip.text = element_blank(),
      legend.position = "bottom")

p1 / p2 
  


#Anschauliche Zerlegung des CRPS für verschiedene Vorhersagen.
#Oben wird die Vorhersageverteilung mit einer Beobachtung
#mit Wert 0
#verglichen. Die Beobachtung ist als 
#Indikatorfunktion dargestellt 
#Die graue Fläche markiert die Abweichung zwischen
#beiden Funktionen. Unten ist diese Abweichung für 
#jeden Wert von x quadriert dargestellt. 
#Das Integral über die quadrierte Differenz ergibt den CRPS. 
#Kleine lokale Abweichungen führen zu einer kleinen Fläche 
#während große den CRPS erhöhen.



# Quantil Score ####


# Beispieldaten
bound <- 2
QS <- function(residual, alpha) {
  if_else(
    residual >= 0,
    (1-alpha)*residual,
    alpha*(-1)*residual
  )
}

grid_df <- expand_grid(
  residual = seq(-1*bound, bound, l = 200),
  alpha = c(.25, .50, .75)
) |> 
  mutate(QS = QS(residual, alpha))

grid_df |> 
  ggplot() +
  geom_line(aes(x = residual, y = QS, color = as.factor(alpha), linetype = as.factor(alpha)),linewidth = 1) +
  labs(
    x = latex2exp::TeX(r"($x - y$)"),
    y = "Score",
    color = latex2exp::TeX(r"($\alpha$)"),
    linetype = latex2exp::TeX(r"($\alpha$)")
  ) +
  theme_bw(base_size = 15) +
  scale_color_viridis_d()+
  theme(legend.position = "bottom")

