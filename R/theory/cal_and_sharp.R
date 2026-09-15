#Example for calibration and sharpness ####
x <- seq(4, 18, length.out = 1000)
scale <- 2

true <- 0.5 * dnorm(x, mean = 10, sd = 2) +
  0.5 * dnorm(x, mean = 7,  sd = 1)

F1 <- scale*( 0.5 * dnorm(x, mean = 10, sd = 2) +
                0.5 * dnorm(x, mean = 7,  sd = 1))

F2 <- scale*dnorm(x, mean = 12, sd = sqrt(1))

F3 <- scale*dnorm(x, mean = 12, sd = sqrt(6))


# Ridges df
df <- data.frame(
  x = rep(x, 3),
  y = rep(3:1, each = length(x)),
  F = rep(c("F3", "F2", "F1"), each = length(x)),
  true = rep(true, 3),
  forecast = c(F1, F2, F3)
) |> 
  mutate(
    F = factor(
      F,
      levels = c("F1", "F2", "F3")
    )
  )

levels(df$F)

a<-ggplot(df, aes(x = x, y = y, group = F)) +
  # Wahre Verteilung
  geom_ridgeline(
    aes(height = true,fill = "Wahre Verteilung Y"),
    color = "black",
    alpha = 0.7,
    scale = scale,
    linewidth = 0.4
  ) +
  geom_line(
    aes(
      y = y + forecast,
      color = "Vorhersage"
    ),
    
    linewidth = 1.2
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Wahre Verteilung Y" = "grey75")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Vorhersage" = "navy")
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c(
      expression(atop(paste(F[3], ": weder"), " scharf noch kalibriert")),
      expression(atop(paste(F[2], ": schärfer, "), "aber nicht kalibriert")),
      expression(atop(paste(F[1], ":"), "kalibriert"))
    )
  ) +
  labs(x = "Vorhersagegegenstand Y",y = "Vorhersage",
       subtitle = bquote(atop("Vorhersage " ~ F[1] ~ " bis " ~ F[3])) ) +
  theme_ridges() +
    theme(
      axis.text.y = element_text(
        size = rel(1.2),
        hjust = 0.5
      ),
      axis.title.x = element_text(
        size = rel(1.3),
        hjust = 0.5
      ),
      axis.title.y = element_text(
        size = rel(1.3),
        hjust = 0.5
      ),
      legend.position = "bottom",
      legend.justification = "center",
      plot.subtitle = element_text(
        size = rel(1.4),
        hjust = 0.5
  ),
  legend.text = element_text(size = rel(1.1))
  )

# true conditional dist
true_Z0 <- dnorm(x, mean = 10, sd = 2)
true_Z1 <- dnorm(x, mean = 7,  sd = 1)

# F4
F4_Z0 <- dnorm(x, mean = 10, sd = 2)
F4_Z1 <- dnorm(x, mean = 7,  sd = 1)

# data
df_F4 <- data.frame(
  x = rep(x, 2),
  y = rep(1:2, each = length(x)),
  Z = rep(c("Z = 0", "Z = 1"), each = length(x)),
  true = c(true_Z0, true_Z1),
  forecast = c(F4_Z0, F4_Z1)
)

b<-ggplot(df_F4, aes(x = x, y = y, group = Z)) +
  geom_ridgeline(aes(height = true,fill = "Wahre Verteilung Y | Z"),
                 color = "black",
                 alpha = 0.7,
                 scale = scale,
                 linewidth = 0.4
  ) +
  geom_line(
    aes(y = y + scale*forecast, color = "F4"),
    linewidth = 1.2
  )+
  scale_color_manual(
    name = NULL,values = c("F4" = "navy"),labels = expression(F[4])
  )+
  scale_fill_manual(
    name = NULL,values = c("Wahre Verteilung Y | Z" = "grey75")
  ) +
  scale_y_continuous(
    breaks = 1:2, labels = c(
      expression(atop(
        Z == 1
      )),
      expression(atop(
        Z == 0
      ))
    )
  ) +
  labs(
    x = "Vorhersagegegenstand Y | Z",
    y = "",
    subtitle = bquote(atop("Vorhersage " ~ F[4],"(kalibriert und scharf)" ))
  ) +
  theme_ridges() +
  theme(
    axis.text.y = element_text(
      size = rel(1.3),
      hjust = 0.5
    ),
    axis.title.x = element_text(
      size = rel(1.3),
      hjust = 0.5
    ),
    axis.title.y = element_text(
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.justification = "center",
    plot.subtitle = element_text(
      size = rel(1.4),
      hjust = 0.5
    ),
    legend.text = element_text(size = rel(1.1))
  )

a + b
