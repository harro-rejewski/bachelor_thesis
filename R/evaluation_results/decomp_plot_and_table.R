#Table and Plot of the Score Decomposition ####
#uses functions from decomp_plot_funs.R
#needs the results from decomp.R
#1. create limits and isolines for the plots ####
xmin <- min(decomp_crps$mcb,decomp_q25$mcb,decomp_q50$mcb,decomp_q75$mcb,decomp_q95$mcb,decomp_q05$mcb)
xmax <- max(decomp_crps$mcb,decomp_q25$mcb,decomp_q50$mcb,decomp_q75$mcb,decomp_q95$mcb,decomp_q05$mcb)
ymin <- min(decomp_crps$dsc,decomp_q25$dsc,decomp_q50$dsc,decomp_q75$dsc,decomp_q95$dsc,decomp_q05$dsc)
ymax <- max(decomp_crps$dsc,decomp_q25$dsc,decomp_q50$dsc,decomp_q75$dsc,decomp_q95$dsc,decomp_q05$dsc)

score_best <- min(decomp_crps$crps,decomp_q25$qs,decomp_q50$qs,decomp_q75$qs,decomp_q95$qs,decomp_q05$qs)
score_worst <- max(decomp_crps$crps,decomp_q25$qs,decomp_q50$qs,decomp_q75$qs,decomp_q95$qs,decomp_q05$qs)

iso <- tibble(
    score_best = score_best,
    score_worst = score_worst,
  )

margin <- 0.3
step <- 0.1
lines <- 35
scores <- seq(iso$score_best-margin,iso$score_worst+margin, by=step)
c_ymin <- 0.5

unc_mean <- mean(c(unique(decomp_crps$unc), 
                   unique(decomp_q25$unc), 
                   unique(decomp_q50$unc),
                   unique(decomp_q75$unc),
                   unique(decomp_q05$unc),
                   unique(decomp_q95$unc)))
#2. create the decompositon plots ####
iso <- tibble(
  score = scores,
  unc = unique(decomp_crps$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )

diff_crps <- unc_mean - unique(decomp_crps$unc)
plot_crps <- plot_decomp(decomp_crps,iso,xmin,xmax,ymin-diff_crps+c_ymin,ymax-diff_crps-0.5)

diff_q25 <- unc_mean - unique(decomp_q25$unc)
iso <- tibble(
  score = scores,
  unc = unique(decomp_q25$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )
diff_q25 <- unc_mean - unique(decomp_q25$unc)
plot_q25 <- plot_decomp(decomp_q25,iso,xmin,xmax,ymin-diff_q25+c_ymin,ymax-diff_q25-0.5)

iso <- tibble(
  score = scores,
  unc = unique(decomp_q50$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )


diff_q50 <- unc_mean - unique(decomp_q50$unc)
plot_q50 <- plot_decomp(decomp_q50,iso,xmin,xmax,ymin-diff_q50+c_ymin,ymax-diff_q50-0.5)

iso <- tibble(
  score = scores,
  unc = unique(decomp_q75$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )

diff_q75 <- unc_mean - unique(decomp_q75$unc)
plot_q75 <-plot_decomp(decomp_q75,iso,xmin,xmax,ymin-diff_q75+c_ymin,ymax-diff_q75-0.5)


iso <- tibble(
  score = scores,
  unc = unique(decomp_q05$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )
diff_q05 <- unc_mean - unique(decomp_q05$unc)
plot_q05 <- plot_decomp(decomp_q05, iso, xmin, xmax, ymin - diff_q05+c_ymin, ymax-diff_q05-0.5)

iso <- tibble(
  score = scores,
  unc = unique(decomp_q95$unc)) %>%
  mutate(
    intercept = unc - score,
    slope = 1,
    label = score
  )
diff_q95 <- unc_mean - unique(decomp_q95$unc)
plot_q95 <- plot_decomp(decomp_q95, iso, xmin, xmax, ymin - diff_q95+c_ymin, ymax-diff_q95-0.5)


#3. save all decomposition plots together ####
png("plots/decomp_Rplot.png", width=1800, height=1500,res = 170)
print(
plot_crps + labs(subtitle="CRPS")+
  plot_q25 + labs(subtitle = expression("2QS (" * alpha == 0.25 * ")"))+
    plot_q50 + labs(subtitle = expression("2QS (" * alpha == 0.50 * ")"))+ 
    plot_q75 + labs(subtitle = expression("2QS (" * alpha == 0.75 * ")"))+
    plot_q05 + labs(subtitle = expression("2QS (" * alpha == 0.05 * ")"))+
    plot_q95 + labs(subtitle = expression("2QS (" * alpha == 0.95 * ")"))+
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
)
dev.off()
#clean plots
rm(plot_crps,plot_q25,plot_q50,plot_q75,plot_q05,plot_q95)



# Table for CRPS Information ####
#5. Put all decompositon data together ####
decomp_table_df <- bind_rows(crps = decomp_crps |> rename(qs = crps),
          q05  = decomp_q05,
          q25  = decomp_q25,
          q50  = decomp_q50,
          q75  = decomp_q75,
          q95  = decomp_q95,
          .id = "score"
)|> 
mutate(model = factor(
         model,
         levels = c("emp_ens", "emos_glob", "emos_lok")
       )) |> 
arrange(horizon,model,score) |> 
select(horizon,model,score,everything())

#6. create latex code for the tabel ####
make_decomp_table <- function(dat) {
  model_labels <- c(
    emp_ens   = "empirisch",
    emos_glob = "global",
    emos_lok  = "lokal"
  )
  score_labels <- c(
    crps = "CRPS",
    q05  = "$2\\QS_{0.05}$",
    q25  = "$2\\QS_{0.25}$",
    q50  = "$2\\QS_{0.50}$",
    q75  = "$2\\QS_{0.75}$",
    q95  = "$2\\QS_{0.95}$"
  )
  cat("\\begin{table}[!h]\n")
  cat("\\caption{Zerlegung der Scores sowie Skill.}\n")
  cat("\\centering\n")
  cat("\\newcolumntype{L}{>{\\raggedright\\arraybackslash}X}\n")
  cat("\\newcolumntype{R}{>{\\raggedleft\\arraybackslash}X}\n")
  cat("\\begin{tabularx}{0.95\\textwidth}{L L L R R R R R}\n")
  cat("\\toprule\n")
  cat("Horizont & Score & Vorhersage & Score & MCB & DSC & UNC & Skill\\\\\n")
  cat("\\midrule\n")
  horizons <- unique(dat$horizon)
  for (h in horizons) {
    d_h <- dat[dat$horizon == h, ]
    scores <- unique(d_h$score)
    first_score <- TRUE
    for (s in scores) {
      d_s <- d_h[d_h$score == s, ]
      score_label <- score_labels[as.character(s)]
      for (i in seq_len(nrow(d_s))) {
        model <- model_labels[as.character(d_s$model[i])]
        if (i == 1) {
          if (first_score) {
            cat(
              "\\multirow[t]{", nrow(d_h), "}{*}{", h, "\\,h}",
              " & \\multirow[t]{", nrow(d_s), "}{*}{", score_label, "}",
              sep = ""
            )
            first_score <- FALSE
          } else {
            cat(
              "& \\multirow[t]{", nrow(d_s), "}{*}{", score_label, "}",
              sep = ""
            )
          }
          cat(
            " & ", model,
            " & ", sprintf("%.2f", d_s$qs[i]),
            " & ", sprintf("%.2f", d_s$mcb[i]),
            " & ", sprintf("%.2f", d_s$dsc[i]),
            " & ", sprintf("%.2f", d_s$unc[i]),
            " & ", sprintf("%.2f", d_s$skill[i]),
            "\\\\\n",
            sep = ""
          )
        } else {
            cat(
            "&& ", model,
            " & ", sprintf("%.2f", d_s$qs[i]),
            " & ", sprintf("%.2f", d_s$mcb[i]),
            " & ", sprintf("%.2f", d_s$dsc[i]),
            " & ", sprintf("%.2f", d_s$unc[i]),
            " & ", sprintf("%.2f", d_s$skill[i]),
            "\\\\\n",
            sep = ""
          )
        }
      }
      if (s != tail(scores, 1)) {
        cat("\\addlinespace[0.4em]\n")
      }
    }
      if (h != tail(horizons, 1)) {
        cat("\\midrule\n")
    }
  }
  cat("\\bottomrule\n")
  cat("\\end{tabularx}\n")
  cat("\\label{tab:decomp}\n")
  cat("\\end{table}\n")
}

#save the table
sink("plots/decomp_table.tex")
make_decomp_table(decomp_table_df)
sink()


