#Data structure visualisation table ####
verification_visu_24 <- verification |>
  mutate(
    date = as.Date(forcast_date, format = "%Y%m%d%H")
  ) |> 
  mutate(day = dense_rank(forcast_date)) |> 
  select(
    all_of(c("forcast_date","day","cell_index", "y")),
    ens_1:ens_3, ens_20
  ) |> 
  mutate(across(c(y, starts_with("ens_")),function(x) sprintf("%.1f", x)))|> 
  slice(c(1:5, (n() - 4):n()))


#function for the latex table ####
make_dataset_table <- function(df) {
  cat("\\begin{table}[ht]\n")
  cat("    \\centering\n")
  cat("    \\fontsize{10.5}{12}\\selectfont\n")
  cat("    \\newcolumntype{C}{>{\\centering\\arraybackslash}X}\n")
  cat("    \\newcolumntype{Y}{>{\\centering\\arraybackslash}p{2cm}}\n")
  cat("    \\caption{Ausschnitt zum Aufbau des Verifikationsdatensatzes zum Vorhersagehorizont $24\\h$.}\n")
  cat("    \\label{tab:datensatz}\n")
  cat("    \\begin{tabularx}{\\textwidth}{Y C C C C C C C C}\n")
  cat("        \\toprule\n")
  cat(
    "        Datum & $t$ & $l$ & $y_{l}^{(t)}$ & ",
    "$x_{l,24\\h,1}^{(t)}$ & $x_{l,24\\h,2}^{(t)}$ & ",
    "$x_{l,24\\h,3}^{(t)}$ & $\\dots$ & ",
    "$x_{l,24\\h,20}^{(t)}$ \\\\\n",
    sep = ""
  )
  cat("        \\midrule\n")
  for (i in 1:5) {
    cat(
      "        ",
      df$forcast_date[i],
      " & ", df$day[i],
      " & ", df$cell_index[i],
      " & ", df$y[i],
      " & ", df$ens_1[i],
      " & ", df$ens_2[i],
      " & ", df$ens_3[i],
      " & $\\dots$",
      " & ", df$ens_20[i],
      " \\\\\n",
      sep = ""
    )
  }
  cat(
    "        $\\vdots$ & $\\vdots$ & $\\vdots$ & $\\vdots$ & ",
    "$\\vdots$ & $\\vdots$ & $\\vdots$ & $\\vdots$ & $\\vdots$ \\\\\n",
    sep = ""
  )
  
  for (i in 6:10) {
    cat(
      "        ",
      df$forcast_date[i],
      " & ", df$day[i],
      " & ", df$cell_index[i],
      " & ", df$y[i],
      " & ", df$ens_1[i],
      " & ", df$ens_2[i],
      " & ", df$ens_3[i],
      " & $\\dots$",
      " & ", df$ens_20[i],
      " \\\\\n",
      sep = ""
    )
  }
  cat("        \\bottomrule\n")
  cat("    \\end{tabularx}\n")
  cat("\\end{table}\n")
}

sink("plots/data_visu_table.tex")
make_dataset_table(verification_visu_24)
sink()
