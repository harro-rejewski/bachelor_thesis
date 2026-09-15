#table for the crps comparision, histogramm of the CRPS, CRPS over the time, and spartial CRPS ####
#1. table for the crps comparision ####
df_table <- bind_rows(
verification |> select(crps_emp,crps_emos_glob,crps_lok_emos) |> 
  pivot_longer(
    starts_with("crps"),
    names_to = "names",
    values_to = "value"
  ) |>
  mutate(names = fct_relevel(names, "crps_emp", "crps_emos_glob", "crps_lok_emos")) |> 
  group_by(names) |> 
  summarise(
    score =  mean(value),
    min  = min(value),
    max = max(value),
    q_25 = quantile(value,probs=.25,type=1),
    q_75 = quantile(value,probs=.75,type=1)
  ),
verification_48 |> select(crps_emp,crps_emos_glob,crps_lok_emos) |> 
  pivot_longer(
    starts_with("crps"),
    names_to = "names",
    values_to = "value"
  ) |> 
  mutate(names = fct_relevel(names, "crps_emp", "crps_emos_glob", "crps_lok_emos")) |> 
  group_by(names) |> 
  summarise(
    score =  mean(value),
    min = min(value),
    max = max(value),
    q_25 = quantile(value,probs=.25,type=1),
    q_75 = quantile(value,probs=.75,type=1)
  ) 
)|> 
 mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    horizon = c(rep("24 h", 3), rep("48 h", 3))
  )


make_crps_table <- function(df) {

  method_names <- c(
    crps_emp = "empirisch",
    crps_emos_glob = "global",
    crps_lok_emos = "lokal"
  )

  cat("\\begin{table}[!h]\n")
  cat("\\caption{CRPS der Vorhersagen.}\n")
  cat("\\centering\n")
  cat("\\newcolumntype{L}{>{\\raggedright\\arraybackslash}X}\n")
  cat("\\newcolumntype{R}{>{\\raggedleft\\arraybackslash}X}\n")
  cat("\\begin{tabularx}{0.9\\textwidth}{L L R R R R}\n")
  cat("\\toprule\n")
  cat("Horizont & Methode & CRPS & Min & Max & $q_{25}$--$q_{75}$\\\\\n")
  cat("\\midrule\n")

  for (h in unique(df$horizon)) {

    cat("\\addlinespace[0.3em]\n")
    cat("\\multicolumn{6}{l}{", h, "\\,h}\\\\\n", sep = "")

    d <- df[df$horizon == h, ]

    for (i in seq_len(nrow(d))) {

      methode <- method_names[as.character(d$names[i])]

      q_range <- paste0(
        sprintf("%.2f", d$q_25[i]),
        "--",
        sprintf("%.2f", d$q_75[i])
      )

      cat(
        "& ", methode,
        " & ", sprintf("%.2f", d$score[i]),
        " & ", sprintf("%.2f", d$min[i]),
        " & ", sprintf("%.2f", d$max[i]),
        " & ", q_range,
        "\\\\\n",
        sep = ""
      )
    }
  }

  cat("\\bottomrule\n")
  cat("\\end{tabularx}\n")
  cat("\\label{tab:crps}\n")
  cat("\\end{table}\n")
}

#save table 
sink("plots/crps_table.tex")
make_crps_table(df_table)
sink()


#2. create df for plotting the CRPS ####
df_crps <- bind_rows(
  verification |> select(crps_emp,crps_emos_glob,crps_lok_emos,cell_index,day_number) |> 
    pivot_longer(
      starts_with("crps"),
      names_to = "names",
      values_to = "value"
    ) |> 
    mutate(names = fct_relevel(names, "crps_emp", "crps_emos_glob", "crps_lok_emos"),
           horizon = 24),
  verification_48 |> select(crps_emp,crps_emos_glob,crps_lok_emos,cell_index,day_number) |> 
    pivot_longer(
      starts_with("crps"),
      names_to = "names",
      values_to = "value"
    ) |> 
    mutate(names = fct_relevel(names, "crps_emp", "crps_emos_glob", "crps_lok_emos"),
           horizon = 48)
)

#labels for plotting
method_labels <- c(
  "crps_emp" = "Empirische Vorhesage", 
  "crps_emos_glob" = "EMOS global", 
  "crps_lok_emos" = "EMOS lokal"
)

horizon_labels <- c(
  "24" = "Vorhersagehorizont 24h", 
  "48" = "Vorhersagehorizont 48h"
)

#3. histogramm of the CRPS for all forecasts ####
ggplot(df_crps, aes(x = value, fill = names)) + 
  geom_histogram(bins = 40, color = "white") + 
  facet_grid(horizon ~ names,labeller = labeller(names = method_labels,horizon=horizon_labels)) +
  scale_fill_manual(values = c("darkorange", "navy", "cyan3"))+
  theme_bw()+
  scale_x_continuous(limits=c(0,6))+
  theme(legend.position = "none")+
  labs(x="CRPS",y="Anzahl")


#4. save mean CRPS plot for the time domain ####
df_crps_day <- df_crps |> 
  group_by(day_number,horizon,names) |> 
  summarise(.groups = "drop_last",value=mean(value))

png("plots/crps_time.png", width=1700, height=1200, res=180)
print(
  ggplot(df_crps_day)+
    aes(x=day_number,y=value,color=names)+
    geom_point()+
    geom_line(linewidth = .5)+
    scale_x_continuous(breaks = seq(from = min(df_crps_day$day_number), 
                                    to = max(df_crps_day$day_number), 
                                    by = 1),
                       labels = as.character(1:30))+
    scale_y_continuous(breaks = seq(from=0, to = 2.5,by=0.5),limits = c(0, NA))+
    facet_wrap(~horizon,nrow=2,labeller = labeller(horizon=horizon_labels))+
    #facet_grid(rows = vars(horizon),labeller = labeller(horizon=horizon_labels))+
    scale_color_manual(
      name = "Vorhersage",
      labels = c("empirisch","global","lokal"),
      values=c("darkorange","navy","cyan3"))+
    labs(y="CRPS",x="Tag im November")+
    theme_bw(base_size = 15)+
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1.05))
    )
)
dev.off()

#  Spartial CRPS map ####
#5. create data for the plot
crps_rast_data <- verification %>%
  group_by(as.numeric(cell_index)) %>%
  summarise(mean_crps_emp = mean(crps_emp),
            mean_crps_emos_glob = mean(crps_emos_glob),
            mean_crps_emos_lok = mean(crps_lok_emos)
  )
crps_rast_data_48 <- verification_48 %>%
  group_by(as.numeric(cell_index)) %>%
  summarise(mean_crps_emp_48 = mean(crps_emp),
            mean_crps_emos_glob_48 = mean(crps_emos_glob),
            mean_crps_emos_lok_48 = mean(crps_lok_emos)
  )
crps_rast_data_all <- crps_rast_data |> inner_join(crps_rast_data_48,by="as.numeric(cell_index)")

#6. create the terra raster ####
raster_crps <- rast(nrows = geo_info$raster_info$dim[1],
                    ncols = geo_info$raster_info$dim[2],
                    crs = geo_info$raster_info$crs,
                    ext = geo_info$raster_info$extent,
                    nlyrs =6)

names(raster_crps) <- c("Empirische Vorhersage 24h","Empirische Vorhersage 48h",
                        "EMOS global 24h","EMOS global 48h",
                        "EMOS lokal 24h","EMOS lokal 48h")
# array in rast
values(raster_crps[[1]]) <- crps_rast_data_all$mean_crps_emp
values(raster_crps[[3]]) <- crps_rast_data_all$mean_crps_emos_glob
values(raster_crps[[5]]) <- crps_rast_data_all$mean_crps_emos_lok
values(raster_crps[[2]]) <- crps_rast_data_all$mean_crps_emp_48
values(raster_crps[[4]]) <- crps_rast_data_all$mean_crps_emos_glob_48
values(raster_crps[[6]]) <- crps_rast_data_all$mean_crps_emos_lok_48

#7. load berlin borders ####
berlin_sf <- st_read("data/bezirksgrenzen/bezirksgrenzen.shp")
berlin_sf <- berlin_sf$geometry
berlin_sf <- st_transform(berlin_sf, crs=geo_info$raster_info$crs)


#8. create and save CRPS map ####
mins <- terra::global(raster_crps, "min", na.rm = TRUE)
maxs <- terra::global(raster_crps, "max", na.rm = TRUE)
common_min <- min(mins[,1])-0.2 #bisschen aufhellen
common_max <- max(maxs[,1])
breaksEast <- seq(13.1, 13.8,.2)
breaksNorth <- seq(52.3,52.7,.1)

png("plots/crps_spat.png", width=1700, height=2000, res=150)
print(
ggplot() +
  geom_spatraster(data = raster_crps) +
  geom_sf(
    data = berlin_sf,
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  facet_wrap(~ lyr, ncol = 2) +
  scale_fill_viridis_c(option = "turbo",direction = 1, alpha =.9,
                       limits = c(common_min, common_max),oob = scales::squish
  ) +
  labs(
  x = "Längengrad",y = "Breitengrad",fill = "CRPS"
  )+
  theme_minimal(base_size = 18)+
  scale_x_continuous(breaks = breaksEast)+
  scale_y_continuous(breaks = breaksNorth)+
  theme(
    panel.grid.major = element_line(color = "grey50",linewidth = 0.8, linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey70",linewidth = 0.5,linetype = "dotted"),
    strip.text = element_text(size = rel(1.03))
  )
)
dev.off()

# clean up
rm(df_crps,df_crps_day,df_table,crps_rast_data,
   crps_rast_data_48,crps_rast_data_all,
   raster_crps)
