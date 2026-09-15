#1. create and save HOSTRADA temperature November 2025 plots ####
#boxplot over for eval days
nov <- ggplot(verification, aes(x = as.factor(day_number), y = y)) +
  geom_boxplot(outlier.size = 0.2)+
  labs(x = "Tag im November", y="Temperatur [°C]") +
  scale_x_discrete( labels = as.character(1:30))+
  scale_y_continuous(breaks = seq(from=-6,to=14,by=3))+
  theme_bw(base_size = 15)

#Histogramm over all times
nov_marg <- ggplot() +
  geom_histogram(aes(x=verification$y,y=after_stat(density)),bins=30,fill="lightblue",color="black")+
  #labs(x="Temperatur [C]",y="Dichte",title = "HOSTRADA Temperatur in Berlin November 2025 0:00 UTC")+
  labs(x="Temperatur [°C]",y="Dichte")+
  theme_bw(base_size = 15)

#put together an save
png("plots/nov_desc.png", width=1700, height=1200, res=180)
print(nov / nov_marg +
        plot_annotation(tag_levels = 'a'))
dev.off()  

#2. Hostrada mean November spartial map ####
#create terra raster
nov_rast_data <- verification %>%
  group_by(cell_index) %>%
  summarise(mean_y = mean(y),
)
raster_nov <- rast(nrows = geo_info$raster_info$dim[1],
                    ncols = geo_info$raster_info$dim[2],
                    crs = geo_info$raster_info$crs,
                    ext = geo_info$raster_info$extent,
                    nlyrs = 1)
names(raster_nov) <- c("mean_temperature_november")
values(raster_nov[[1]]) <- nov_rast_data$mean_y

#limits for plots
mins <- terra::global(raster_nov, "min", na.rm = TRUE)
maxs <- terra::global(raster_nov, "max", na.rm = TRUE)
common_min <- min(mins[,1])
common_max <- max(maxs[,1])
breaksEast <- seq(13.1, 13.8,.2)
breaksNorth <- seq(52.3,52.7,.1)

#plot
nov_spat <-ggplot() +
  geom_spatraster(data = raster_nov) +
  geom_sf(data = berlin_sf, fill = NA, color = "black", linewidth = 1) +
  scale_fill_viridis(option = "turbo", direction = 1, limits = c(common_min, common_max), oob = scales::squish,alpha = 0.9)+
  theme_minimal(base_size = 14)+
  labs(x="Längengrad", y="Breitengrad", fill="[°C]") + 
  scale_x_continuous(breaks = breaksEast) +
  scale_y_continuous(breaks = breaksNorth) +
  theme(
    panel.grid.major = element_line(color = "grey50", linewidth = 0.8, linetype = "dashed"), 
    panel.grid.minor = element_line(color = "grey70", linewidth = 0.5, linetype = "dotted"),  
)

#save
png("plots/nov_spat_Rplot.png", width=1200, height=600,res = 150)
print(nov_spat)
dev.off()

#3. Regridded Icon mean November spartial map ####
#create data for the raster
icon_spartial_24_data <- verification %>%
  group_by(cell_index) %>%
  summarise(ens_mean = mean(ens_mean),
  )
icon_spartial_48_data <- verification_48 %>%
  group_by(cell_index) %>%
  summarise(ens_mean = mean(ens_mean),
  )
#create raster
raster_ens_nov <- rast(nrows = geo_info$raster_info$dim[1],
                   ncols = geo_info$raster_info$dim[2],
                   crs = geo_info$raster_info$crs,
                   ext = geo_info$raster_info$extent,
                   nlyrs = 2)

values(raster_ens_nov[[1]]) <- icon_spartial_24_data$ens_mean
values(raster_ens_nov[[2]]) <- icon_spartial_48_data$ens_mean
names(raster_ens_nov) <- c("ICON-D2-EPS 24h", "ICON-D2-EPS 48h")

#create plot (same common, min and max as HOSTRADA map)
nov_spat_icon <-ggplot() +
  geom_spatraster(data = raster_ens_nov) +
  geom_sf(data = berlin_sf, fill = NA, color = "black", linewidth = 1) +
  facet_wrap(~ lyr, ncol = 2)+
  scale_fill_viridis(option = "turbo", direction = 1, limits = c(common_min, common_max), oob = scales::squish,alpha = 0.9)+
  theme_minimal(base_size = 15)+
  labs(x="Längengrad", y="Breitengrad", fill="[°C]") + 
  scale_x_continuous(breaks = breaksEast) +
  scale_y_continuous(breaks = breaksNorth) +
  theme(
    panel.grid.major = element_line(color = "grey50", linewidth = 0.8, linetype = "dashed"),  # große Gitterlinien
    panel.grid.minor = element_line(color = "grey70", linewidth = 0.5, linetype = "dotted"),   # kleine Gitterlinien
  )

#save plot
png("plots/mean_icon_spat_Rplot.png", width=1400, height=700,res = 150)
print(nov_spat_icon)
dev.off()


#4. Icon Ensemble over November 2025 #### 
verification_visu <- bind_rows(verification |>
                                 mutate(horizon = "Vorhersagehorizont 24h") |>
                                 select(
                                   all_of(c("forcast_date","cell_index", "horizon", "y")),
                                   ens_1:ens_20
                                 ),
                               verification_48 |>
                                 mutate(horizon = "Vorhersagehorizont 48h") |>
                                 select(
                                   all_of(c("forcast_date","cell_index", "horizon", "y")),
                                   ens_1:ens_20
                                 )
) |> mutate(horizon=as.factor(horizon))

verification_visu <- verification_visu |> 
  mutate(
    date = as.Date(forcast_date, format = "%Y%m%d%H")
  ) |> 
  mutate(day = dense_rank(forcast_date)) |> 
  select(
    all_of(c("forcast_date","day","horizon","cell_index", "y")),
    ens_1:ens_20
  )

verification_visu <- verification_visu |> pivot_longer(
  cols = ens_1:ens_20,
  names_to = "ensemble",
  values_to = "value"
) |> 
  mutate(
    ensemble = factor(ensemble,levels = paste0("ens_", 1:20))
  )

verification_visu <- verification_visu |> 
  group_by(forcast_date, horizon, ensemble, day) |>
  summarise(
    value = mean(value, na.rm = TRUE),
    y = mean(y, na.rm = TRUE),
    .groups = "drop"
  )

icon_time <- ggplot(verification_visu)+
  aes(x=day,color = ensemble,y=value)+
  #facet_grid(rows = vars(horizon))+
  facet_wrap(~horizon,nrow=2)+
  geom_line(linewidth=0.3)+
  geom_line(aes(y=y),linetype = "dashed",linewidth = 1.5,color="darkgreen")+
  scale_y_continuous(breaks = seq(round(min(verification_visu$value)),
                                  round(max(verification_visu$value)),by=2)
  )+
  scale_x_continuous(
    breaks = 1:30,
    limits = c(1, 30)
  )+
  scale_color_viridis_d(
    option = "plasma",
    labels = c(1:20)
  )+
  labs(y="Temperatur [°C]",x="Tag im November",color="Ensemblemitglied")+
  theme_bw(base_size = 15)+
  guides(color = guide_legend(nrow = 2))+
  theme(strip.background = element_blank(),
        legend.position = "bottom")

png("plots/icon_time.png", width=1700, height=1200, res=180)
print(icon_time)
dev.off()  

#clean up
rm(icon_time,verification_visu)
rm(nov_spat_icon,icon_spartial_24_data,icon_spartial_48_data,raster_ens_nov)
rm(nov_rast_data,raster_nov,nov_spat)
rm(nov,nov_marg)