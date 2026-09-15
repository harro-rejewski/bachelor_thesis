# ICON-D2-EPS Region ####
icon <- "data/latlon_icon.grib2"
icon_rast <- terra::rast(icon)
icon_rast <- icon_rast[[1]]

# borders
countries <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)
countries <- terra::vect(countries)
ext <- ext(icon_rast)

icon_region <- ggplot() +
  geom_spatraster(data = icon_rast,na.rm = TRUE) +
  geom_sf(
    data = countries,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  coord_sf(
    xlim = c(ext$xmin, ext$xmax),
    ylim = c(ext$ymin, ext$ymax),
    expand = FALSE
  ) +
  scale_fill_viridis(option = "turbo",direction=1,na.value = NA,alpha=0.75) +
  labs(x="Längengrad", y="Breitengrad", fill="[°C]")+
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(n.breaks = 6) +
  theme_minimal(base_size = 14)+
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.8, linetype = "dashed"))

# save the plot
png("plots/icon_region_Rplot.png", width=1000, height=700,res = 150)
print(icon_region)
dev.off()
rm(icon_region)


# HOSTRADA Region ####
hostrada_path <- "data/host_full_20251103_06.nc"
host_rast <- terra::rast(hostrada_path)
ext <- ext(host_rast)
countries <- ne_countries(scale = "medium",returnclass = "sf")
countries <- st_transform(countries,crs(host_rast))
hostrada_region <- ggplot() +
  geom_spatraster(data = host_rast)+
  scale_fill_viridis(option = "turbo",direction=1,na.value = NA,alpha=0.75)+
  geom_sf(
    data = countries,
    fill = NA,
    color = "black",
    linewidth = 0.2
  )+
  coord_sf(
    xlim = c(ext$xmin, ext$xmax),
    ylim = c(ext$ymin, ext$ymax),
    expand = FALSE
  )+
  labs(x="Längengrad", y="Breitengrad", fill="[°C]")+
  theme_minimal(base_size = 15)+
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.8, linetype = "dashed")
  )
  
#save the plot
png("plots/host_region_Rplot.png", width=700, height=700,res = 150)
print(hostrada_region)
dev.off()
rm(hostrada_region)