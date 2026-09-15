# Plot water, forest and Farmland for Berlin ####  

#1, load sf ####
brand_sf <- st_read("data/brandenburg/gis_osm_water_a_free_1.shp")
berlin_sf_land <- st_read("data/brandenburg/gis_osm_landuse_a_free_1.shp")

#2. crop to borders ####
bbox <- st_bbox(c(xmin = 13.000,ymin = 52.338,xmax = 13.761,ymax = 52.7),
  crs = st_crs(brand_sf)
)
brand_sf <- st_crop(brand_sf, bbox)
unique(brand_sf$fclass)
berlin_sf_land <- st_crop(berlin_sf_land, bbox)

#3. select features ####
keep <- c("water","reservoir","riverbank")
brand_sf <- brand_sf |> filter(fclass %in% keep)
brand_sf <- st_transform(brand_sf, crs=geo_info$raster_info$crs)
keep <- c("forest","farmland")
berlin_sf_land <- berlin_sf_land |> filter(fclass %in% keep)
berlin_sf_land <- st_transform(berlin_sf_land, crs=geo_info$raster_info$crs)

#4. load Berlin borders ####
berlin_sf <- st_read("data/bezirksgrenzen/bezirksgrenzen.shp")
berlin_sf <- berlin_sf$geometry
berlin_sf <- st_transform(berlin_sf, crs=geo_info$raster_info$crs)

#5. create map ####
berlin_geo <- ggplot() +
  geom_sf(data=berlin_sf_land,aes(fill=fclass),alpha=.55,color=NA)+
  geom_sf(data=brand_sf,aes(fill="water"),color="cyan2",linewidth=.6)+
  geom_sf(data = berlin_sf, fill = NA,color = "grey40",linewidth = .9)+
  scale_fill_manual(
    values = c(
      water = "cyan",
      forest = "green4",
      farmland = "orange"
    ),
    labels = c(
      "water" = "Gewässer",
      "farmland" = "Ackerland",
      "forest" = "Wald"
  ))+
  labs(fill="Flächentyp",x="Längengrad", y="Breitengrad")+
  theme_minimal(base_size = 14)+
  scale_x_continuous(breaks = breaksEast) +
  scale_y_continuous(breaks = breaksNorth) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey70", linewidth = 0.8, linetype = "dashed"),  # große Gitterlinien
  )
  
#6. save map ####
png("plots/berlin_geo_Rplot.png", width=1200, height=800,res = 150)
print(berlin_geo)
dev.off()

#cleanup
rm(berlin_sf,brand_sf,berlin_sf_land)