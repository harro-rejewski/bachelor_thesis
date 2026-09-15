#Plot Berlin to show diffrences between native ICON Grid and HOSTRADA Grid ####

#1. Load Berlin Borders ####
berlin_sf <- st_read("data/bezirksgrenzen/bezirksgrenzen.shp")
berlin_sf <- berlin_sf$geometry

#2. Load HOSTRADA data
nc_file <- "data/host_20251126_00.nc"
hostrada <-  terra::rast(nc_file)
n_valid <- sum(!is.na(values(hostrada)))
values(hostrada)
berlin_sf <- st_transform(berlin_sf, crs=crs(hostrada))
n_crop <- sum(!is.na(values(hostrada)))

#3. Set Borders for Plotting ####
common_min <- -1.5
common_max <- 1.5
breaksEast <- seq(13.1, 13.8,.2)
breaksNorth <- seq(52.3,52.7,.1)

#4. hostrada berlin plot ####
p_host <- ggplot() +
  geom_spatraster(data = hostrada) +
  geom_sf(data = berlin_sf,
          fill = NA,
          color = "black",
          linewidth = 1) +
  theme_minimal()+
  scale_fill_viridis(
    option = "turbo",
    direction = 1,
    limits = c(common_min, common_max),
    oob = scales::squish,
    alpha = 0.9
  )+
  scale_x_continuous(breaks = breaksEast) + 
  scale_y_continuous(breaks = breaksNorth) + 
  theme_minimal(base_size = 14)+
  labs(subtitle="HOSTRADA",x="Längengrad", y="Breitengrad", fill="[°C]") + 
  theme(
    panel.grid.major = element_line(color = "grey50", linewidth = 0.8, linetype = "dashed"),  # große Gitterlinien
    panel.grid.minor = element_line(color = "grey70", linewidth = 0.5, linetype = "dotted"),   # kleine Gitterlinien
  )



#ICON Plot ####
#this code is largely copied from the ICON Tutorial 2025: Working with the ICON Model
#5. Load Icon Grid description ####
grid <- "../cdo/icon_grid_0047_R19B07_L.nc"  #cdo sinfo you can check that it is the forecast from 26.11.2025 00.00
data <- "data/icon-d2-24h-forecast-26-11-25-00-ensemble-1"
ncHandle <- open.nc(grid)  # function to convert coordinates in degress 
rad2deg <- function(rad) {(rad * 180) / (pi)}  # get longitudes and latitudes of triangle vertices of a cell 
vlon <- rad2deg(var.get.nc(ncHandle,"clon_vertices")) 
vlat <- rad2deg(var.get.nc(ncHandle,"clat_vertices"))  
close.nc(ncHandle)
#6. Load icon data ####
gribHandle <- grib_open(data)  # select the grib record based on a given list of keys 
gribRecord <- grib_select(gribHandle, list(shortName = "2t"))
grib_close(gribHandle)  # create a data table with data to plot - ids and values are tripled 
DT <- data.table(lon = as.vector(vlon), 
                 lat = as.vector(vlat), 
                 id = rep(1:(dim(vlon)[2]) , each=3), 
                 var = rep(gribRecord$values, each=3))

#6. domain and edges for the plot ####
#select xrange und yrange
xrange <- c(13.089, 13.761) 
yrange <- c(52.338, 52.675) 
usedIDs <- unique(DT[lon%between%xrange & lat%between%yrange]$id) 
DT <- DT[id %in% usedIDs]  
# ... when the 3 corners on opposite sides of the date line move one corner 
IDsR = DT[,list( (max(lon)-min(lon))>200 & mean(lon)>0.0 ), by=id][V1==T]$id 
IDsL = DT[,list( (max(lon)-min(lon))>200 & mean(lon)<0.0 ), by=id][V1==T]$id 
DT[id %in% IDsR & lon < 0.0, lon := lon + 360.0] 
DT[id %in% IDsL & lon > 0.0, lon := lon - 360.0] 
# ... copy triangles by 360deg to fill holes near date line 
DTT <- DT[id%in%IDsR] 
DTT[lon > 0.0, lon := lon - 360.0]
DTT$id <- DTT$id + max(DT$id) 
DT <- rbind(DT, DTT) 
DTT <- DT[id%in%IDsL] 
DTT[lon < 0.0, lon := lon + 360.0] 
DTT$id <- DTT$id + max(DT$id) 
DT <- rbind(DT, DTT)
#to celsius
DT$var = DT$var - 273.15
berlin_sf <- st_transform(berlin_sf, crs="EPSG:4326")
st_crs(berlin_sf)

#7. create icon plot ####
p_icon <- ggplot() + 
  geom_polygon(data = DT, aes(x = lon, y = lat, group = id, fill = var)) + 
  geom_sf(data = berlin_sf,
          fill = NA,
          color = "black",
          linewidth = 1)+
  scale_fill_viridis(
    option = "turbo",
    direction = 1,
    limits = c(common_min, common_max),
    oob = scales::squish,
    alpha=0.9
  )+
  scale_x_continuous(breaks = breaksEast) + 
  scale_y_continuous(breaks = breaksNorth) + 
  theme_minimal(base_size = 14)+
  labs(subtitle = "ICON-D2",
       x="Längengrad", y="Breitengrad", fill="[°C]") + 
  theme(
    panel.grid.major = element_line(color = "grey50", size = 0.8, linetype = "dashed"),  # große Gitterlinien
    panel.grid.minor = element_line(color = "grey70", size = 0.5, linetype = "dotted"),   # kleine Gitterlinien
  )

combined_plot <- p_icon + p_host + 
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'a')

#8. save the plot ####
png("plots/proj_diff_Rplot.png", width=1400, height=700,res = 150)
print(combined_plot)
dev.off()

#clean up
rm(combined_plot,p_icon,p_host)
rm(hostrada,hostrada_file,n_crop,n_valid)
rm(vlat,vlon)
rm(ncHandle,gribHandle,gribRecord,DT,DTT)
