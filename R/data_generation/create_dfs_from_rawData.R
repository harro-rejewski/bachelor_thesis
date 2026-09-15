#If you have the raw concatinated data and raw Hostrada data
#then you can create the dfs in the data folder with this script
#to get this raw data you have to contact me

#0. Load Helper Functions
source("data_generation/functions_df_creation.R")

#1. create crop raster for berlin ####
#ranges for berlin latitude longitude
xrange <- c(13.089, 13.761) 
yrange <- c(52.338, 52.675) 
#crop raster for berlin
berlin_border_raster <- ext(
  project(
    set.crs(as.polygons(ext(c(xrange,yrange))), "EPSG:4326"),
    "EPSG:3034"
  )
)

# 2. Load croped Hostrada data for oct and nov ####
##Read Hostrada Oktober ####
hostrada_file <- "../hostrada_data/tas_1hr_HOSTRADA-v1-0_BE_gn_2025100100-2025103123.nc"
host_oct <- load_hostrada(hostrada_file,berlin_border_raster)
host_oct$raster

##Read Hostrada November ####
hostrada_file <- "../hostrada_data/tas_1hr_HOSTRADA-v1-0_BE_gn_2025110100-2025113023.nc"
host_nov <- load_hostrada(hostrada_file,berlin_border_raster)
host_nov$raster

host_nov$raster
 
#3. Load Berlin borders for plotting ####
berlin_sf <- st_read("data/bezirksgrenzen/bezirksgrenzen.shp")
berlin_sf <- berlin_sf$geometry
berlin_sf <- st_transform(berlin_sf, crs=crs(host_oct$raster))

#3. Save number of cells ####
n_cells <- ncell(host_oct$raster)

# 4. Read in the file list for the icon data ####
folder_path <- "../concatinated_data"
file_paths_leadtimes <- get_icon_files(folder_path)

#5. Load ICON-data for Lead time 24h ####
#extract 24h paths
lead_24 <- file_paths_leadtimes$icon_list_24 
file_info_df <- create_file_info_df(lead_24)

#get all the relevant layers in the hostrada file ####
forcast_date_vec_string <- file_info_df$forcast_time
forcast_date_vec_formated <- ymd_h(forcast_date_vec_string)
relevant_nlyr_oct <- which(time(host_oct$raster) %in% forcast_date_vec_formated)
relevant_nlyr_nov <- which(time(host_nov$raster) %in% forcast_date_vec_formated)
relevant_nlyr <- c(relevant_nlyr_oct,relevant_nlyr_nov)

# Read in the relevant rasters ####
#das dauert halt ech nen weilchen read with croping
icon_rasters <- lapply(lead_24, function(r){crop(rast(r), berlin_border_raster)})
names(icon_rasters) <- as.character(file_info_df$forcast_time)

# put everything together
full <- create_full_data(icon_rasters,
                         host_oct,
                         host_nov,
                         relevant_nlyr,
                         forcast_date_vec_string,
                         n_cells)
#6. Load ICON-data for Lead time 48h ####
# get path to rasters
lead_48 <- file_paths_leadtimes$icon_list_48 
file_info_df_48 <- create_file_info_df(lead_48)
 
# get all the relevant layers in the hostrada file ####
forcast_date_vec_string_48 <- file_info_df_48$forcast_time
forcast_date_vec_formated <- ymd_h(forcast_date_vec_string_48)
relevant_nlyr_oct <- which(time(host_oct$raster) %in% forcast_date_vec_formated)
relevant_nlyr_nov <- which(time(host_nov$raster) %in% forcast_date_vec_formated)
relevant_nlyr <- c(relevant_nlyr_oct,relevant_nlyr_nov)

# Read in the relevant rasters ####
icon_rasters_48 <- lapply(lead_48, function(r){crop(rast(r), berlin_border_raster)})
names(icon_rasters) <- as.character(file_info_df$forcast_time)

# change the day indet to align with full_24
full_48 <- create_full_data(icon_rasters_48,
                            host_oct,
                            host_nov,
                            relevant_nlyr,
                            forcast_date_vec_string,
                            n_cells)

#7. create the verification df (October is for training, November for verification ) ####
start_i <- grep("2025110100",forcast_date_vec_string)
num_verification_days <- length(forcast_date_vec_string)-start_i+1 #30
verification <- full %>% filter(day_number >= start_i)
verification_48 <- full_48 %>% filter(day_number >= start_i)

#8. create matrix for the ajacent cells ####
#rook adjacency
adjacent_cells <- terra::adjacent(host_nov$raster,1:1748,directions=4,pairs=FALSE,include=TRUE)


#9. HOSTRADA Raster Information ####
raster_info <- list(
  extent = as.vector(ext(host_nov$raster)), 
  crs    = as.character(crs(host_nov$raster)), 
  res    = as.vector(res(host_nov$raster)), 
  dim    = c(nrow(host_nov$raster), ncol(host_nov$raster))
)
geo_info <- list("raster_info" = raster_info)


#10. save the data ####
save(full,file="data/full")
save(full_48,file="data/full_48")

save(verification,file="data/verification_24")
save(verification_48,file="data/verification_48")
save(adjacent_cells,file="data/adjacent_cells_4")
save(geo_info, file="data/geo_info")

#11. Extract HOSTRADA Layer for the comparision plot ####
target_time <- ymd_h("2025112600")
layer_idx <- which(time(host_nov$raster) == target_time)
layer <- host_nov$raster[[layer_idx]]

terra::writeCDF(
  layer,
  "data/host_20251126_00.nc",
  overwrite = TRUE
)

#12. Hostrada data for HOSTRADA display ####
host_path <- "../hostrada_data/tas_1hr_HOSTRADA-v1-0_BE_gn_2025110100-2025113023.nc"
host_rast <- terra::rast(host_path)
target_time <- ymd_h("2025110306")
layer_idx <- which(time(host_rast) == target_time)
host_rast <- host_rast[[layer_idx]]
time(host_rast)
terra::writeCDF(
  host_rast,
  "data/host_full_20251103_06.nc",
  overwrite = TRUE
)


#13. clean up ####
rm(host_oct)
rm(host_nov)
rm(get_icon_files,create_file_info_df,create_full_data,load_hostrada) #functions
rm(target_time,layer_idx,layer)
rm(lead_24,lead_48,host_rast,folder_path, host_path,relevant_nlyr,relevant_nlyr_nov,relevant_nlyr_oct,forcast_date_vec_string_48,forcast_date_vec_string,forcast_date_vec_formated,icon_rasters,icon_rasters_48)