#Functions for the data generation Script create_dfs_from_rawData.R ####
get_icon_files <- function(base_dir) {
  files <- list.files(
    path = base_dir,
    pattern = "\\.nc$",
    recursive = TRUE,
    full.names = TRUE
  )
  files_24 <- files[grepl("_24\\.nc$", files)]
  files_48 <- files[grepl("_48\\.nc$", files)]
  list(
    icon_list_24  = files_24,
    icon_list_48  = files_48
  )
}

create_file_info_df <- function(file_path){
  splits <- strsplit(file_path, "/")
  splits <- sapply(splits, function(splits){splits[length(splits)]})
  info <- strsplit(splits, "_")
  info <- lapply(info, function(inf){x<-sub("\\.nc$", "", inf[4])
  c(ref_time=inf[2], forcast_time=inf[3], lead_time=x)})
  file_info_df <- berryFunctions::l2df(info, byrow = TRUE)
}


create_full_data <- function(icon_rasters,host_oct,host_nov,relevant_nlyr,
                                 forecast_date_vec_string,
                                 number_of_cells) {
  i <- 1
  #here maybe smarter solution for copying but for now it works and does not take too long
  full <- c()
  for (rast in icon_rasters) {
    ens_data <- values(rast) - 273.15
    date <- as.Date(time(rast)[1])
    month_num <- as.integer(format(date, "%m"))
    if (month_num == 10) {
      obs <- values(host_oct$raster[[relevant_nlyr[i]]])
    } else if (month_num == 11) {
      obs <- values(host_nov$raster[[relevant_nlyr[i]]])
    }
    data <- cbind(1:number_of_cells,i,forecast_date_vec_string[i],obs,ens_data)
    full <- rbind(full, data)
    i <- i + 1
  }
  full <- as.data.frame(full)
  names(full)[1] <- "cell_index"
  names(full)[2] <- "day_number"
  names(full)[3] <- "forcast_date"
  names(full)[4] <- "y"
  full <- full  %>% rename_with(~ sub("2t_height=2_", "ens_", .x),
                                starts_with("2t_height=2_"))
  #fix type for each variable
  full <- full %>%
    mutate(across(ens_1:ens_20, as.numeric),
           y = as.numeric(y),
           day_number = as.numeric(day_number),
           cell_index = as.numeric(cell_index))
  #add ens_mean and ens_sd
  full <- full %>% 
    mutate(ens_sd = apply(select(., ens_1:ens_20), 1, sd, na.rm = FALSE),
           ens_mean = rowMeans(select(., ens_1:ens_20), na.rm = FALSE),
           #ens_bc1 = rowMeans(select(., ens_1:ens_5), na.rm = FALSE),
           #ens_bc2 = rowMeans(select(., ens_6:ens_10), na.rm = FALSE),
           #ens_bc3 = rowMeans(select(., ens_11:ens_15), na.rm = FALSE),
           #ens_bc4 = rowMeans(select(., ens_16:ens_20), na.rm = FALSE)
           )
}

# function that corps and loads hostrada file####
load_hostrada <- function(hostrada_file, berlin_border_raster){
  hostrada <- terra::rast(hostrada_file)
  #crop to berlin area (das dauert etwas)
  hostrada <- terra::crop(hostrada, berlin_border_raster)
  names(hostrada) <- time(hostrada)
  res <- list()
  res[[1]]<- hostrada
  names(res) <- c("raster")
  res
}

