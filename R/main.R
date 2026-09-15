#main script for the Evaluation ####
#load all functions ####
source("functions/functions_emos.R")

#functions for calibration plotting
source("functions/pit_and_marg_cal_plot.R")
source("functions/quantil_cal_plot.R")

#functions for score decomp
source("functions/decomp_funs.R")
source("functions/qs_decomposition.R")
source("functions/crps_decomp_fun_future.R")

#load data ####
load("data/full")
load("data/verification_24") #these df already contain 
load("data/full_48")
load("data/verification_48")
load("data/adjacent_cells_4")
load("data/geo_info")

#config what to calculate ####
postprocess <- FALSE #this does the whole postprocessing for each forcast(takes time)
quant_cal <- TRUE  #this takes time as well
decomp <- FALSE #decomposition of the Scores for all forecasts
descriptives 


#Indexing for traingset, windowsize etc ####
n_cells <- 1748
start_i <- 32
positions <- start_i:61
window_size <- 30 #20 shorter window might be better
num_verification_days <- 61-start_i+1
last_day_index <- 61


# 24h - postproccessing and calibration plots ####
shift <- 1 
source("forecasts/ens_24.R")
source("forecasts/emos_glob_24.R")
source("forecasts/emos_lok_24.R")

# 48h - postprocessing  and calibration plots ####
shift <- 2
source("forecasts/ens_48.R")
source("forecasts/emos_glob_48.R")
source("forecasts/emos_lok_48.R")

# create and save the calibration plots together (marginal,probabilisticm quantil)
source("evaluation_results/calibration_plots_together.R")

#create all plots related to the CRPS (without decompositon) ####
source("evaluation_results/CRPS_table_and_plot.R")

#plot the 2 \cdot Quantilscore along all Quanitlelevels ####
source("evaluation_results/quantile_score_plot")


# Score decomposition (CRPS,QS(.25,.50,.75,.95,.05)) for all forecasts (this takes a lot of time) ####
if(decomp){source("forecasts/decomp.R")}
source("evaluation_results/decomp_plot_and_table") #creates and saves the decomosition results


#Plots for description of the Datasets ICON and HOSTRADA ####
source("data_description/icon_and_host_region.R") #Icon and HOSTRADA Region map
source("data_description/visu_dataset.R") #table for structure of the df
source("data_description/projection_diff_Host_Icon.R") #illustrate diffrent projections
source("data_description/plot_temp_nov.R") #Berlin temperature Observation (Hostrada) and forecasts (ICON) november 2025
source("data_description/berlin_topology.R") #Berlin map with forest, water and framland

