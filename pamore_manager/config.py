#from datetime import datetime, timezone,timedelta
from pathlib import Path

# Get the directory where this config.py file is located
BASE_DIR = Path(__file__).resolve().parent

#weights and grid description for the CDO remapping to the HOSTRADA LCC grid
LCC_WEIGHT_FILE = BASE_DIR.parent / "cdo" / "direct_weights.nc"
LCC_GRID_DESCRIPTION_FILE = BASE_DIR.parent / "cdo" / "LCC.txt"


#Here you can set the FTP and the folder for the forecast data.
HOST = "download.dwd.de"
REMOTE_DIR = "pub/Pamore/oflxd21.1ab47c5f4c0fceb289fc24ccbe9d605e000"
FORECAST_START = "20250929" #Folder name for the initial time of the forecast.


DATA_DIR = BASE_DIR.parent / "pamore_data" / FORECAST_START #raw data original name
CONVERTED_DATA_DIR = BASE_DIR.parent / "converted_data" / FORECAST_START #converted data to the HOSTRADA LCC grid will be stored here
CONCATINATED_DATA_DIR = BASE_DIR.parent / "concatinated_data" / FORECAST_START #Finally here the ensembles will be concatinated and stored in one file per lead time

# Create directories if they don't exist
for path in [DATA_DIR, CONVERTED_DATA_DIR, CONCATINATED_DATA_DIR]:
    path.mkdir(parents=True, exist_ok=True)


# file settings for the postprocessing
CONCATINATED_DATA =  BASE_DIR.parent / "concatinated_data"
LEAD_TIME_LIST = ["24","48"]#[f"{i:02d}" for i in range(0, 49, 6)] #vorerst 00, 06, 12,18, 24 #[f"{i:02d}" for i in range(0, 49)] #"00" too "48"
LIST_DIR = BASE_DIR.parent / "postprocess_list"
