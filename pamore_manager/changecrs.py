import config
import renamefiles
import subprocess
from pathlib import Path
from tqdm import tqdm

def change_crs():
    data_dir = Path(config.DATA_DIR)
    for file in tqdm(data_dir.iterdir(), desc="Changing CRS and renaming files"):
        if file.is_file():
            input_file_path = file
            output_file_name = renamefiles.create_output_filename(input_file_path)
            output_file_path = config.CONVERTED_DATA_DIR / output_file_name
        cmd = [
            "cdo",
            "-f", "nc4",
            "-z", "zip",
            f'remap,{config.LCC_GRID_DESCRIPTION_FILE},{config.LCC_WEIGHT_FILE}',
            input_file_path,
            output_file_path
        ]
        subprocess.run(cmd, check=True)
