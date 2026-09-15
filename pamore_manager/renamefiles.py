import subprocess
from pathlib import Path
import config
from datetime import datetime, timedelta
import re

# CDO-Befehl
def extract_reftime(file_path):
    cmd = ["cdo", "sinfo", str(file_path)]
    cdo_output = subprocess.run(cmd, capture_output=True, text=True)
    lines = cdo_output.stdout.splitlines()
    for line in lines:
        line = line.strip()
        if line.startswith("RefTime"):
            match = re.search(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}", line)
            dt = datetime.strptime(match.group(), "%Y-%m-%d %H:%M:%S")
            return dt
    return None

#extract ensemble member and forcast hour from filename
def extract_forcast_hour(file_path):
    numbers = re.findall(r"\d+", file_path.stem)[0]
    result = [int(numbers[i:i+2]) for i in range(0, 4, 2)]
    hour = result[0]*24+result[1]
    return hour

def extract_member(file_path):
    file_path_suffix = file_path.suffix
    member = re.findall(r"\d+", file_path_suffix)[0]
    member = int(member)
    return member

def create_output_filename(file_path):
    member = extract_member(file_path)
    forcast_hour = extract_forcast_hour(file_path)
    reftime_unform = extract_reftime(file_path)
    forcast_time = reftime_unform + timedelta(hours=forcast_hour)
    reftime_str = reftime_unform.strftime("%Y%m%d%H")
    forcast_time_str = forcast_time.strftime("%Y%m%d%H")
    return f"icond2_{reftime_str}_{forcast_time_str}_{forcast_hour:02d}_{member:02d}.nc"