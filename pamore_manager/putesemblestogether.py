import config
import subprocess
from pathlib import Path



def merge_ensembles():
# create a dictionary with forcast_time as key and list of [filename,member] as value
    dic = dict()
    data_dir = config.CONVERTED_DATA_DIR
    for file in data_dir.iterdir():
        if file.is_file():
            print(file.stem)
            stem = file.stem
            name = file.name
            #das kann man auch einfacher machen anstatt splitten und zusammensetzen
            stem_split = stem.split("_")
            reftime = stem_split[1]
            forcast_time = stem_split[2]
            forcast_hour = stem_split[3]
            member = stem_split[4]
            #key is everything except member
            key = f"{stem_split[0]}_{stem_split[1]}_{stem_split[2]}_{stem_split[3]}.nc"
            dic.setdefault(key, []).append([name, member])
    
    #sort the list by member number
    for list in dic.values():
        list.sort(key=lambda x: int(x[1]))

    for key, list in dic.items():
        filenames = [config.CONVERTED_DATA_DIR/pair[0] for pair in list]
        output = config.CONCATINATED_DATA_DIR / key
        #stack files with cdo zip
        cmd = ["cdo", "-f", "nc4", "-z", "zip", "cat"] + filenames + [output]
        subprocess.run(cmd, check=True)
