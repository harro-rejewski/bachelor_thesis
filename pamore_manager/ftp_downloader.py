from ftplib import FTP
from pathlib import Path
import config
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor
from queue import Queue
from ftplib import FTP
import gzip, io


def download_and_unzip():
    # establish FTP connection and get file list
    ftp = FTP(config.HOST)
    ftp.login()
    ftp.cwd(config.REMOTE_DIR)
    files = ftp.nlst()
    print(f"{len(files)} Dateien gefunden")
    print(ftp.retrlines('LIST'))
    # Download
    for filename in tqdm(files, desc="Downloading and extracting files"):
        local_gz_path = config.DATA_DIR / filename           # Original .gz
        local_unzipped_path = config.DATA_DIR / filename.replace(".gz", "")  # Entpackt

        # Datei herunterladen
        with local_gz_path.open("wb") as f:
            ftp.retrbinary(f"RETR {filename}", f.write)

        # Datei entpacken
        with gzip.open(local_gz_path, "rb") as gz_file:
            with local_unzipped_path.open("wb") as out_file:
                out_file.write(gz_file.read())

        local_gz_path.unlink()
    # close connection
    ftp.quit()



### optimieren mit threading
def ftp_worker(file_queue, worker_id, progress):
    "jeder worker erstellt seine eigene ftp verbindung und zieht aus der threadsafe queue (der download_and_unzip_parallel funktion) dateien zum downloaden"
    ftp = FTP(config.HOST)
    ftp.login()
    ftp.cwd(config.REMOTE_DIR)
    print(f"Worker {worker_id} connected to FTP server")

    while True:
        try:
            #warteschlange ist atomar, deshalb keine race conditions
            #print(f"Worker {worker_id} waiting for file...")
            filename = file_queue.get(block=False)
        except:
            print("die warteschlange ist leer. es gibt keine dateien mehr zu laden")
            break
        #print(f"Worker {worker_id} got file: {filename}")
        """
        if filename.isfile():
            print("geht hier irgend etwas")
            file_queue.task_done()
            progress.update(1)
            continue
        """
        if not filename.endswith(".gz"):
            file_queue.task_done()
            progress.update(1)
            continue

        try:
            buffer = io.BytesIO() # in-memory buffer also ich speichere die datei im ram anstatt auf der festplatte
            ftp.retrbinary(f"RETR {filename}", buffer.write) #buffer zeiger landet am ende der datei                 
            buffer.seek(0)  #bufferzeiger wieder auf anfang setzen
            #print(f"Worker {worker_id} downloaded {filename}, now extracting...")
        except Exception as e:
            print(f"Worker {worker_id} failed to download {filename}: {e}")
            file_queue.task_done()
            continue
        out_path = config.DATA_DIR / filename.replace(".gz", "")
        #sofort aus dem RAM entpacken
        with gzip.open(buffer, "rb") as gz, out_path.open("wb") as out:
            out.write(gz.read())
        #print(f"Worker finished processing {filename}")

        progress.update(1)
        file_queue.task_done()
    ftp.quit()

def download_and_unzip_parallel(workers=4):
    ftp = FTP(config.HOST)
    ftp.login()
    ftp.cwd(config.REMOTE_DIR)
    files = ftp.nlst()
    ftp.quit()
    print(f"{len(files)} Dateien gefunden")

    q = Queue()
    for f in files:
        q.put(f)
    progress = tqdm(total=len(files), desc="Downloading & extracting")
    #with = „mach etwas und räum danach sauber auf“ kontextmanager ohne with open und close und so selbst setzen
    with ThreadPoolExecutor(max_workers=workers) as executor:
        for i in range(workers): #_ ist konvention für ungenutzte variable
            print(f"Starte Worker {i}")
            executor.submit(ftp_worker, q,i,progress)
        #checkt ob alle aufgaben erledigt sind also für jeden get muss ein task done aufgerufen worden sein
        #d.h. das programm wartet hier bis alle dateien runtergeladen und entpackt sind sonst blockiert es hier
        q.join()
    progress.close()