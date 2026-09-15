import ftp_downloader
import changecrs
import putesemblestogether


ftp_downloader.download_and_unzip_parallel(workers=4)
changecrs.change_crs()
putesemblestogether.merge_ensembles()