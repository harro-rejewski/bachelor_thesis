FROM rocker/geospatial:4.6.1

# System-Abhängigkeiten
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libeccodes-dev \
        libeccodes-tools \
        git \
        curl && \
    rm -rf /var/lib/apt/lists/*

# ecCodes testen
RUN grib_ls -V

# gribr installieren
RUN R -e 'install.packages("remotes", repos="https://cloud.r-project.org")' && \
    R -e 'remotes::install_github("nawendt/gribr")'

# renv installieren
RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org")'

WORKDIR /home/rstudio/project

COPY renv.lock .

# Nur fehlende Pakete aus renv.lock installieren,
# und dabei exakt die Version aus dem Lockfile verwenden
# Nur fehlende Pakete aus renv.lock installieren
RUN R --vanilla -e '\
    lock <- renv::lockfile_read("renv.lock"); \
    pkgs <- lock$Packages; \
    installed <- rownames(installed.packages()); \
    missing <- setdiff(names(pkgs), installed); \
    cat("Fehlende Pakete:", paste(missing, collapse=", "), "\n"); \
    if (length(missing) > 0) { \
        for (p in missing) { \
            info <- pkgs[[p]]; \
            cat("Installiere", p, "Version", info$Version, "\n"); \
            if (!is.null(info$Source) && info$Source == "Repository") { \
                renv::install(paste0(p, "@", info$Version)); \
            } else { \
                renv::install(p); \
            } \
        } \
    } \
'
