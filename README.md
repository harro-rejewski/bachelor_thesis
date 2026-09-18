# Bachelorarbeit

Dieses Repository enthält die Analyseskripte für meine Bachelorarbeit im Fach **Mathematik mit Informatik** an der **Universität Greifswald** mit dem Titel:

> **Statistische Evaluation und Vergleich probabilistischer Temperaturvorhersagen**

## Anwendung

Der zentrale Bestandteil des Repositories ist der Ordner `R`. Dieser enthält die Analyseskripte, die für die Auswertung und Erstellung der Ergebnisse der Bachelorarbeit verwendet wurden.
Die Daten sind über
```link
https://nextcloud.uni-greifswald.de/index.php/s/9fe4QgKxHQxcB9b
```
abrufbar. Für die Hauptanalyse muss der Ordner `data` in das Projektverzeichnis `R` abgelegt werden.

Über das Skript `main.R` können alle wichtigen Analyseskripte ausgeführt werden. Die berechneten Ergebnisse und Abbildungen werden im Ordner `Plots` abgelegt.

### Reproduzierbarkeit

Um die Ergebnisse zu reproduzieren, stehen zwei Möglichkeiten zur Verfügung:

* das bereitgestellte **Dockerfile** im Ordner `Docker`
* die entsprechende **`.lock`-Datei** im Ordner `Docker`
#### Docker

Das Docker-Image kann beispielsweise mit folgendem Befehl im Ordner Docker erstellt werden:

```bash
docker build --no-cache -t ba-rstudio:final .
```

Anschließend kann ein Container erstellt und das Repository eingebunden werden:

```bash
sudo docker run -d \
  --name container-name \
  -p 8787:8787 \
  -e PASSWORD=test \
  -v ~/path-of-this-repo:/home/rstudio/project \
  ba-rstudio:final
```

Der Container kann anschließend mit folgendem Befehl gestartet werden:

```bash
docker start container-name
```

Die RStudio Umgebung ist dann in erreichbar über:
```bash
http://localhost:8787/
```


## Sonstige Inhalte des Repositories

Finale PDF Version der Arbeit.

###  `Tex`
Latex Scripte zum Erstellen der Arbeit. 
(Zur Repoduktion der Inhalte des Ergebnisteils die Dateien aus dem Ordner `R/plots` in den Ordner
`Tex/figs` kopieren.)

### `CDO`

Im Ordner `CDO` befindet sich der CDO-Befehl zur Generierung einer Projektionsdatei für die verwendeten Gewichte. Die Dateien dafür sind ebenfalls über den Nextcloudordner zugänglich.

### `Pamore Manger`

Der Ordner `Pamore Manger` enthält Python-Skripte, mit denen sich **ICON-D2-EPS-Daten** über einen PAMORE-Downloadlink herunterladen und anschließend auf das Gitter der HOSTRADA projizieren lassen.

## Nicht enthaltene Daten

Die **ICON-D2-Modellvorhersagen** sowie die **HOSTRADA-Daten** sind aus Gründen der Dateigröße nicht Bestandteil dieses Repositories. Die Daten sind über
```link
https://nextcloud.uni-greifswald.de/index.php/s/9fe4QgKxHQxcB9b
```
einsehbar. 
