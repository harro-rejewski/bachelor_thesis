# Bachelorarbeit

Dieses Repository enthält die Analyseskripte für meine Bachelorarbeit im Fach **Mathematik mit Informatik** an der **Universität Greifswald** mit dem Titel:

> **Evaluation und Vergleich probabilistischer Temperaturvorhersagen**

## Anwendung

Der zentrale Bestandteil des Repositories ist der Ordner `R`. Dieser enthält die Daten und Analyseskripte, die für die Auswertung und Erstellung der Ergebnisse der Bachelorarbeit verwendet wurden.

Über das Skript `main.R` können alle wichtigen Analyseskripte ausgeführt werden. Die berechneten Ergebnisse und Abbildungen werden im Ordner `Plots` abgelegt.

### Reproduzierbarkeit

Um die Ergebnisse zu reproduzieren, stehen zwei Möglichkeiten zur Verfügung:

* das bereitgestellte **Docker-Image** im Ordner `Docker`
* die entsprechende **`.lock`-Datei**, über die die verwendeten R-Paketversionen nachvollzogen und reproduziert werden können

#### Docker

Das Docker-Image kann beispielsweise mit folgendem Befehl erstellt werden:

```bash
docker build --no-cache -t geospatial-gribr .
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

## Sonstige Inhalte des Repositories

### `CDO`

Im Ordner `CDO` befinden sich:

* die Gitterdefinitionen des **ICON-D2** und der **HOSTRADA**
* ein CDO-Befehl zur Generierung einer Projektionsdatei für die verwendeten Gewichte

### `Pamore Manger`

Der Ordner `Pamore Manger` enthält Python-Skripte, mit denen sich **ICON-D2-EPS-Daten** über einen PAMORE-Downloadlink herunterladen und anschließend auf das Gitter der HOSTRADA projizieren lassen.

## Nicht enthaltene Daten

Die rohen **ICON-D2-Modellvorhersagen** sowie die **HOSTRADA-Daten** sind aus Gründen der Dateigröße nicht Bestandteil dieses Repositories.

Die entsprechenden Daten können auf Anfrage zur Verfügung gestellt werden.
