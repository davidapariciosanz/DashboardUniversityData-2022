
# Dashboard UniversiData 2022

This project provides a dashboard visualization of data about university access of five Spanish universities 
through the years. This project has been carried out for UniversiDATA.        

It has two main folders, one contains a file that provides the code of converting the raw data to processed data and then convert them into the rds
found in their respective folder and the other contains the dashboard project.        

This project has been created with R, HTML, CSS, SCSS, JavaScript and YAML.

## Folder 1: data_processing
###  File structure

```
<project root_path>
├─data/                   - Raw data
├─rds/                    - Processed data
└─main.R/                 - Code to convert raw data into processed data
```

### Imports
```
data.table (≥ 1.14.2)
readxl (≥ 1.4.0)
dplyr (≥ 1.0.9)
geojsonio (≥ 0.9.4)
stringr (≥ 1.4.0)
readr (≥ 2.1.2)
```

## Folder 2: Dashboard
###  File structure

```
<project root_path>
├─modules/
│ └─<module ID>           - Module folder
│   ├─R                   - Module functions shared across UI and server
│   ├─module-ui.html      - Module HTML template
│   └─server.R            - Module-level server function
├─R/                      - Common functions shared across modules
├─rds/                    - Processed data
├─views/                  - Small snippets
├─www/                    - Static files: css, js, img, ...
├─index.html              - Template for homepage
├─modules.yaml            - Modules label, icon, badge, ...
└─server.R                - Root server function
```

### Imports
```
shiny (≥ 1.7.2)
shidashi (≥ 0.1.3)
ggplot2 (≥ 3.3.6)
cowplot (≥ 1.1.1)
ggiraph (≥ 0.8.3)
ggExtra (≥ 0.10.0)
plyr (≥ 1.8.7)
dplyr (≥ 1.0.9)
data.table (≥ 1.14.2)
plotly (≥ 4.10.0)
leaflet (≥ 2.1.1)
grDevices (≥ 4.2.1)
readr (≥ 2.1.2)
geojsonio (≥ 0.9.4)
utils (≥ 4.2.1)
digest (≥ 0.6.29)
fastmap (≥ 1.1.0)
formatR (≥ 1.12)
httr (≥ 1.4.4)
yaml (≥ 2.3.5)
jsonlite (≥ 1.8.0)
htmltools (≥ 0.5.3)
logger (≥ 0.2.2)
rstudioapi (≥ 0.14)
```

## Notes

The app is uploaded on shinyapps.io and there are many links in case the first one runs out of time.       
https://david-aparicio-sanz.shinyapps.io/Dashboard-UniversiDATA-2022/       
https://david-aparicio-sanz.shinyapps.io/Dashboard-UniversiDATA-2022-Copy/        

If you can't run the app or you have a problem, contact me: 
davidapariciosanz01@gmail.com
