# Analysis of DIYSCO2 Measurement Campaigns

## Setup:

You will need [R](https://www.r-project.org/). I'm currently running the R version specified below on MacOSX El Capitan 10.11.6:
```
R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)
```

Before your run the `AMT-analysis.R` script, you will have to install the `packages` which are listed in the first few lines of the code. Just uncomment them and run. 

*** 

## Input:

The data from the winter (2016-03-18) & summer (2015-05-28) measurement campaigns.

### Geodata for plots:

* Metro Vancouver Boundary:
* OSM roads: https://mapzen.com/data/metro-extracts/metro/vancouver_canada/ (IMPOSM)
* Transect: https://gist.github.com/joeyklee/c67a0bced086b683032353d78732dcf1
* Neighborhoods: http://data.vancouver.ca/datacatalogue/localAreaBoundary.htm
* Study Area mask: https://gist.github.com/joeyklee/55812ef95f9c43201fb39e27f63f3c8c

### Data processed from the `mapping.py` script

NOTE: the open access data are the processed data. If you want to run the analysis script here, you will need a few more data which are listed in the `mapping.py` readme (e.g. traffic count roads split by grid size, etc).

Points: The geojson points for each measurement campaign

* filtered-data-points-150528.geojson
* filtered-data-points-160318.geojson

Processed data grids: The processed grids for each measurement campaign and grid size (50m, 100m, 200m, and 400m)

* gridded_emissions_###.geojson

*** 

## Run the script
You can run the analysis using the `AMT-analysis.R` script. 

NOTE: The folderpaths will have the be updated for your system. Also, note you will need to input data for the script to give you what you would expect. 


***

## Output:
The main data outputs of of the project are open access and can be found [here]().


