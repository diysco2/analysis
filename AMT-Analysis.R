# install.packages('GISTools')
# install.packages('rgdal')
# install.packages('scales')
# install.packages('RColorBrewer')
# install.packages('classInt')
# install.packages('ggplot2')
# install.packages('plyr')



# ------- Import libraries -------$
library(GISTools)
library(rgdal)
# install.packages('scales')
require(scales)
# install.packages("RColorBrewer")
library(RColorBrewer)
# install.packages('classInt')
require(classInt) # Jenks natural breaks

# Plotting using ggplot2
require(ggplot2)
require(plyr)
# require(ggmap)

# install.packages("ggsn")
# require (ggsn)


# ----------- setup ---------------# 
# Set the Working Directory
setwd('/Users/leejoey/Dropbox/_Projects/ubc-micromet/DIYSCO2-main')

# set projection for proj
projection_utm10n = CRS("+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m
                        +no_defs")
projection_wgs84 = CRS("+init=epsg:4326")

# --------- map layers ------------- #
# Metrovan
metrovan = readOGR(dsn=paste(getwd(), "/diysco2-db/_main_/yvr-metro-van/",sep=""),layer="metroVan")
metrovan = spTransform(metrovan, projection_utm10n)
# metrovan = spTransform(metrovan, projection_utm10n)
# Roads
roads = readOGR(dsn=paste(getwd(), "/diysco2-db/_main_/OSM-roads/",sep=""), layer="osm-roads-main-metro-bbox")
roads = spTransform(roads, projection_utm10n)
# roads = spTransform(roads, projection_wgs84)
# prepare data for ggplot

# Transect
transect = readOGR(dsn=paste(getwd(), "/diysco2-db/_main_/study-area/",sep=""), layer="transect_epicc2sp_woss")
transect = spTransform(transect, projection_utm10n)
# Neighborhoods
hoods = readOGR(dsn=paste(getwd(), "/diysco2-db/_main_/yvr-open-data-neighborhoods/",sep=""),layer="csg_neighborhood_areas_grouped-crop-transect")
hoods = spTransform(hoods, projection_utm10n)
# Filter mask
mask = readOGR(dsn=paste(getwd(), "/diysco2-db/_main_/study-area/", sep=""), layer="transect_edge_mask")
mask = spTransform(mask, projection_utm10n)
# mask = spTransform(mask, projection_wgs84)


# ---------------------------- functions ------------------- #
source(paste(getwd(),"/analysis/AMT-Analysis-Helpers.R", sep=""))

roads_gg = makeSpatialGG(roads)
transect_gg= makeSpatialGG(transect)
hoods_gg= makeSpatialGG(hoods)
metrovan_gg= makeSpatialGG(metrovan)





# Read data into list
createDataList = function(experiment){
  outputList =  list(
    "date" = experiment,
    "points" = NULL,
    
    "data_50m" = NULL,
    "data_100m" = NULL,
    "data_200m" = NULL,
    "data_400m" = NULL,
    
    "data_50m_all" = NULL,
    "data_100m_all" = NULL,
    "data_200m_all" = NULL,
    "data_400m_all" = NULL,
    
    "data_50m_within" = NULL,
    "data_100m_within" = NULL,
    "data_200m_within" = NULL,
    "data_400m_within" = NULL,
    
    "data_50m_within_nona" = NULL,
    "data_100m_within_nona" = NULL,
    "data_200m_within_nona" = NULL,
    "data_400m_within_nona" = NULL,
    
    "data_epicc_50m" = NULL,
    "data_epicc_100m" = NULL,
    "data_epicc_200m" = NULL,
    "data_epicc_400m" = NULL,
    
    "data_epicc_log_50m" = NULL,
    "data_epicc_log_100m" = NULL,
    "data_epicc_log_200m" = NULL,
    "data_epicc_log_400m" = NULL,
    
    "data_epicc_log_50m_gt0" = NULL,
    "data_epicc_log_100m_gt0" = NULL,
    "data_epicc_log_200m_gt0" = NULL,
    "data_epicc_log_400m_gt0" = NULL,
    
    "data_50m_within_gg" = NULL,
    "data_100m_within_gg" = NULL,
    "data_200m_within_gg" = NULL,
    "data_400m_within_gg" = NULL,
    
    "data_epicc_50m_gg" = NULL,
    "data_epicc_100m_gg" = NULL,
    "data_epicc_200m_gg" = NULL,
    "data_epicc_400m_gg" = NULL
  )

  
  # read in points:

  fn = paste(getwd(), "/diysco2-db/campaigns/",experiment,"/diysco2-filtered-points/all_20150528.geojson",sep="")
  outputList$points = readOGR(dsn=fn,'OGRGeoJSON')
  
  # --------- Read in Data ----------- #
  fpath = paste("/diysco2-db/campaigns/", experiment, "/diysco2-grid/gridded_emissions_dev_", sep="")
  fn = paste(getwd(),fpath,50,'m.geojson', sep="")
  # all data & epicc subset
  outputList$data_50m = readOGR(fn, 'OGRGeoJSON')
  proj4string(outputList$data_50m) = projection_utm10n
#   outputList$data_50m = spTransform(outputList$data_50m, projection_wgs84)
  
  fn = paste(getwd(),fpath,100,'m.geojson', sep="")
  outputList$data_100m = readOGR(fn, 'OGRGeoJSON')
  proj4string(outputList$data_100m) = projection_utm10n
#   outputList$data_100m = spTransform(outputList$data_100m, projection_wgs84)
  
  fn = paste(getwd(),fpath,200,'m.geojson', sep="")
  outputList$data_200m = readOGR(fn, 'OGRGeoJSON')
  proj4string(outputList$data_200m) = projection_utm10n
#   outputList$data_200m = spTransform(outputList$data_200m, projection_wgs84)
  
  fn = paste(getwd(),fpath,400,'m.geojson', sep="")
  outputList$data_400m = readOGR(fn, 'OGRGeoJSON')
  proj4string(outputList$data_400m) = projection_utm10n
#   outputList$data_400m = spTransform(outputList$data_400m, projection_wgs84)
  
  # Convert factors to numeric
  outputList$data_50m = factorToNumericExcept(outputList$data_50m)
  outputList$data_100m = factorToNumericExcept(outputList$data_100m)
  outputList$data_200m = factorToNumericExcept(outputList$data_200m)
  outputList$data_400m = factorToNumericExcept(outputList$data_400m)
  
  # Make building, taffic, total, and measured emissions ha-1
  outputList$data_50m = makePerHa(outputList$data_50m, 50)
  outputList$data_100m = makePerHa(outputList$data_100m, 100)
  outputList$data_200m = makePerHa(outputList$data_200m, 200)
  outputList$data_400m = makePerHa(outputList$data_400m, 400)
  
  outputList$data_50m = fixDiffs(outputList$data_50m)
  outputList$data_100m = fixDiffs(outputList$data_100m)
  outputList$data_200m = fixDiffs(outputList$data_200m)
  outputList$data_400m = fixDiffs(outputList$data_400m)
  
  outputList$data_50m_all = outputList$data_50m
  outputList$data_100m_all = outputList$data_100m
  outputList$data_200m_all = outputList$data_200m
  outputList$data_400m_all = outputList$data_400m
  
  # Get only the cells within the study area
  outputList$data_50m_within = removeEdgeCells(outputList$data_50m, mask)
  outputList$data_100m_within = removeEdgeCells(outputList$data_100m, mask)
  outputList$data_200m_within = removeEdgeCells(outputList$data_200m, mask)
  outputList$data_400m_within = removeEdgeCells(outputList$data_400m, mask)
  
  outputList$data_50m_within_nona = subsetNaCells(outputList$data_50m_within)
  outputList$data_100m_within_nona = subsetNaCells(outputList$data_100m_within)
  outputList$data_200m_within_nona = subsetNaCells(outputList$data_200m_within)
  outputList$data_400m_within_nona = subsetNaCells(outputList$data_400m_within)
  
  # Subset out data that is not NA
  outputList$data_epicc_50m = subset(outputList$data_50m_within_nona, outputList$data_50m_within_nona@data$bco2e_may >=0  )
  outputList$data_epicc_100m = subset(outputList$data_100m_within_nona, outputList$data_100m_within_nona@data$bco2e_may >=0  )
  outputList$data_epicc_200m = subset(outputList$data_200m_within_nona, outputList$data_200m_within_nona@data$bco2e_may >=0  )
  outputList$data_epicc_400m = subset(outputList$data_400m_within_nona, outputList$data_400m_within_nona@data$bco2e_may >=0  )
  
  # filter data for comparing
  outputList$data_epicc_log_50m = subsetCellsForValidAnalysis(outputList$data_epicc_50m, 50)
  outputList$data_epicc_log_100m = subsetCellsForValidAnalysis(outputList$data_epicc_100m, 100)
  outputList$data_epicc_log_200m = subsetCellsForValidAnalysis(outputList$data_epicc_200m, 200)
  outputList$data_epicc_log_400m =subsetCellsForValidAnalysis(outputList$data_epicc_400m, 400)
  
  outputList$data_epicc_log_50m_gt0 = subsetForCorrelation(outputList$data_epicc_log_50m)
  outputList$data_epicc_log_100m_gt0 = subsetForCorrelation(outputList$data_epicc_log_100m) 
  outputList$data_epicc_log_200m_gt0 =  subsetForCorrelation(outputList$data_epicc_log_200m)
  outputList$data_epicc_log_400m_gt0 =  subsetForCorrelation(outputList$data_epicc_log_400m)
  
  # prepare data for ggplot
  makeSpatialGG = function(idata){
    idata@data$id = rownames(idata@data)
    idata_points = fortify(idata, region="id")
    
    idata_output = join(idata_points, idata@data, by="id")
    
    return(idata_output)
  }
  outputList$data_50m_within_gg = makeSpatialGG(outputList$data_50m_within)
  outputList$data_100m_within_gg = makeSpatialGG(outputList$data_100m_within) 
  outputList$data_200m_within_gg =  makeSpatialGG(outputList$data_200m_within)
  outputList$data_400m_within_gg =  makeSpatialGG(outputList$data_400m_within)
  
  outputList$data_epicc_50m_gg = makeSpatialGG(outputList$data_epicc_50m)
  outputList$data_epicc_100m_gg = makeSpatialGG(outputList$data_epicc_100m) 
  outputList$data_epicc_200m_gg =  makeSpatialGG(outputList$data_epicc_200m)
  outputList$data_epicc_400m_gg =  makeSpatialGG(outputList$data_epicc_400m)
  
  
  print("Output list complete")
  return(outputList)
} 

experiment_150528 = createDataList(150528)
experiment_160318  = createDataList(160318)



# onlyRelevantEmissions(experiment_150528$data_50m_within, "gridded-data-50m-utm10n-150528.geojson")
# onlyRelevantEmissions(experiment_150528$data_100m_within, "gridded-data-100m-utm10n-150528.geojson")
# onlyRelevantEmissions(experiment_150528$data_200m_within, "gridded-data-200m-utm10n-150528.geojson")
# onlyRelevantEmissions(experiment_150528$data_400m_within, "gridded-data-400m-utm10n-150528.geojson")
# 
# onlyRelevantEmissions(experiment_160318$data_50m_within, "gridded-data-50m-utm10n-160318.geojson")
# onlyRelevantEmissions(experiment_160318$data_100m_within, "gridded-data-100m-utm10n-160318.geojson")
# onlyRelevantEmissions(experiment_160318$data_200m_within, "gridded-data-200m-utm10n-160318.geojson")
# onlyRelevantEmissions(experiment_160318$data_400m_within, "gridded-data-400m-utm10n-160318.geojson")



tableFromSummary = function(ilist){
  for(i in 1:length(ilist)){
    
    output = paste(
      ilist[i][[1]]$stats[1],
      ilist[i][[1]]$stats[3],
      ilist[i][[1]]$stats[4],
      ilist[i][[1]]$stats[6],
      sep="   &   ")
    print(output)
  }
  
}

# get back stats
listStatistics = function(inputList){
  
  outputStats = list(
    
    points_summary = NULL,
    
    absolute_differences_measured_vs_inventory = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    rmse_report = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    skew_stats = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_co2_ppm_within = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_measured_emissions_within = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_building_emissions_within = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_traffic_emissions_within = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_total_emissions_within = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_co2_ppm_epicc = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_measured_emissions_epicc = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_building_emissions_epicc = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_traffic_emissions_epicc = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    summary_stats_total_emissions_epicc = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    
    relative_differences = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
      ),
  
    magnitude_differences_buildings = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
      ),
    magnitude_differences_traffic = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
    magnitude_differences_total = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
    ),
  
    correlation_coeffs = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
      ),
    
    mean_absolute_error = list(
      "data_50m" = NULL,
      "data_100m" = NULL,
      "data_200m" = NULL,
      "data_400m" = NULL
      )
    
    
  ) # end of outputStats list
  
  # points_summary:
  outputStats$points_summary = getSummary(inputList$points@data$co2)
  
  # absolute_differences_measured_vs_inventory
  absDiffStats = function(data_epicc_log){
    output = c(
      paste("", round(nrow(subset(data_epicc_log@data, data_epicc_log@data$absdiff >0) ) / nrow(data_epicc_log@data), 4)*100.0, " % of the \n measured emissions \n are greater than \nthe inventory", sep=""),
      paste( round(nrow(subset(data_epicc_log@data, data_epicc_log@data$absdiff <=10 & data_epicc_log@data$absdiff >= -10) ) / nrow(data_epicc_log@data), 4)*100.0, " %  of the\n measured emissions \n are within +/- 10 ppm of the \n inventory", sep=""),
      paste( round(nrow(subset(data_epicc_log@data, data_epicc_log@data$absdiff <=20 & data_epicc_log@data$absdiff >= -20) ) / nrow(data_epicc_log@data), 4)*100.0, " % of the \n measured emissions \n are within +/- 20 ppm of the \n inventory", sep="")
    )
    return(output)
  }
  outputStats$absolute_differences_measured_vs_inventory$data_50m = absDiffStats(inputList$data_epicc_50m)
  outputStats$absolute_differences_measured_vs_inventory$data_100m = absDiffStats(inputList$data_epicc_100m)
  outputStats$absolute_differences_measured_vs_inventory$data_200m =absDiffStats(inputList$data_epicc_200m)
  outputStats$absolute_differences_measured_vs_inventory$data_400m =absDiffStats(inputList$data_epicc_400m)
  
  # rmse_report
  getRMSE = function(data_epicc_log, var1, var2){
    val_diffs = data_epicc_log@data[[var1]] - data_epicc_log@data[[var2]]
    val_diffs_sq = val_diffs ^ 2
    val_diffs_rmse = sqrt(mean(val_diffs ^ 2)) # kg co2 ha hr
    print(val_diffs_rmse)
    
    return(val_diffs_rmse)
  }
  outputStats$rmse_report$data_50m = getRMSE(inputList$data_epicc_50m, "co2_avg_e", "bt_co2e")
  outputStats$rmse_report$data_100m = getRMSE(inputList$data_epicc_100m, "co2_avg_e", "bt_co2e")
  outputStats$rmse_report$data_200m =getRMSE(inputList$data_epicc_200m, "co2_avg_e", "bt_co2e")
  outputStats$rmse_report$data_400m =getRMSE(inputList$data_epicc_400m, "co2_avg_e", "bt_co2e")
  
  # skew_stats
  getSkew = function(data_within){
    output = c(
      paste((nrow(subset(data_within@data, data_within@data$co2_skew >0))/ nrow(data_within@data))*100, "% are greater than 0", sep=" "),
      paste((nrow(subset(data_within@data, data_within@data$co2_skew <0))/ nrow(data_within@data))*100, "% are less than 0", sep=" "),
      paste((nrow(subset(data_within@data, data_within@data$co2_skew ==0))/ nrow(data_within@data))*100, "% are equal to 0", sep=" "),
      paste((nrow(subset(data_within@data, is.na(data_within@data$co2_skew)))/ nrow(data_within@data))*100, "% are NAs", sep=" ")
      )
    return(output)
  }
  outputStats$skew_stats$data_50m = getSkew(inputList$data_50m_within)
  outputStats$skew_stats$data_100m = getSkew(inputList$data_100m_within)
  outputStats$skew_stats$data_200m = getSkew(inputList$data_200m_within)
  outputStats$skew_stats$data_400m = getSkew(inputList$data_400m_within)
  
  # summary_stats_co2_ppm_within 
  outputStats$summary_stats_co2_ppm_within$data_50m = getSummary(inputList$data_50m_within@data$co2_avg)
  outputStats$summary_stats_co2_ppm_within$data_100m = getSummary(inputList$data_100m_within@data$co2_avg)
  outputStats$summary_stats_co2_ppm_within$data_200m = getSummary(inputList$data_200m_within@data$co2_avg)
  outputStats$summary_stats_co2_ppm_within$data_400m = getSummary(inputList$data_400m_within@data$co2_avg)
  # summary_stats_measured_emissions_within
  outputStats$summary_stats_measured_emissions_within$data_50m = getSummary(inputList$data_50m_within@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_within$data_100m = getSummary(inputList$data_100m_within@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_within$data_200m = getSummary(inputList$data_200m_within@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_within$data_400m = getSummary(inputList$data_400m_within@data$co2_avg_e)
  # summary_stats_building_emissions_within 
  outputStats$summary_stats_building_emissions_within$data_50m = getSummary(inputList$data_50m_within@data$bco2e_may)
  outputStats$summary_stats_building_emissions_within$data_100m = getSummary(inputList$data_100m_within@data$bco2e_may)
  outputStats$summary_stats_building_emissions_within$data_200m = getSummary(inputList$data_200m_within@data$bco2e_may)
  outputStats$summary_stats_building_emissions_within$data_400m = getSummary(inputList$data_400m_within@data$bco2e_may)
  # summary_stats_traffic_emissions_within
  outputStats$summary_stats_traffic_emissions_within$data_50m = getSummary(inputList$data_50m_within@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_within$data_100m = getSummary(inputList$data_100m_within@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_within$data_200m = getSummary(inputList$data_200m_within@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_within$data_400m = getSummary(inputList$data_400m_within@data$e_10_14_hr)
  # summary_stats_total_emissions_within 
  outputStats$summary_stats_total_emissions_within$data_50m = getSummary(inputList$data_50m_within@data$bt_co2e)
  outputStats$summary_stats_total_emissions_within$data_100m = getSummary(inputList$data_100m_within@data$bt_co2e)
  outputStats$summary_stats_total_emissions_within$data_200m = getSummary(inputList$data_200m_within@data$bt_co2e)
  outputStats$summary_stats_total_emissions_within$data_400m = getSummary(inputList$data_400m_within@data$bt_co2e)
  
  # summary_stats_co2_ppm_epicc 
  outputStats$summary_stats_co2_ppm_epicc$data_50m = getSummary(inputList$data_epicc_50m@data$co2_avg)
  outputStats$summary_stats_co2_ppm_epicc$data_100m = getSummary(inputList$data_epicc_100m@data$co2_avg)
  outputStats$summary_stats_co2_ppm_epicc$data_200m = getSummary(inputList$data_epicc_200m@data$co2_avg)
  outputStats$summary_stats_co2_ppm_epicc$data_400m = getSummary(inputList$data_epicc_400m@data$co2_avg)
  # summary_stats_measured_emissions_epicc
  outputStats$summary_stats_measured_emissions_epicc$data_50m = getSummary(inputList$data_epicc_50m@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_epicc$data_100m = getSummary(inputList$data_epicc_100m@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_epicc$data_200m = getSummary(inputList$data_epicc_200m@data$co2_avg_e)
  outputStats$summary_stats_measured_emissions_epicc$data_400m = getSummary(inputList$data_epicc_400m@data$co2_avg_e)
  # summary_stats_building_emissions_epicc 
  outputStats$summary_stats_building_emissions_epicc$data_50m = getSummary(inputList$data_epicc_50m@data$bco2e_may)
  outputStats$summary_stats_building_emissions_epicc$data_100m = getSummary(inputList$data_epicc_100m@data$bco2e_may)
  outputStats$summary_stats_building_emissions_epicc$data_200m = getSummary(inputList$data_epicc_200m@data$bco2e_may)
  outputStats$summary_stats_building_emissions_epicc$data_400m = getSummary(inputList$data_epicc_400m@data$bco2e_may)
  # summary_stats_traffic_emissions_epicc
  outputStats$summary_stats_traffic_emissions_epicc$data_50m = getSummary(inputList$data_epicc_50m@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_epicc$data_100m = getSummary(inputList$data_epicc_100m@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_epicc$data_200m = getSummary(inputList$data_epicc_200m@data$e_10_14_hr)
  outputStats$summary_stats_traffic_emissions_epicc$data_400m = getSummary(inputList$data_epicc_400m@data$e_10_14_hr)
  # summary_stats_total_emissions_epicc 
  outputStats$summary_stats_total_emissions_epicc$data_50m = getSummary(inputList$data_epicc_50m@data$bt_co2e)
  outputStats$summary_stats_total_emissions_epicc$data_100m = getSummary(inputList$data_epicc_100m@data$bt_co2e)
  outputStats$summary_stats_total_emissions_epicc$data_200m = getSummary(inputList$data_epicc_200m@data$bt_co2e)
  outputStats$summary_stats_total_emissions_epicc$data_400m = getSummary(inputList$data_epicc_400m@data$bt_co2e)
  
  # relative_differences 
  examinRelativeDifferences = function(idata, gs){
    df = idata@data$reldiff
    output = c(
      paste("grid size: ", gs, "m", 
            (length(subset(df, df <= 0.5 & df >= -0.5)) / length(df))*100.0, "% of the data have a relative difference that is within a factor of 2","****",
            (length(subset(df, df <= 1 & df >= -1)) / length(df))*100.0, "% of the data have a relative difference that is within a factor of 10","****"
      )      
    )
    return(output)
  }
  outputStats$relative_differences$data_50m = examinRelativeDifferences(inputList$data_epicc_log_50m, 50)
  outputStats$relative_differences$data_100m = examinRelativeDifferences(inputList$data_epicc_log_100m, 100)
  outputStats$relative_differences$data_200m = examinRelativeDifferences(inputList$data_epicc_log_200m, 200)
  outputStats$relative_differences$data_400m = examinRelativeDifferences(inputList$data_epicc_log_400m, 400)
  
  # magnitude_differences_buildings 
  examineMagnitudes = function(idata, evar, gs){
    df = idata@data
    if(evar=="bco2e_may"){
      df = subset(df, df[[evar]] >1)
    }else if(evar=="e_10_14_hr"){
      df = subset(df, df[[evar]] >1)
    } else if (evar=="bt_co2e"){
      df = subset(df, df[[evar]] >2)
    }
    
    df=(log10(df$co2_avg_e) - log10(df[[evar]]))
    
    output = c(
      paste(
        "grid size: ", gs, "m", 
        (length(subset(df, df <= 0.5 & df >= -0.5)) / length(df))*100.0, " of measured emissions are within a factor of 2 compared to the the inventory" , "****",
        (length(subset(df, df <= 1 & df >= -1)) / length(df))*100.0, " of measured emissions are within a factor of 10 compared to the the inventory","****"      
      ))
    return(output)
  }
  outputStats$magnitude_differences_buildings$data_50m = examineMagnitudes(inputList$data_epicc_log_50m_gt0,"bco2e_may", 50)
  outputStats$magnitude_differences_buildings$data_100m = examineMagnitudes(inputList$data_epicc_log_100m_gt0,"bco2e_may", 100)
  outputStats$magnitude_differences_buildings$data_200m = examineMagnitudes(inputList$data_epicc_log_200m_gt0,"bco2e_may",200)
  outputStats$magnitude_differences_buildings$data_400m = examineMagnitudes(inputList$data_epicc_log_400m_gt0,"bco2e_may",400)
  # magnitude_differences_traffic
  outputStats$magnitude_differences_traffic$data_50m = examineMagnitudes(inputList$data_epicc_log_50m_gt0,"e_10_14_hr", 50)
  outputStats$magnitude_differences_traffic$data_100m = examineMagnitudes(inputList$data_epicc_log_100m_gt0,"e_10_14_hr", 100)
  outputStats$magnitude_differences_traffic$data_200m = examineMagnitudes(inputList$data_epicc_log_200m_gt0,"e_10_14_hr",200)
  outputStats$magnitude_differences_traffic$data_400m = examineMagnitudes(inputList$data_epicc_log_400m_gt0,"e_10_14_hr",400)
  # magnitude_differences_total
  outputStats$magnitude_differences_total$data_50m = examineMagnitudes(inputList$data_epicc_log_50m_gt0,"bt_co2e", 50)
  outputStats$magnitude_differences_total$data_100m = examineMagnitudes(inputList$data_epicc_log_100m_gt0,"bt_co2e", 100)
  outputStats$magnitude_differences_total$data_200m = examineMagnitudes(inputList$data_epicc_log_200m_gt0,"bt_co2e",200)
  outputStats$magnitude_differences_total$data_400m = examineMagnitudes(inputList$data_epicc_log_400m_gt0,"bt_co2e",400)
  
  
  # correlation_coeffs_buildings_unfiltered 
  getCorrelations = function(data_epicc_log){
    data_epicc_log_bt = subset(data_epicc_log, data_epicc_log@data$bt_co2e > 2 & data_epicc_log@data$co2_avg_e > 2 )    
    output = c(
        paste("filtered-The correlation between buildings and measured emissions: ", 
              cor(log10(data_epicc_log@data$bco2e_may), log10(data_epicc_log@data$co2_avg_e)), sep=""),
        paste("filtered-The correlation between traffic and measured emissions: ",
              cor(log10(data_epicc_log@data$e_10_14_hr), log10(data_epicc_log@data$co2_avg_e)), sep=""),
        paste("The correlation between total and measured emissions: ", 
              cor(log10(data_epicc_log@data$bt_co2e), log10(data_epicc_log@data$co2_avg_e)), sep=""),
        paste("filtered-The correlation between buildings and measured emissions: ", 
              cor(log10(data_epicc_log_bt@data$bco2e_may), log10(data_epicc_log_bt@data$co2_avg_e)), sep=""),
        paste("filtered-The correlation between traffic and measured emissions: ", 
              cor(log10(data_epicc_log_bt@data$e_10_14_hr), log10(data_epicc_log_bt@data$co2_avg_e)), sep=""),
        paste("filtered-The correlation between total and measured emissions: ",
              cor(log10(data_epicc_log_bt@data$bt_co2e), log10(data_epicc_log_bt@data$co2_avg_e)), sep="")
      )
    return(output)
  }
  outputStats$correlation_coeffs$data_50m = getCorrelations(inputList$data_epicc_log_50m_gt0)
  outputStats$correlation_coeffs$data_100m = getCorrelations(inputList$data_epicc_log_100m_gt0)
  outputStats$correlation_coeffs$data_200m = getCorrelations(inputList$data_epicc_log_200m_gt0)
  outputStats$correlation_coeffs$data_400m = getCorrelations(inputList$data_epicc_log_400m_gt0)

  
  # mean absolute error
  outputStats$mean_absolute_error$data_50m  = mae(inputList$data_epicc_50m@data$bt_co2e - inputList$data_epicc_50m@data$co2_avg_e)
  outputStats$mean_absolute_error$data_100m =mae(inputList$data_epicc_100m@data$bt_co2e - inputList$data_epicc_100m@data$co2_avg_e)
  outputStats$mean_absolute_error$data_200m =mae(inputList$data_epicc_200m@data$bt_co2e - inputList$data_epicc_200m@data$co2_avg_e)
  outputStats$mean_absolute_error$data_400m =mae(inputList$data_epicc_400m@data$bt_co2e - inputList$data_epicc_400m@data$co2_avg_e)
  
  return(outputStats)
}
experiment_150528_stats = listStatistics(experiment_150528)
experiment_160318_stats = listStatistics(experiment_160318)

# ------------ get summary stats for experiment 160318  ------------- #
# how many grid cells are in the data
nrow(experiment_160318$data_100m_within@data)
# how many grid cells after removing NA's 
nrow(experiment_160318$data_100m_within_nona@data)
nrow(experiment_150528$data_100m_within_nona@data)

# give the stats as a table from the following data
tableFromSummary(experiment_160318_stats$summary_stats_co2_ppm_within)
tableFromSummary(experiment_160318_stats$summary_stats_measured_emissions_within)
tableFromSummary(experiment_160318_stats$summary_stats_measured_emissions_epicc)
tableFromSummary(experiment_160318_stats$summary_stats_total_emissions_epicc)
tableFromSummary(experiment_160318_stats$summary_stats_building_emissions_epicc)
tableFromSummary(experiment_160318_stats$summary_stats_traffic_emissions_within)
experiment_160318_stats$mean_absolute_error
experiment_160318_stats$rmse_report
experiment_160318_stats$correlation_coeffs
experiment_160318_stats$magnitude_differences_buildings
# What is the maximum emissions from the experiment
max(experiment_160318$data_100m@data$co2_avg_e, na.rm=T)
# plot the data with no Na's
plot(experiment_160318$data_100m_within_nona)
# give the summary stats from buildings and traffic
tableFromSummary(experiment_160318_stats$summary_stats_building_emissions_epicc)
tableFromSummary(experiment_160318_stats$summary_stats_traffic_emissions_epicc)

# ------------ get summary stats for experiment 150528  ------------- #
tableFromSummary(experiment_150528_stats$summary_stats_co2_ppm_within)
tableFromSummary(experiment_150528_stats$summary_stats_measured_emissions_epicc)
tableFromSummary(experiment_150528_stats$summary_stats_total_emissions_epicc)
tableFromSummary(experiment_150528_stats$summary_stats_building_emissions_epicc)
tableFromSummary(experiment_150528_stats$summary_stats_traffic_emissions_within)
experiment_150528_stats$mean_absolute_error
experiment_150528_stats$rmse_report
experiment_150528_stats$correlation_coeffs
experiment_150528_stats$magnitude_differences_buildings


# ---------- T-test between neighborhoods as suggested by Velasco --------- #
# Subset by neighborhood ($hoodgrouped)
tmp_hoodNames = unique(experiment_160318$data_100m_within@data$hoodgrouped)



compareHoods = function(df){
  # for each neighborhood subset out the grid cell values
  myData = df$data_100m_within@data
  myHoodPixels = list()
  # the results of the test
  testResults = list()
  # p-value matrix
  pValMatrix = c()
  
  for(i in 1:length(tmp_hoodNames)){
    tmpCells = subset(myData$co2_avg, myData$hoodgrouped == tmp_hoodNames[i])
    myHoodPixels[[i]] <- tmpCells
  }
  names(myHoodPixels) = tmp_hoodNames
  # print(myHoodPixels)
  
  # TODO use recursion to compare - for now try below
  for(i in 1:length(myHoodPixels)){
    nh = list()
    pvals = c()
    for(j in 1:length(myHoodPixels)){
      mytest = t.test(myHoodPixels[[i]], myHoodPixels[[j]])
      mytest$data.name = paste(names(myHoodPixels)[[i]], names(myHoodPixels)[[j]], sep =" VS ")
      # get the results of each test
      nh[[j]] = mytest
      #get those pvals
      pvals[[j]] =  mytest$p.value
    }
    testResults[[i]] = nh
    pValMatrix[[i]] = pvals
    print(pvals)
  }
  names(testResults) = tmp_hoodNames
  # print(testResults)
  pValMatrix = do.call(rbind, pValMatrix)
  rownames(pValMatrix) <- tmp_hoodNames
  colnames(pValMatrix) <- tmp_hoodNames
  
  # return the interesting objects
  output = list("testResults" = testResults, "pValMatrix" = pValMatrix)
  return(output)
}
comparedNeighborhoods_160318 = compareHoods(experiment_160318)
comparedNeighborhoods_150528 = compareHoods(experiment_150528)

heatmap(comparedNeighborhoods_160318$pValMatrix,Colv="Rowv", col = heat.colors(256), margins=c(20,20))
heatmap(comparedNeighborhoods_150528$pValMatrix, Colv="Rowv", col = heat.colors(256), margins=c(20,20))


# ------------------------------- Handy Expressions -------------------------------------#
label_co2ppm ="Mixing ratios (ppm)"
label_measured_emissions = expression(paste("Measured Emissions (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1, ")",sep=""))
label_measured_emissions_uptake = expression(paste("Net Uptake (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1, ")",sep=""))
label_building_emissions = expression(paste("Building Emissions (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_traffic_emissions = expression(paste("Traffic Emissions (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_total_emissions = expression(paste("Total Emissions (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_absdiff_emissions = expression(paste("Absolute Emissions Differences: (", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))

label_measured_emissions_log = expression(paste("Measured Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1, ")",sep=""))
label_building_emissions_log = expression(paste("Building Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_traffic_emissions_log = expression(paste("Traffic Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_total_emissions_log = expression(paste("Total Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))

label_building_emissions_binned = expression(paste("Binned Building Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_traffic_emissions_binned = expression(paste("Binned Traffic Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))
label_total_emissions_binned = expression(paste("Binned Total Emissions (log of ", "kg"," ",CO[2]," ",ha^-1," ",hr^-1,")", sep=" "))




# parameter list for viz
vizParamList = list(
  
  map_measuredEmissions = list(
    name =label_measured_emissions ,
    low = "#ffff99",
    mid = "red", 
    midpoint =120, 
    high = muted("red"), 
    limits = c(0, max(experiment_160318$data_100m_within_gg$co2_avg_e, na.rm=T))
  ),
  
  map_buildingEmissions = list(
    name =label_building_emissions ,
    low = "yellow",
    mid = "blue", 
    midpoint =90, 
    high = muted("blue"), 
    limits = c(0, max(experiment_160318$data_epicc_100m_gg$bco2e_may, na.rm=T))
  ),
  
  map_trafficEmissions = list(
    name =label_traffic_emissions ,
    low = "pink",
    mid = "blue", 
    midpoint =100, 
    high = muted("blue"), 
    limits = c(0, max(experiment_160318$data_100m_within_gg$e_10_14_hr, na.rm=T))
  ),
  
  map_totalEmissions = list(
    name =label_total_emissions ,
    low = "orange",
    mid = "blue", 
    midpoint =110, 
    high = muted("blue"), 
    limits = c(0, max(experiment_160318$data_epicc_100m_gg$bt_co2e, na.rm=T))
  ),
  
  map_inventories_consistent = list(
    name =NULL ,
    low = "#ffff99",
    mid = "red",  
    midpoint =110, 
    high = muted("red"), 
    limits = c(0, max(experiment_160318$data_epicc_100m_gg$bt_co2e, na.rm=T))
  ),
  
  map_co2ppm = list(
    name =label_co2ppm ,
    low = "#ffbf80",
    mid = "blue", 
    midpoint =480, 
    high = muted("blue"), 
    limits = c(380, max(experiment_160318$data_100m_within_gg$co2_avg, na.rm=T))
  )

)

# # Create List of charts
chartFunctions = list(
  # make generic choropleth map
  makeChoropleth = function(idata, variable, scaleParams){
    # Context
    p = ggplot() +  
      # metrovan
      geom_polygon(data=metrovan_gg, aes(long,lat,group=group),fill="white", size=1) +
      # roads
      geom_path(data=roads_gg,aes(x=long,y=lat,group=group),color="black", size=1, alpha=0.15)  +
      geom_path(data=roads_gg,aes(x=long,y=lat,group=group),color="white", size=0.5)  +
      # transect
      geom_polygon(data=transect_gg,aes(x=long,y=lat,group=group),color="red", fill=rgb(0,0,0,0), size=0.5) 
    
    # the data
    p = p +
      geom_polygon(data=idata, aes_string("long","lat",group="id",fill=variable)) + 
      geom_path(color="white", size=0.15) +
      scale_fill_gradient2(
        name =scaleParams$name ,
        low = scaleParams$low,
        mid = scaleParams$mid, 
        midpoint =scaleParams$midpoint, 
        high = scaleParams$high, 
        limits = scaleParams$limits ) + 
      coord_equal(ratio=1) +
      coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))
    
    return(p)
  },
  
  # make log10-log10 plot
  makeLogLogPlot = function(idata, variable, title){
    
    idata@data[variable] = log10(idata@data[variable])
    idata@data$co2_avg_e = log10(idata@data$co2_avg_e)
    
    p = ggplot(idata@data, aes_string(x=variable, y="co2_avg_e", colour="hoodgrouped", group="id")) +
      geom_point() +
      scale_colour_manual(values=cbPalette)+
      ggtitle(title) +
      #     guides(colour=FALSE) +
      theme_bw() +
      geom_abline(intercept = 0)+
      geom_abline(intercept = 1,linetype = 2, alpha=0.5)+
      geom_abline(intercept = -1, linetype = 2,alpha=0.5)+
      xlim(-1, 2.5) + ylim(-2,2.5)
    p
    return(p)
  }
    
  
)

saveToPdf = function(chart,fname, gridSize){
  ofile = paste(getwd(), "/assets/AMT/", fname, "-",gridSize, ".pdf",sep="")
  pdf(ofile, width = 10, height=10)
  print(chart)
  dev.off()
}

showNetUptake = function(idata){
  tmp = subset(idata, idata$co2_avg_e <=0)
  p =  ggplot()+ 
    geom_polygon(data=tmp, 
               aes_string(x="long", y="lat", group="id", fill="co2_avg_e")) + 
    geom_path(color="white", size=0.15) + 
    scale_fill_gradient(name =label_measured_emissions_uptake,
                        low = "green", 
                        high = "white", 
                        limits = c(-14, 0)) +
    coord_equal(ratio=1) +
    coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000)) +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  return(p)
}







# winter
## Building Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_50m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "winter-building-emissions", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
  "winter-building-emissions", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_200m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "winter-building-emissions", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_400m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "winter-building-emissions", 400)

## Traffic Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_50m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "winter-traffic-emissions", 50)
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "winter-traffic-emissions", 100)
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_200m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "winter-traffic-emissions", 200)
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_400m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "winter-traffic-emissions", 400)

## Measured emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_50m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "winter-measured-emissions", 50)
saveToPdf(showNetUptake(experiment_160318$data_epicc_50m_gg),
          "winter-measured-emissions-ne", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "winter-measured-emissions", 100)
saveToPdf(showNetUptake(experiment_160318$data_epicc_100m_gg),
          "winter-measured-emissions-ne", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_200m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "winter-measured-emissions", 200)
saveToPdf(showNetUptake(experiment_160318$data_epicc_200m_gg),
          "winter-measured-emissions-ne", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_400m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "winter-measured-emissions", 400)
saveToPdf(showNetUptake(experiment_160318$data_epicc_400m_gg),
          "winter-measured-emissions-ne", 400)



## Total Emissions
## Building Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_50m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "winter-total-emissions", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "winter-total-emissions", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_200m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "winter-total-emissions", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_160318$data_epicc_400m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "winter-total-emissions", 400)






# Summer
## Building Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_50m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "summer-building-emissions", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_100m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "summer-building-emissions", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_200m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "summer-building-emissions", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_400m_gg, 'bco2e_may', vizParamList$map_buildingEmissions),
          "summer-building-emissions", 400)

## Traffic Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_50m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "summer-traffic-emissions", 50)
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_100m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "summer-traffic-emissions", 100)
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_200m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "summer-traffic-emissions", 200)
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_400m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions),
          "summer-traffic-emissions", 400)


## Total Emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_50m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "summer-total-emissions", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_100m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "summer-total-emissions", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_200m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "summer-total-emissions", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_400m_gg, 'bt_co2e', vizParamList$map_buildingEmissions),
          "summer-total-emissions", 400)

## Measured emissions
saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_50m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "summer-measured-emissions", 50)
saveToPdf(showNetUptake(experiment_150528$data_epicc_50m_gg),
          "summer-measured-emissions-ne", 50)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_100m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "summer-measured-emissions", 100)
saveToPdf(showNetUptake(experiment_150528$data_epicc_100m_gg),
          "summer-measured-emissions-ne", 100)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_200m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "summer-measured-emissions", 200)
saveToPdf(showNetUptake(experiment_150528$data_epicc_200m_gg),
          "summer-measured-emissions-ne", 200)

saveToPdf(chartFunctions$makeChoropleth(experiment_150528$data_epicc_400m_gg, 'co2_avg_e', vizParamList$map_measuredEmissions),
          "summer-measured-emissions", 400)
saveToPdf(showNetUptake(experiment_150528$data_epicc_400m_gg),
          "summer-measured-emissions-ne", 400)











# winter
pdf(paste(getwd(), "/assets/AMT/", "winter-measured-emissions-gte0",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'co2_avg_e', vizParamList$map_measuredEmissions)
dev.off()


pdf(paste(getwd(), "/assets/AMT/", "winter-measured-emissions-lte0",".pdf",sep=""), width=10, height=10)
ggplot() +  
  geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <=0), 
               aes(x=long, y=lat, group=group, fill=co2_avg_e)) + 
  geom_path(color="white", size=0.15) +
  scale_fill_gradient(name =label_measured_emissions,
                      low = "green", 
                      high = "white", 
                      limits = c(-14, 0)) + coord_equal() +
  coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))+
  #       theme(legend.position="bottom")+
#   theme(aspect.ratio = 1)

dev.off()

# Summer
pdf(paste(getwd(), "/assets/AMT/", "summer-measured-emissions-gte0",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_150528$data_100m_within_gg, 'co2_avg_e', vizParamList$map_measuredEmissions)
dev.off()


pdf(paste(getwd(), "/assets/AMT/", "summerr-measured-emissions-lte0",".pdf",sep=""), width=10, height=10)
ggplot() +  
  geom_polygon(data=subset(experiment_150528$data_100m_within_gg, experiment_150528$data_100m_within_gg$co2_avg_e <=0), 
               aes(x=long, y=lat, group=group, fill=co2_avg_e)) + 
  geom_path(color="white", size=0.15) +
  scale_fill_gradient(name =label_measured_emissions,
                      low = "green", 
                      high = "white", 
                      limits = c(-14, 0)) + coord_equal() +
  coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))+
  #       theme(legend.position="bottom")+
  theme(aspect.ratio = 1)

dev.off()


# ---- Winter ---- #
# Measured Emissions
chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'co2_avg_e', vizParamList$map_measuredEmissions) + 
  geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <0), 
               aes(x=long, y=lat, group=group, color=co2_avg_e), fill="green") + 
  geom_path(color="white", size=0.15) +
  scale_color_gradient(name =label_measured_emissions_uptake ,
                      low = "green", 
                      high = "white", 
                      limits = c(-14, -0.9999))


# Building Emissions
pdf(paste(getwd(), "/assets/AMT/", "winter-building-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bco2e_may', vizParamList$map_buildingEmissions)
dev.off()
# Traffic Emissions
pdf(paste(getwd(), "/assets/AMT/", "winter-traffic-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions)
dev.off()
# Total Emissions
pdf(paste(getwd(), "/assets/AMT/", "winter-total-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bt_co2e', vizParamList$map_totalEmissions)
dev.off()
# co2 mixing ratios
pdf(paste(getwd(), "/assets/AMT/", "winter-co2ppm-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'co2_avg', vizParamList$map_co2ppm) 
dev.off()



# consistent param list
pdf(paste(getwd(), "/assets/AMT/", "winter-buildings-emissions-100m-same",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bco2e_may', vizParamList$map_inventories_consistent)
dev.off()

pdf(paste(getwd(), "/assets/AMT/", "winter-traffic-emissions-100m-same",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'e_10_14_hr', vizParamList$map_inventories_consistent)
dev.off()


pdf(paste(getwd(), "/assets/AMT/", "winter-total-emissions-100m-same",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_160318$data_epicc_100m_gg, 'bt_co2e', vizParamList$map_inventories_consistent)
dev.off()



cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# log log plot
p1 = chartFunctions$makeLogLogPlot(experiment_160318$data_epicc_log_100m_gt0, "bco2e_may", "Measured Emissions vs Building Emissions") + guides(colour=FALSE) 
p2 = chartFunctions$makeLogLogPlot(experiment_160318$data_epicc_log_100m_gt0, "e_10_14_hr", "Measured Emissions vs Traffic Emissions")  + guides(colour=FALSE) 
p3 = chartFunctions$makeLogLogPlot(experiment_160318$data_epicc_log_100m_gt0, "bt_co2e", "Measured Emissions vs Total Emissions") + theme(legend.position="bottom")

multiplot(p1, p2, p3, cols=1)





# ---- Summer ---- #

chartFunctions$makeChoropleth(experiment_160318$data_100m_within_gg, 'co2_avg_e', vizParamList$map_measuredEmissions) + 
  geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <0), 
               aes(x=long, y=lat, group=group, color=co2_avg_e), fill="green") + 
  geom_path(color="white", size=0.15) +
  scale_color_gradient(name =label_measured_emissions_uptake ,
                       low = "green", 
                       high = "green", 
                       limits = c(-14, -0.9999))+ coord_equal() +
  coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))

# Building Emissions
pdf(paste(getwd(), "/assets/AMT/", "summer-building-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_150528$data_epicc_100m_gg, 'bco2e_may', vizParamList$map_buildingEmissions)
dev.off()
# Traffic Emissions
pdf(paste(getwd(), "/assets/AMT/", "summer-traffic-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_150528$data_100m_within_gg, 'e_10_14_hr', vizParamList$map_trafficEmissions)
dev.off()
# Total Emissions
pdf(paste(getwd(), "/assets/AMT/", "summer-total-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_150528$data_epicc_100m_gg, 'bt_co2e', vizParamList$map_totalEmissions)
dev.off()
# co2 mixing ratios
pdf(paste(getwd(), "/assets/AMT/", "summer-co2ppm-emissions-100m",".pdf",sep=""), width=10, height=10)
chartFunctions$makeChoropleth(experiment_150528$data_100m_within_gg, 'co2_avg', vizParamList$map_co2ppm) 
dev.off()


# +annotate("text",x=490000, y = 5457000, label="some text")

# log log plot
p1 = chartFunctions$makeLogLogPlot(experiment_150528$data_epicc_log_100m_gt0, "bco2e_may", "Measured Emissions vs Building Emissions") + guides(colour=FALSE) 
p2 = chartFunctions$makeLogLogPlot(experiment_150528$data_epicc_log_100m_gt0, "e_10_14_hr", "Measured Emissions vs Traffic Emissions")  + guides(colour=FALSE) 
p3 = chartFunctions$makeLogLogPlot(experiment_150528$data_epicc_log_100m_gt0, "bt_co2e", "Measured Emissions vs Total Emissions") + theme(legend.position="bottom")
multiplot(p1, p2, p3, cols=1)




# 
# 
# ggplot()+
#   geom_polygon(data=subset(experiment_150528$data_100m_within_gg, is.na(experiment_150528$data_100m_within_gg$bt_co2e) ==F), 
#                aes(x=long, y=lat, group=group, fill=bt_co2e)) + 
#   geom_path(color="white", size=0.15) +
#   scale_fill_gradient(name =label_measured_emissions_uptake ,
#                        low = "white", 
#                        high = "green")
# 
# ggplot()+
#   geom_polygon(data=subset(experiment_150528$data_100m_within_gg, is.na(experiment_150528$data_100m_within_gg$bco2e_may) ==F), 
#                aes(x=long, y=lat, group=group, fill=bco2e_may)) + 
#   geom_path(color="white", size=0.15) +
#   scale_fill_gradient(name =label_measured_emissions_uptake ,
#                       low = "white", 
#                       high = "green")
# 
# 
# rm(dat1, dat2, test)
# rm(scaleParams_buildingEmissions, scaleParams_measuredEmissions, temp)
ggplot()+
    # Geometry 1
    geom_polygon(data=experiment_160318$data_100m_within_gg, aes(long,lat,group=group,fill=co2_avg_e)) + 
    geom_path(color="white", size=0.15) +
    scale_fill_gradient2(name =label_measured_emissions ,
                         low = "#ffff99",
                         mid = "red", 
                         midpoint =120, 
                         high = muted("red"), 
                         limits = c(0, max(experiment_160318$data_100m_within_gg$co2_avg_e, na.rm=T))) +
  # Geometry 2
  geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <0), 
               aes(x=long, y=lat, group=group, color=co2_avg_e), fill="green") + 
  geom_path(color="white", size=0.15) +
  scale_color_gradient(name =label_measured_emissions_uptake ,
                       low = "green", 
                       high = "green", 
                       limits = c(-14, -0.9999))+ coord_equal() +
  coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))


head(experiment_160318$data_100m_within_gg)


# ggplot()+
#   # metrovan
#   geom_polygon(data=metrovan_gg, aes(long,lat,group=group),fill="white", size=1) +
#   # roads
#   geom_path(data=roads_gg,aes(x=long,y=lat,group=group),color="black", size=1, alpha=0.15)  +
#   geom_path(data=roads_gg,aes(x=long,y=lat,group=group),color="white", size=0.5)  +
#   # transect
#   geom_polygon(data=transect_gg,aes(x=long,y=lat,group=group),color="red", fill=rgb(0,0,0,0), size=0.5)  +
#   # Data to be drawn
#   geom_polygon(data=experiment_160318$data_100m_within_gg, aes(long,lat,group=group,fill=co2_avg_e)) + 
#   geom_path(color="white", size=0.15) +
#   scale_fill_gradient2(name =label_measured_emissions ,
#                        low = "#ffff99",
#                        mid = "red", 
#                        midpoint =120, 
#                        high = muted("red"), 
#                        limits = c(0, max(experiment_160318$data_100m_within_gg$co2_avg_e, na.rm=T))) +
#   geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <=0 ), 
#                aes(x=long, y=lat, group=group, fill=1), fill="green") + 
#   
#   coord_cartesian(ylim = c(5451500, 5462550),xlim = c(488000, 496000))
# 
# 

# 
# png(paste(getwd(), "/assets/test/", "test-",100,".png" ,sep=""), width=960, height=700 )
test = spTransform(experiment_160318$data_100m_within, projection_wgs84)
test = makeSpatialGG(test)

map <- get_map(location = c(lon = -123.110219, lat = 49.271250), zoom = 12, source = "stamen",maptype=("toner-lite"),  color = "bw")
maptiles = ggmap(map)
maptiles + geom_polygon(data=test, aes(long,lat,group=group,fill=co2_avg_e)) +
    geom_path(color="white", size=0.15)


# maptiles +
#   geom_polygon(data=experiment_160318$data_100m_within_gg, aes(long,lat,group=group,fill=co2_avg_e)) +
#   geom_path(color="white", size=0.15) +
#   coord_equal() + scale_fill_gradient2(low = "#ffff99",mid = "red", 
#                                        midpoint =120, high = muted("red"), 
#                                        limits = c(0, max(experiment_160318$data_100m_within_gg$co2_avg_e, na.rm=T))) +
#   geom_polygon(data=subset(experiment_160318$data_100m_within_gg, experiment_160318$data_100m_within_gg$co2_avg_e <=0 ), aes(x=long, y=lat, group=group), fill="green") +
#   geom_path(color="white", size=0) +
#   coord_equal()
# dev.off()