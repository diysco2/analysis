# function: convert numeric to factor of a vector
asNumeric = function(x){
  # see: http://stackoverflow.com/questions/8596466/r-change-all-columns-of-type-factor-to-numeric
  as.numeric(as.character(x))
}

# function: convert factors to numeric in a dataframe 
factorsNumeric = function(d){ 
  # see: http://stackoverflow.com/questions/8596466/r-change-all-columns-of-type-factor-to-numeric
  modifyList(d, lapply(d[, sapply(d, is.factor)], asNumeric))
}
# function: convert factors to numeric except for hoods
factorToNumericExcept = function(data){
  factor_columns = colnames(data@data)
  factor_columns = factor_columns[factor_columns !="hoodgrouped" & factor_columns !="NAME"]
  for(i in 1:length(factor_columns)){
    cname = factor_columns[i]
    data@data[[cname]] = asNumeric(data@data[[cname]])
  }
  return(data)
}

# function: subset cells above zero and where bt_co2e != Nan
# subsetCellsAboveZeroEmissions = function(data, gs){
#   data = subset(data, 
#                 is.na(data@data$bt_co2e) == F & 
#                   data@data$bco2e_may>= 0 &
#                   data@data$bt_co2e >= 0 &
#                   data@data$e_10_14_hr >=0 )
#   return(data)
# }

subsetNaCells = function(data, gs){
  data = subset(data, 
                is.na(data@data$bt_co2e) == F &
                  is.na(data@data$co2_avg_e) == F &
                  is.na(data@data$bco2e_may) == F &
                  is.na(data@data$e_10_14_hr) == F & 
                  data@data$co2_cnt >=10)
  return(data)
}

# function: subset cells for comparing:
subsetCellsForValidAnalysis = function(data, gs){
  data = subset(data, 
                is.na(data@data$bt_co2e) == F & 
                  is.na(data@data$co2_avg_e) == F & 
                  is.na(data@data$bco2e_may) == F &
                  is.na(data@data$e_10_14_hr) == F &
                  data@data$bco2e_may>= 0 &
                  data@data$co2_cnt >=10)
  return(data)
}


# function: subset cells for comparing:
subsetForCorrelation = function(data){
  data = subset(data, 
                data@data$bco2e_may> 0 &
                  data@data$bt_co2e > 0 &
                  data@data$e_10_14_hr >0 &
                  data@data$co2_avg_e> 0 )
  return(data)
}

# Convert per grid To per ha
makePerHa = function(idata, gs){
  if(gs == 50){
    idata@data$co2_avg_e =  idata@data$co2_avg_e*4
    idata@data$bt_co2e =  idata@data$bt_co2e*4
    idata@data$bco2e_may =  idata@data$bco2e_may*4
    idata@data$e_10_14_hr =  idata@data$e_10_14_hr*4
  } else if (gs == 100){
    idata@data$co2_avg_e =  idata@data$co2_avg_e
    idata@data$bt_co2e =  idata@data$bt_co2e
    idata@data$bco2e_may =  idata@data$bco2e_may
    idata@data$e_10_14_hr =  idata@data$e_10_14_hr
  } else if (gs == 200){
    idata@data$co2_avg_e =  idata@data$co2_avg_e/4
    idata@data$bt_co2e =  idata@data$bt_co2e/4
    idata@data$bco2e_may =  idata@data$bco2e_may/4
    idata@data$e_10_14_hr =  idata@data$e_10_14_hr/4
  } else if (gs == 400){
    idata@data$co2_avg_e =  idata@data$co2_avg_e/16
    idata@data$bt_co2e =  idata@data$bt_co2e/16
    idata@data$bco2e_may =  idata@data$bco2e_may/16
    idata@data$e_10_14_hr =  idata@data$e_10_14_hr/16
  }
  return(idata)
}

# Get only the cells within the study area
removeEdgeCells = function(idata, mask){
  # use gWithin() to get only cells within the mask
  withinGrid = gWithin(idata, mask, byid=T)
  # subset only those cells
  idata = idata[c(withinGrid),]
  return(idata)
}

# calculate the means of the building, traffic, total, and measured emissions
calcTransectMean = function(idata){
  tmean = c(
    "measured" = mean(idata@data$co2_avg_e, na.rm=T),
    "total"= mean(idata@data$bt_co2e, na.rm=T),
    "traffic" = mean(idata@data$e_10_14_hr, na.rm=T),
    "buildings" = mean(idata@data$bco2e_may, na.rm=T))
  #   print(tmean)
  return(tmean)
}


# print summmary stats to latex pastable text
summaryToTable = function(idata, var, gs){
  df = idata@data[[var]]
  sdf = summary(df)
  print(sdf)
  print(paste(
    paste(gs, "m"), sdf[1],sdf[2],sdf[3],sdf[4],sdf[5],sdf[6], sep=" & "
  ))
}

# fix diffs --- fix this in the python code and then comment this function out!!! 
fixDiffs = function(idata){
  df = idata@data
  df$absdiff = abs(df$co2_avg_e) - abs(df$bt_co2e)
  df$reldiff = (abs(df$co2_avg_e) - abs(df$bt_co2e)) / abs(df$bt_co2e)
  idata@data = df
  return(idata)
}

getSummary = function(idata){
  print(
    summary(idata, na.rm=T)
  )
  print(
    sd(idata, na.rm=T)
  )  
  output = list(
    stats = summary(idata, na.rm=T),
    stddev = sd(idata, na.rm=T)
    )
  
  return(output)
}

makeSpatialGG = function(idata){
  idata@data$id = rownames(idata@data)
  idata_points = fortify(idata, region="id")
  
  idata_output = join(idata_points, idata@data, by="id")
  
  return(idata_output)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  return(mean(abs(error)))
}
