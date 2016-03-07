# setwd("C:/Desktop/Online Coursera/Coursera-Developing-Data-Products/project/")

# Load required libraries

require(data.table)

# library(sqldf)

library(dplyr)

library(DT)

library(rCharts)

# Read data from csv file and change column names

circData = fread("./data/Book1.csv")
head(circData)
setnames(circData,"cs","couplingStrength")
setnames(circData,"c1","couplingType")
setnames(circData,"n1","numberofOscillators")
setnames(circData,"percryRNA","perCryRNAconcentration")
# Exploratory data analysis
sum(is.na(circData$couplingStrength)) # 
sum(is.na(circData$period)) # 0
length(unique(circData$period)) # 3
unique(circData$period)
unique(circData$couplingStrength)
length(unique(circData$couplingStrength)) # 104
length(unique(circData$couplingType)) # 3
unique(circData$couplingType)
periods <- sort(unique(circData$period))
length(table(circData$ccOuplingStrength)) # 104
unique(circData$numberofOscillators) 
min(circData$numberofOscillators) # 0
max(circData$numberofOscillators) # 390
couplingTypes <- sort(unique(circData$couplingType))

## Helper functions
#' Period, numberofOscillators, couplingStrengh and couplingType Aggregate
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @param minnumberofOscillators
#' @param maxnumberofOscillators
#' @param mincouplingStrength
#' @param maxcouplingStrength
#' @param couplingTypes
#' @return data.table
groupByPeriodAll <- function(dt, minperiod, maxperiod, minnumberofOscillators,
                           maxnumberofOscillators, mincouplingStrength,
                           maxcouplingStrength, couplingTypes) {

  result <- dt %>% filter(
                          dt$period > minperiod, dt$period <= maxperiod,
                          dt$numberofOscillators >= minnumberofOscillators, dt$numberofOscillators <= maxnumberofOscillators,
                          dt$couplingStrength >=  mincouplingStrength, dt$couplingStrength <= maxcouplingStrength,
                          dt$couplingType %in% couplingTypes) 
  return(result)
}
#' Period Aggregate
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @return data.table
groupByperiod <- function(dt, minperiod, maxperiod) {
  result <- dt %>% filter( dt$period >= minperiod, dt$period <= maxperiod) 
  return(result)
}
#' couplingStrength Aggregate
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @param minnumberofOscillators
#' @param maxnumberofOscillators
#' @param mincouplingStrength
#' @param maxcouplingStrength
#' @param couplingTypes
#' @return result data.table
groupByperiodcouplingStrength <- function(dt, minperiod, maxperiod, mincouplingStrength,
                                          maxcouplingStrength, minnumberofOscillators,
                                          maxnumberofOscillators, couplingTypes) {
  dt <- groupByperiod(dt, minperiod, maxperiod)
  result <- dt %>% 
    group_by(period) %>% 
    summarise(total_couplingStrengths = n_distinct(couplingStrength)) %>%
    arrange(period)
  return(result) 
}
#' couplingTypes Aggregate
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @param minnumberofOscillators
#' @param maxnumberofOscillators
#' @param mincouplingStrength
#' @param maxcouplingStrength
#' @param couplingTypes
#' @return result data.table
groupBycouplingType <- function(dt, minperiod, maxperiod, 
                         minnumberofOscillators, maxnumberofOscillators, mincouplingStrength,
                         maxcouplingStrength, couplingTypes) {
  dt <- groupByPeriodAll(dt, minperiod, maxperiod, minnumberofOscillators,
                       maxnumberofOscillators,  mincouplingStrength,
                       maxcouplingStrength, couplingTypes) 
  result <- datatable(dt, options = list(iDisplayLength = 100))
  return(result)
}

#' Total count of couplingTypes by Period Aggregate
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @param minnumberofOscillators
#' @param maxnumberofOscillators
#' @param mincouplingStrength
#' @param maxcouplingStrength
#' @param couplingTypes
#' @return data.table 2 columns
groupByPeriodAgg <- function(dt, minperiod, maxperiod, minnumberofOscillators,
                           maxnumberofOscillators, mincouplingStrength,
                           maxcouplingStrength, couplingTypes) {
  # only need the minperiod and maxperiod here
  dt <- groupByperiod(dt, minperiod, maxperiod)
  result <- dt %>% 
    group_by(period)  %>% 
    summarise(count = n_distinct(dt$couplingType)) %>%
    arrange(period)
  return(result)
}
#' Count of average number of Oscillators by Period 
#' @param dt data.table

#' @param minperiod

#' @param maxperiod

#' @param minnumberofOscillators

#' @param maxnumberofOscillators
#' @param couplingTypes
 
149
#' @return data.table 2 columns

groupBynumberofOscillatorsAvg <- function(dt,  minperiod, maxperiod, minnumberofOscillators,
                            maxnumberofOscillators, mincouplingStrength,
                            maxcouplingStrength, couplingTypes) {
  dt <- groupByPeriodAll(dt, minperiod, maxperiod, minnumberofOscillators,
                       maxnumberofOscillators, mincouplingStrength,
                       maxcouplingStrength, couplingTypes)
  result <- dt %>% 
    group_by(period) %>% 
    arrange(period)
  return(result)      
}
#' Average numberofOscillators for each couplingType
#' @param dt data.table
#' @param minperiod
#' @param maxperiod
#' @param minnumberofOscillators
#' @param maxnumberofOscillators
#' @param couplingTypes
#' @return data.table 2 columns
groupBynumberofOscillatorscouplingTypeAvg <- function(dt,  minperiod, maxperiod, minnumberofOscillators,
                                 maxnumberofOscillators, mincouplingStrength,
                                 maxcouplingStrength, couplingTypes) {
  dt <- groupByPeriodAll(dt, minperiod, maxperiod, minnumberofOscillators,
                       maxnumberofOscillators, mincouplingStrength,
                       maxcouplingStrength, couplingTypes)
  result <- dt %>% 
    group_by(couplingType) %>%
    summarise(avgnumberofOscillators = mean(numberofOscillators)) %>%
    arrange(couplingType)
  return(result)
}
#' Plot couplingStrength by period
#' @param dt data.table
#' @param dom
#' @param xAxisLabel period
#' @param yAxisLabel number of sets
#' @return couplingStrengthByperiod plot
plotcouplingStrengthCountByperiod <- function(dt, dom = "couplingStrengthByPeriod", 
                                xAxisLabel = "Period",
                                yAxisLabel = "Coupling Strength") {
  couplingStrengthByPeriod <- nPlot(
    total_couplingStrengths~period,
    data = dt,
    type = "stackedAreaChart",
    dom = dom, width = 650
  )
  couplingStrengthByPeriod$chart(margin = list(left = 100))
  couplingStrengthByPeriod$chart(color = c('purple', 'blue', 'green'))
  couplingStrengthByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
                   return '<h5><b>period</b>: ' + e.point.period + '<br>' + '<b>Total Coupling trength</b>: ' 
                   + e.point.total_couplingStrengths + '<br>'
                   + '</h5>'
} !#")
  couplingStrengthByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
  couplingStrengthByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
  couplingStrengthByPeriod 
  }
#' Plot number of couplingTypes by period
#' @param dt data.table
#' @param dom
#' @param xAxisLabel period
#' @param yAxisLabel number of couplingTypes
#' @return couplingTypesByperiod plot
plotcouplingTypesCountByPeriod <- function(dt, dom = "couplingTypesByPeriod", 
                                  xAxisLabel = "period",
                                  yAxisLabel = "Number of couplingTypes") {
  couplingTypesByPeriod <- nPlot(
    count ~ period,
    data = dt,
    type = "multiBarChart",
    dom = dom, width = 650
  )
  couplingTypesByPeriod$chart(margin = list(left = 100))
  couplingTypesByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
  couplingTypesByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
#   couplingTypesByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
#                      return '<h5><b>period</b>: ' + e.point.period + '<br>' + '<b>Total couplingTypes</b>: ' + e.point.count + '<br>'
#                      + '</h5>'
# } !#")
  couplingTypesByPeriod
  }
#' Plot number of Oscillators by period
#' @param dt data.table
#' @param dom
#' @param xAxisLabel period
#' @param yAxisLabel number of numberofOscillators
#' @return plotnumberofOscillatorsByperiod plot
plotnumberofOscillatorsByPeriod <- function(dt, dom = "numberofOscillatorsByPeriod", 
                             xAxisLabel = "period", 
                             yAxisLabel = "Number of Oscillators") {
  numberofOscillatorsByPeriod <- nPlot(
    numberofOscillators ~ period,
    data = dt,
    type = "scatterChart",
    dom = dom, width = 650
  )
  numberofOscillatorsByPeriod$chart(margin = list(left = 100), 
                     showDistX = TRUE,
                     showDistY = TRUE)
  numberofOscillatorsByPeriod$chart(color = c('green', 'orange', 'blue'))
  numberofOscillatorsByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
                     return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
                     + '<b>Set ID</b>: ' + e.point.setId + '<br>'
                     + '<b>couplingType</b>: ' + e.point.couplingType
                     + '</h5>'
} !#")
  numberofOscillatorsByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
  numberofOscillatorsByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
  #     numberofOscillatorsByperiod$chart(useInteractiveGuideline = TRUE)
  numberofOscillatorsByPeriod
  }
#' Plot average number of Oscillators by period
#' @param dt data.table
#' @param dom
#' @param xAxisLabel period
#' @param yAxisLabel number of numberofOscillators
#' @return couplingTypesByperiod plot
plotnumberofOscillatorsByPeriodAvg <- function(dt, dom = "numberofOscillatorsByPeriodAvg", 
                                xAxisLabel = "period",
                                yAxisLabel = "Number of Oscillators") {
  numberofOscillatorsByPeriodAvg <- nPlot(
    avg ~ period,
    data = dt,
    type = "lineChart",
    dom = dom, width = 650
  )
  numberofOscillatorsByPeriodAvg$chart(margin = list(left = 100))
  numberofOscillatorsByPeriodAvg$chart(color = c('orange', 'blue', 'green'))
  numberofOscillatorsByPeriodAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  numberofOscillatorsByPeriodAvg$xAxis(axisLabel = xAxisLabel, width = 70)
  numberofOscillatorsByPeriodAvg
}
#' Plot average number of Oscillators by couplingType
#' @param dt data.table
#' @param dom
#' @param xAxisLabel couplingType
#' @param yAxisLabel number of numberofOscillators
#' @return numberofOscillatorsBycouplingTypeAvg plot
plotnumberofOscillatorsBycouplingTypeAvg <- function(dt, dom = "numberofOscillatorsBycouplingTypeAvg", 
                                 xAxisLabel = "couplingTypes", 
                                 yAxisLabel = "Number of Oscillators") {
  numberofOscillatorsBycouplingTypeAvg <- nPlot(
    avgnumberofOscillators ~ couplingType,
    data = dt,
    type = "multiBarChart",
    dom = dom, width = 650
  )
  numberofOscillatorsBycouplingTypeAvg$chart(margin = list(left = 100))
  numberofOscillatorsBycouplingTypeAvg$chart(color = c('pink', 'blue', 'green'))
  numberofOscillatorsBycouplingTypeAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  numberofOscillatorsBycouplingTypeAvg$xAxis(axisLabel = xAxisLabel, width = 200,
                         rotateLabels = -20, height = 200)
  numberofOscillatorsBycouplingTypeAvg

}
