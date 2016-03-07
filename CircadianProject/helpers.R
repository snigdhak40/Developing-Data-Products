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
 
153
                            maxcouplingStrength, couplingTypes) {
 
154
  dt <- groupByPeriodAll(dt, minperiod, maxperiod, minnumberofOscillators,
 
155
                       maxnumberofOscillators, mincouplingStrength,
 
156
                       maxcouplingStrength, couplingTypes)
 
157
  result <- dt %>% 
 
158
    group_by(period) %>% 
 
159
    summarise(avg = mean(numberofOscillators)) %>%
 
160
    arrange(period)
 
161
  return(result)      
 
162
}
 
163
 
164
#' Average numberofOscillators for each couplingType
 
165
#' 
 
166
#' @param dt data.table
 
167
#' @param minperiod
 
168
#' @param maxperiod
 
169
#' @param minnumberofOscillators
 
170
#' @param maxnumberofOscillators
 
171
#' @param couplingTypes
 
172
#' @return data.table 2 columns
 
173
#'
 
174
groupBynumberofOscillatorscouplingTypeAvg <- function(dt,  minperiod, maxperiod, minnumberofOscillators,
 
175
                                 maxnumberofOscillators, mincouplingStrength,
 
176
                                 maxcouplingStrength, couplingTypes) {
 
177
  dt <- groupByPeriodAll(dt, minperiod, maxperiod, minnumberofOscillators,
 
178
                       maxnumberofOscillators, mincouplingStrength,
 
179
                       maxcouplingStrength, couplingTypes)
 
180
  result <- dt %>% 
 
181
    group_by(couplingType) %>%
 
182
    summarise(avgnumberofOscillators = mean(numberofOscillators)) %>%
 
183
    arrange(couplingType)
 
184
  return(result)
 
185
}
 
186
 
187
#' Plot couplingStrength by period
 
188
#' 
 
189
#' @param dt data.table
 
190
#' @param dom
 
191
#' @param xAxisLabel period
 
192
#' @param yAxisLabel number of sets
 
193
#' @return couplingStrengthByperiod plot
 
194
plotcouplingStrengthCountByperiod <- function(dt, dom = "couplingStrengthByPeriod", 
 
195
                                xAxisLabel = "Period",
 
196
                                yAxisLabel = "Coupling Strength") {
 
197
  couplingStrengthByPeriod <- nPlot(
 
198
    total_couplingStrengths~period,
 
199
    data = dt,
 
200
    type = "stackedAreaChart",
 
201
    dom = dom, width = 650
 
202
  )
 
203
  couplingStrengthByPeriod$chart(margin = list(left = 100))
 
204
  couplingStrengthByPeriod$chart(color = c('purple', 'blue', 'green'))
 
205
  couplingStrengthByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
 
206
                   return '<h5><b>period</b>: ' + e.point.period + '<br>' + '<b>Total Coupling trength</b>: ' 
 
207
                   + e.point.total_couplingStrengths + '<br>'
 
208
                   + '</h5>'
 
209
} !#")
 
210
  couplingStrengthByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
 
211
  couplingStrengthByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
 
212
  couplingStrengthByPeriod 
 
213
  }
 
214
 
215
#' Plot number of couplingTypes by period
 
216
#' 
 
217
#' @param dt data.table
 
218
#' @param dom
 
219
#' @param xAxisLabel period
 
220
#' @param yAxisLabel number of couplingTypes
 
221
#' @return couplingTypesByperiod plot
 
222
plotcouplingTypesCountByPeriod <- function(dt, dom = "couplingTypesByPeriod", 
 
223
                                  xAxisLabel = "period",
 
224
                                  yAxisLabel = "Number of couplingTypes") {
 
225
  couplingTypesByPeriod <- nPlot(
 
226
    count ~ period,
 
227
    data = dt,
 
228
    type = "multiBarChart",
 
229
    dom = dom, width = 650
 
230
  )
 
231
  couplingTypesByPeriod$chart(margin = list(left = 100))
 
232
  couplingTypesByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
 
233
  couplingTypesByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
 
234
#   couplingTypesByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
 
235
#                      return '<h5><b>period</b>: ' + e.point.period + '<br>' + '<b>Total couplingTypes</b>: ' + e.point.count + '<br>'
 
236
#                      + '</h5>'
 
237
# } !#")
 
238
  couplingTypesByPeriod
 
239
  }
 
240
 
241
#' Plot number of Oscillators by period
 
242
#' 
 
243
#' @param dt data.table
 
244
#' @param dom
 
245
#' @param xAxisLabel period
 
246
#' @param yAxisLabel number of numberofOscillators
 
247
#' @return plotnumberofOscillatorsByperiod plot
 
248
plotnumberofOscillatorsByPeriod <- function(dt, dom = "numberofOscillatorsByPeriod", 
 
249
                             xAxisLabel = "period", 
 
250
                             yAxisLabel = "Number of Oscillators") {
 
251
  numberofOscillatorsByPeriod <- nPlot(
 
252
    numberofOscillators ~ period,
 
253
    data = dt,
 
254
    type = "scatterChart",
 
255
    dom = dom, width = 650
 
256
  )
 
257
  numberofOscillatorsByPeriod$chart(margin = list(left = 100), 
 
258
                     showDistX = TRUE,
 
259
                     showDistY = TRUE)
 
260
  numberofOscillatorsByPeriod$chart(color = c('green', 'orange', 'blue'))
 
261
  numberofOscillatorsByPeriod$chart(tooltipContent = "#! function(key, x, y, e){ 
 
262
                     return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
 
263
                     + '<b>Set ID</b>: ' + e.point.setId + '<br>'
 
264
                     + '<b>couplingType</b>: ' + e.point.couplingType
 
265
                     + '</h5>'
 
266
} !#")
 
267
  numberofOscillatorsByPeriod$yAxis(axisLabel = yAxisLabel, width = 80)
 
268
  numberofOscillatorsByPeriod$xAxis(axisLabel = xAxisLabel, width = 70)
 
269
  #     numberofOscillatorsByperiod$chart(useInteractiveGuideline = TRUE)
 
270
  numberofOscillatorsByPeriod
 
271
  }
 
272
 
273
#' Plot average number of Oscillators by period
 
274
#' 
 
275
#' @param dt data.table
 
276
#' @param dom
 
277
#' @param xAxisLabel period
 
278
#' @param yAxisLabel number of numberofOscillators
 
279
#' @return couplingTypesByperiod plot
 
280
plotnumberofOscillatorsByPeriodAvg <- function(dt, dom = "numberofOscillatorsByPeriodAvg", 
 
281
                                xAxisLabel = "period",
 
282
                                yAxisLabel = "Number of Oscillators") {
 
283
  
 
284
  numberofOscillatorsByPeriodAvg <- nPlot(
 
285
    avg ~ period,
 
286
    data = dt,
 
287
    type = "lineChart",
 
288
    dom = dom, width = 650
 
289
  )
 
290
  numberofOscillatorsByPeriodAvg$chart(margin = list(left = 100))
 
291
  numberofOscillatorsByPeriodAvg$chart(color = c('orange', 'blue', 'green'))
 
292
  numberofOscillatorsByPeriodAvg$yAxis(axisLabel = yAxisLabel, width = 80)
 
293
  numberofOscillatorsByPeriodAvg$xAxis(axisLabel = xAxisLabel, width = 70)
 
294
  numberofOscillatorsByPeriodAvg
 
295
}
 
296
 
297
#' Plot average number of Oscillators by couplingType
 
298
#' 
 
299
#' @param dt data.table
 
300
#' @param dom
 
301
#' @param xAxisLabel couplingType
 
302
#' @param yAxisLabel number of numberofOscillators
 
303
#' @return numberofOscillatorsBycouplingTypeAvg plot
 
304
plotnumberofOscillatorsBycouplingTypeAvg <- function(dt, dom = "numberofOscillatorsBycouplingTypeAvg", 
 
305
                                 xAxisLabel = "couplingTypes", 
 
306
                                 yAxisLabel = "Number of Oscillators") {
 
307
  numberofOscillatorsBycouplingTypeAvg <- nPlot(
 
308
    avgnumberofOscillators ~ couplingType,
 
309
    data = dt,
 
310
    type = "multiBarChart",
 
311
    dom = dom, width = 650
 
312
  )
 
313
  numberofOscillatorsBycouplingTypeAvg$chart(margin = list(left = 100))
 
314
  numberofOscillatorsBycouplingTypeAvg$chart(color = c('pink', 'blue', 'green'))
 
315
  numberofOscillatorsBycouplingTypeAvg$yAxis(axisLabel = yAxisLabel, width = 80)
 
316
  numberofOscillatorsBycouplingTypeAvg$xAxis(axisLabel = xAxisLabel, width = 200,
 
317
                         rotateLabels = -20, height = 200)
 
318
  numberofOscillatorsBycouplingTypeAvg
 
319
}
