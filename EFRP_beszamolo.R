library(dplyr)
paste0(getwd(), '/EFRP_beszamolo_functions.r') %>% source(.)
setwd("D:/Google Drive/BCE/Mester/20192001/ÚNKP-kutatás")
#1. step: read data

wti <- loadData()

StartDate <- "2014-12-29"
EndDate <- "2016-12-31"
AssetList <- c("Date","CL1", "CL2","CL3","CL4","CL5","CL6","CL7","CL8","CL9","CL10","CL11","CL12","CL13","CL14","CL15","CL16","CL17","CL18","CL19","CL20","CL21","CL22","CL23","CL24")

#2.step: clean-up data: 
  #1,remove weekends
RawData <- prepareDatabase(wti,StartDate,EndDate,AssetList)
#TEMP: subsetting for start of the months(2015 Jan and 2016 Dec)
RawData <- subset(RawData, RawData$Date == as.Date("2015-1-1") | RawData$Date == as.Date("2015-2-2")| RawData$Date == as.Date("2015-3-2")| RawData$Date == as.Date("2015-4-1")| RawData$Date == as.Date("2015-5-1")| RawData$Date == as.Date("2015-6-1")| RawData$Date == as.Date("2015-7-1")| RawData$Date == as.Date("2015-8-3")| RawData$Date == as.Date("2015-9-1")| RawData$Date == as.Date("2015-10-1")| RawData$Date == as.Date("2015-11-3")| RawData$Date == as.Date("2015-12-1")| RawData$Date == as.Date("2016-1-1")| RawData$Date == as.Date("2016-2-1")| RawData$Date == as.Date("2016-3-1")| RawData$Date == as.Date("2016-4-1")| RawData$Date == as.Date("2016-5-2")| RawData$Date == as.Date("2016-6-1")| RawData$Date == as.Date("2016-7-1")| RawData$Date == as.Date("2016-8-1")| RawData$Date == as.Date("2016-9-1")| RawData$Date == as.Date("2016-10-3")| RawData$Date == as.Date("2016-11-1")| RawData$Date == as.Date("2016-12-1"))
  #2,align maturity
  #TODO
  # data- reorganized data: lineárisan interpolalni kell, hogy legyen 30 napos, 60 napos  --> composit termek

#3.step: dimension reduction for each day using Diebold-Li, 2006

fitted_model <- dimRed(RawData,0.0609,c(1:24))

#4.step: calcualte residual return and actual return

  #1, Calculate returnKiszámolni naponta/havonta, hogy 23 lejáratra mekkora a valós hozam. ( cl2/cl1, cl3/cl1, stb.)

actualReturn <- returnCalc(RawData,2)
    #Ezt hónapokra  megcsinálni, most naponta számolja ki, mintha 1 hónap telt volna el.
    #loghozam?

  #2, Every data point calculate Deibold Li prices

residualReturn <- returnCalc(fitted_model,1)

  #3, Trading strategy: choose among the maturities, which one is the maximum to buy and the minimum to sell in every month

maxResidualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
maxActualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
minResidualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
minActualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
for ( i in 1:17)
{
  maxResidualReturns[i]<-which.max(residualReturn[i,])#indexelés ellenõrzése ez melyik CL-re vonatkozik?
  minResidualReturns[i]<-which.min(residualReturn[i,]) 
  maxActualReturns[i] <- which.max(actualReturn[i,])
  minActualReturns[i] <- which.min(actualReturn[i,])
}


  #4, Asuming 6 months holding periods, calculate the return based on selecting products with Diebold-Li method

    # for Long positions:

expostLongResidualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
expostLongActualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
    # for Short positions:

expostShortResidualReturns <- data.frame(matrix(ncol = 17, nrow = 1))
expostShortActualReturns <- data.frame(matrix(ncol = 17, nrow = 1))

  
for (i in 1: 17)
  {
    expostLongResidualReturns[i] <- RawData[i+6,maxResidualReturns[1,i]+1]/RawData[i,maxResidualReturns[1,i]+1]-1
  #5a, Calculate the return of LONG in 6 months 
    expostLongActualReturns[i] <- RawData[i+6,maxActualReturns[1,i]+1]/RawData[i,maxActualReturns[1,i]+1]-1
    
}
for (i in 1: 17)
{
  expostShortResidualReturns[i] <- -RawData[i+6,maxResidualReturns[1,i]+1]/RawData[i,maxResidualReturns[1,i]+1]-1
  #5b, Calculate the return of SHORT in 6 months  
  expostShortActualReturns[i] <- -RawData[i+6,maxActualReturns[1,i]+1]/RawData[i,maxActualReturns[1,i]+1]-1
  
}



  #6, Which one has higher returns?
LongResults <- data.frame(matrix(ncol = 17, nrow = 1))
for (i in 1:17)
{
  if (expostLongActualReturns[1,i]<=expostLongResidualReturns[1,i])
  {
    LongResults[i]=TRUE    
  }
  else
  {
    LongResults[i]=FALSE
  }
}
ShortResults <- data.frame(matrix(ncol = 17, nrow = 1))
for (i in 1:17)
{
  if (expostShortActualReturns[1,i]<=expostShortResidualReturns[1,i])
  {
    ShortResults[i]=TRUE    
  }
  else
  {
    ShortResults[i]=FALSE
  }
}






