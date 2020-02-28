#loading data
loadData <- function(){
  wti<-readxl::read_excel("WTI2.xlsx")
  wti$Date = as.Date(wti$Date, format="%m/%d %M:%S")
  return(wti)
}
#filtering database 
prepareDatabase <- function(df,StartDate,EndDate,SelectColumns){
  x <- as.Date(StartDate)
  y <- as.Date(EndDate)
  RawData <- df[df$Date >= x & df$Date <= y,] %>%
    dplyr::as_tibble() %>%
    dplyr::select(SelectColumns)
  
  RawData<- subset(RawData,(weekdays(RawData$Date) %in% c('hétfõ','kedd', 'szerda', 'csütörtök','péntek' ))) 
  #ha angol a felhasznaloi felulet:
  #RawData<- subset(RawData,(weekdays(RawData$Date) %in% c('Monday','Tuesday', 'Wednesday', 'Thursday','Friday' ))) 
  
  return(RawData)
}
#dimension reduction, applying Diebold-Li method
dimRed <- function(data,lambda,maturities){
  x1 <- sapply(maturities,function(q) (1-exp(-lambda*q)) / (lambda*q))
  x2 <- sapply(maturities,function(q) (1-exp(-lambda*q)) / (lambda*q) - exp(-lambda*q))
  datalist = list()
  
  for(i in 1:NROW(data))
  {
    X <- cbind(t(data[i,2:25]),rep(1,length(maturities)),x1,x2)
    colnames(X) <- c("price", "level","slope", "curvature")
    Xdf <-as.data.frame(X)
    fit <- lm(formula = price ~ level+slope+curvature,data = Xdf)
    #plot(t(data[i,2:25]), type="l",col="red", main = data[i,1])
    #lines(fit$fitted.values, col ="green")
    #dat<- as.data.frame(t(t(data[i,2:25])-fit$fitted.values)) --> különbség, error
    dat <- as.data.frame(t(fit$fitted.values))
    datalist[[i]] <- dat
  }
  big_data = do.call(rbind, datalist)
  return(big_data)
}
#calculate returns
returnCalc <- function(data,start){
  results <- data.frame(matrix(ncol = NCOL(data)-2, nrow = NROW(data)))
  for (i in 2:NROW(data)) 
  {
    for(j in 3:NCOL(data))
    {
      y <- data[i,j-1]/data[i-1,j] -1
      results[i-1,j-2]= y
    }
  }
  return(results)
}