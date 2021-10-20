#First Mini Project 

getwd()
setwd("C:/Users/ACER/Desktop/specdata") #Setting the new directory into specdata 

#Problem No. 1
#Calculates the mean of the pollutant 
pollutantMean <- function(directory, pollutant, id = 1:332){
      list <- list.files(path = directory, pattern = ".csv") 
      x <- numeric() 
      
      for(i in id){ 
        data <- read.csv(list[i])  
        x <- c(x, data[[pollutant]])  
      }
      mean(x, na.rm = T) 
}

#Here are some examples to test the output
pollutantMean("C:/Users/ACER/Desktop/specdata/", "sulfate", 1:10) 
pollutantMean("C:/Users/ACER/Desktop/specdata/", "nitrate", 70:72) 
pollutantMean("C:/Users/ACER/Desktop/specdata/", "nitrate", 23) 


#Problem No. 2
#To create a function that yields the name of the file in the first column and the number of complete cases in the second column
complete <- function(directory, id = 1:332){
      my.list <- list.files(path = directory, pattern = ".csv")
      nobs <- numeric()
      
      for(i in id){
        my.data <- read.csv(my.list[i])
        sum <- sum(complete.cases(my.data))
        nobs <- c(nobs, sum)
      }
      data.frame(id, nobs)
}

#Here are some examples to test the output
complete("C:/Users/ACER/Desktop/specdata/", 1) 
complete("C:/Users/ACER/Desktop/specdata/", c(2, 4, 8, 10, 12))
complete("C:/Users/ACER/Desktop/specdata/", 30:25)
complete("C:/Users/ACER/Desktop/specdata/", 3)


#Problem No. 3
#To calculate the correlation between sulfate and nitrate
corr <- function(directory, threshold = 0){
  mylist <- list.files(path = directory, pattern = ".csv")
  d.f <- complete(directory)
  id <- d.f[d.f["nobs"] > threshold, ]$id
  corrr <- numeric()
  
  for(i in id){
    mydata <- read.csv(mylist[i])
    dff <- mydata[complete.cases(mydata), ]
    corrr <- c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}

#Here are some examples to test the output
cr <- corr("C:/Users/ACER/Desktop/specdata/", 150) 
head(cr); summary(cr)

cr <- corr("C:/Users/ACER/Desktop/specdata/", 400) 
head(cr); summary(cr)

cr <- corr("C:/Users/ACER/Desktop/specdata/", 5000) 
head(cr); summary(cr); length(cr)

cr <- corr("C:/Users/ACER/Desktop/specdata/") 
head(cr); summary(cr); length(cr)


#Problem No. 4
#Creating a plot of the 30-day mortality rates for heart attack

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)
MortR <- as.numeric(outcome[, 11])

hist(MortR,
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths",
     col = "lightblue"
)