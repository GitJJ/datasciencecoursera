rd <- read.csv("specdata/001.csv")

pollutantmean <- function(directory, pollutant, id=1:332){
      first.i <- id[1]
      for (i in id){
            if (i==first.i){
                  all.data <- read.csv(paste0(directory,"/",formatC(i, width=3, flag="0"),".csv"))
            }else{
                  new.data <- read.csv(paste0(directory,"/",formatC(i, width=3, flag="0"),".csv"))
                  all.data <- rbind(all.data, new.data)
            }
      }
      id.poll <- which(names(rd)==pollutant)
      mean.out <- mean(all.data[,id.poll], na.rm = TRUE)
      return(mean.out)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


complete <- function(directory, id=1:332){
      record.data <- data.frame(id=integer(),nobs=integer())
      for(i in seq_along(id)){
            raw.data <- read.csv(paste0(directory,"/",formatC(id[i], width=3, flag="0"),".csv"))
            completeCases.id <- which(complete.cases(raw.data)==TRUE)
            no.CC <- NROW(completeCases.id)
            record.data[i,] <- c(i, no.CC)
      }
      return(record.data)
}


complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

corr <- function(directory, threshold=0){
      #initialise data
      id=1:332
      test.data <- data.frame(Date=factor(), sulfate=numeric(), nitrate=numeric(), id=integer())
      correlation.data <- data.frame(id = integer(), correlation=numeric())
      j <- 0
      for (i in id){
            no.CC <- complete(directory, i)
            if (no.CC$nobs > threshold){
                  new.data <- read.csv(paste0(directory,"/",formatC(i, width=3, flag="0"),".csv"))
                  #test.data <- rbind(test.data, new.data)
                  corr.R <- cor(new.data$sulfate, new.data$nitrate, use = "complete")
                  j <- j+1
                  correlation.data[j,] <- c(i,corr.R)
            }
      }
      return(correlation.data$correlation)
}
directory <- "specdata"
threshold <- 150
corr.R <- corr("specdata", 150)
cr <- corr("specdata", 400)
cr <- corr("specdata", 5000)
cr <- corr("specdata")
head(cr)
summary(cr)

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
