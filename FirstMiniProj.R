#CMSC197
#FirstMiniProject

#Item no. 1
unzip("rprog_data_specdata.zip", exdir = "specdata")           #unzipping of csv data and creation of specdata directory


pollutantmean <- function(directory, pollutant, id = 1:332){
  allfiles <- list.files(directory, full.names = TRUE)        #creates a list of files
  df <- data.frame()                                          #creates an empty data frame
    
  for (i in 1:332) {
      df <- rbind(df, read.csv(allfiles[i]))                  #loops through the files rbinding them together
    }
 
   mean(df[, pollutant], na.rm = TRUE)                        #computes for the mean           
}

pollutantmean("specdata", "sulfate", 1:10)                    #sample code
   

#Item no. 2
complete <- function(directory, id = 1:332){
  files <- list.files(directory, full.names = TRUE)           #creates a list of files
  df <- data.frame()                                          #creates an empty data frame
  
  for(i in id){
    tempo <- read.csv(files[i])                               #loops through the files and storing to variable tempo
    nobs <- sum(complete.cases(tempo))                        #adding all the observed cases in each data file and storing it to variable nobs
    df_1 <- data.frame(i, nobs)                               #creating a data frame
    df <- rbind(df, df_1)                                     #rbinding df and df_1 
  }
  colnames(df) <- c("id", "nobs")                             #renaming the column names
  df
}

complete("specdata", 1)                                      #sample code


#Item no. 3
corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)          #creates a list of files
  vec <- vector(mode = "numeric", length = 0)                #creates an empty numeric vector
  
  for (i in 1:length(files)) {
    tempo <- read.csv(files[i])                              #loops through the files and storing to the variable tempo
    add <- sum((!is.na(tempo$sulfate)) & (!is.na(tempo$nitrate))) #adding values under sulfate and nitrate columns without including NA
    if (add > threshold) {                                   #if statement
      tmp <- tempo[which(!is.na(tempo$sulfate)), ]           #storing files under sulfate column from tempo variable excluding NA  
      tempo1 <- tmp[which(!is.na(tmp$nitrate)), ]            #storing files under nitrate column from tmp variable excluding NA
      vec <- c(vec, cor(tempo1$sulfate, tempo1$nitrate))     #concatenate vector vec and sulfate and nitrate columns
    }
  }
  
  vec                                                         #returns vector
}

cr <- corr("specdata", 150)                                   #sample code
head(cr) 
summary(cr)


#Item no. 4
unzip("rprog_data_ProgHospData.zip", exdir = "hospdata")     #unzipping zip file and creating hospdata local directory
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #reading the csv file and storing it to variable outcome and specifying column classes as character
outcome[, 11] <- as.numeric(outcome[, 11])                   #returns the 11th column of the csv file into a numeric value vector

hist(outcome[, 11],xlab="30-day death rates from heart attack",  #renaming the x axis label
     main ="Histogram of the 30-day death rates from heart attack", #renaming the main title of the histogram       
     col = "lightblue")

hist(outcome[,11])                                          #sample code


