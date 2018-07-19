# refreshenvironment

# Set working directory ---------------------------------------------------
setwd("/Users/cassandrabayer/Desktop/attendancePredictor")

# Load Packages -----------------------------------------------------------
# load basic packages
library(data.table)
library(tidyr)
library(tidyverse)

# visualization packages
library(ggplot2)
library(plotly)

# basic stats and prediction
library(stats)
library(forecast)

# Model Selection
library(MASS)
library(glmnet)
library(car)

#Dates
library(zoo)
library(lubridate)

# Text analysis
library("RSiteCatalyst")
library("RTextTools")

# spell check
sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }
library(qdap)
#devtools::install_github("ropensci/spelling")

## spatial
library(rgdal)
library(maptools)
library(ggmap)

# Load any custom functions -----------------------------------------------
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

shift <- function(x, offset = 1, pad = NA) {
  r <- (1 + offset):(length(x) + offset)
  r[r<1] <- NA
  ans <- x[r]
  ans[is.na(ans)] <- pad
  return(ans)
}

maxMissing <- function(x){
  if(all(is.na(x))){
    return(NA_real_)
  } else{
    return(max(x, na.rm = T))
  }
}

nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
