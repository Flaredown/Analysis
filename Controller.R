setwd("/Users/cassandrabayer/Desktop/R Scripts/Flaredown-Analysis")
library(data.table)
library(tidyr)
library(ggplot2)
library(stats)

# Model Selection
library(MASS)
library(glmnet)
library(car)

#Dates
library(zoo)

# Functions
# Functions    
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