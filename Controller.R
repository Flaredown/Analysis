# refreshenvironment


# Set working directory ---------------------------------------------------
setwd("/Users/cassandrabayer/Desktop/R Scripts/Flaredown-Analysis")


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


# Load data  --------------------------------------------------------------
# Inital loading and basic cleaning
fd <- data.table(read.csv("fd-export 3.csv", stringsAsFactors = F))
fd[, trackable_name := tolower(trackable_name)]


