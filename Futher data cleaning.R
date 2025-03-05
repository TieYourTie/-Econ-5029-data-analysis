############################################################
#Econ 5029
#note: the code in this file has the content of trying different method of the
#re-calcuate the housing-supply-elastic


############################################################
rm(list = ls())
#make sure everthing will be fine.)

#Step one Lode the all package that necessary. 
library (lubridate)    
library (mFilter)      
library (neverhpfilter)
library (tsbox)
library (RColorBrewer) #so sad they do not have colorful black 
library(plotly)
library(wesanderson)
library(writexl)
library(tidyverse)
library(readr)
############################################################
#1.Date processing
  #1.1. stationary the data
  #1.2. stationary check
    #1.2.1 ADF test 
    #1.2.2 union root test 
#2. the following method will be used in the code file 
  #2.1. De-trend log value
  #2.2. Log-difference regression
  #2.3. autoregressive distributed lag (ARDL) model
############################################################








