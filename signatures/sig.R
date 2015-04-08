###############################################################################
#                                                                             #
#  City Lab Finance project                                                   #
#  Vendor signatures                                                          #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  April 8, 2015                                                              #
#                                                                             #
###############################################################################

library(data.table)
library(ggplot2)
library(plyr)

# User must set working directory
setwd("~/Google Drive/Grad school/Courses/City Lab/cl-fin-fraud")
source("data_exploratory/data_clean.R")

#######################
#                     #
#  PAYMENT SIGNATURE  #
#                     #
#######################

sigs.pmt <- ddply(as.data.frame(pmt.recent), "vendor.name", function(x) {
  mean.amt <- mean(x$pmt.amt, na.rm=T) # Mean payment
  med.amt <- median(x$pmt.amt, na.rm=T) # Median payment
  q25.amt <- quantile(x$pmt.amt, .25) # Lower quartile payment
  q75.amt <- quantile(x$pmt.amt, .75) # Upper quartile payment
  sd.amt <- sd(x$pmt.amt, na.rm=T) # Standard deviation of payment
  cnt.tot <- length(x$pmt.amt) # Number of payments
  cnt.jan <- length(x$pmt.amt[x$month == "01"]) # Avg number of Jan payments
  cnt.feb <- length(x$pmt.amt[x$month == "02"]) # Avg number of Feb payments
  cnt.mar <- length(x$pmt.amt[x$month == "03"]) # Avg number of Mar payments
  cnt.apr <- length(x$pmt.amt[x$month == "04"]) # Avg number of Apr payments
  cnt.may <- length(x$pmt.amt[x$month == "05"]) # Avg number of May payments
  cnt.jun <- length(x$pmt.amt[x$month == "06"]) # Avg number of Jun payments
  cnt.jul <- length(x$pmt.amt[x$month == "07"]) # Avg number of Jul payments
  cnt.aug <- length(x$pmt.amt[x$month == "08"]) # Avg number of Aug payments
  cnt.sep <- length(x$pmt.amt[x$month == "09"]) # Avg number of Sep payments
  cnt.oct <- length(x$pmt.amt[x$month == "10"]) # Avg number of Oct payments
  cnt.nov <- length(x$pmt.amt[x$month == "11"]) # Avg number of Nov payments
  cnt.dec <- length(x$pmt.amt[x$month == "12"]) # Avg number of Dec payments
  cnt.con <- length(x$pmt.amt[x$pmt.type == "Contract"]) # Contract payments
  cnt.dv <- length(x$pmt.amt[x$pmt.type == "Direct voucher"]) # DV payments
  data.frame(mean.amt, med.amt, q25.amt, q75.amt, sd.amt, cnt.tot, cnt.jan, 
             cnt.feb, cnt.mar, cnt.apr, cnt.may, cnt.jun, cnt.jul, cnt.aug, 
             cnt.sep, cnt.oct, cnt.nov, cnt.dec, cnt.con, cnt.dv)
})

########################
#                      #
#  CONTRACT SIGNATURE  #
#                      #
########################

sigs.cntrct <- ddply(as.data.frame(contract), "vendor.name", function(x) {
  mean.amt <- mean(x$cntrct.amt, na.rm=T) # Mean contract amount
  med.amt <- median(x$cntrct.amt, na.rm=T) # Median contract amount
  sd.amt <- sd(x$cntrct.amt, na.rm=T) # Standard deviation of contract amount
  unique.cnt.cntrct <- length(unique(x$cntrct.num)) # Unique contract count
  most.common.type <- find.most.common(x, "cntrct.type") # Most common type
  most.common.proc <- find.most.common(x, "proc.type") # Most common proc
  mean.length <- mean(x$cntrct.lngth, na.rm=T) # Mean contract length
  data.frame(mean.amt, med.amt, sd.amt, unique.cnt.cntrct, most.common.type,
             most.common.proc, mean.length)
})

#########################
#                       #
#  AUXILIARY FUNCTIONS  #
#                       #
#########################

find.most.common <- function(df, col.name) {
  X <- df[col.name]
  tab <- table(X)
  most <- names(tab[tab == max(tab)])
  # If there is a tie across categories, return NA
  if (length(most) > 1) {
    return(NA)
  }
  return(most)
}
