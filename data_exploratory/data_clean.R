###############################################################################
#                                                                             #
#  City Lab Finance project                                                   #
#  Data cleaning                                                              #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  April 5, 2015                                                              #
#                                                                             #
###############################################################################

library(data.table)
library(reshape)

##################
#                #
#  PAYMENT DATA  #
#                #
##################

# https://data.cityofchicago.org/Administration-Finance/Payments/s4vu-giwb

# User must set working directory
setwd("~/Google Drive/Grad school/Courses/City Lab/cl-fin-fraud/data_exploratory")
payment <- fread('Payments.csv', header=T, stringsAsFactors = F)

################
#  CLEAN DATA  #
################

nrow(payment)
length(unique(payment$"VENDOR NAME"))

# Rename columns
payment <- rename(payment, 
                  c("VOUCHER NUMBER" = "dv.num",
                    "AMOUNT" = "pmt.amt",
                    "Check Date" = "pmt.date",
                    "DEPARTMENT NAME" = "dept.name",
                    "CONTRACT NUMBER" = "cntrct.num",
                    "VENDOR NAME" = "vendor.name"))

# Assign correct types to data
amt <- substr(payment$pmt.amt, 2, nchar(payment$pmt.amt)) # Remove '$'
payment$pmt.amt <- as.numeric(amt) # Convert to numeric

payment <- within(payment, { # Separate check date into columns
  month <- ifelse(nchar(pmt.date) == 4, NA, substr(pmt.date, 1, 2))
  day <- ifelse(nchar(pmt.date) == 4, NA, substr(pmt.date, 4, 5))
  year <- ifelse(nchar(pmt.date) == 4, pmt.date, substr(pmt.date, 7, 10))
})

payment$month <- factor(payment$month, # Convert to factor
                        levels = c("01","02","03","04","05","06",
                                   "07","08","09","10","11","12"))

payment$day <- factor(payment$day, # Convert to factor
                      levels = c("01","02","03","04","05","06",
                                 "07","08","09","10","11","12",
                                 "13", "14", "15", "16", "17", "18",
                                 "19", "20", "21", "22", "23", "24",
                                 "25", "26", "27", "28", "29", "30", "31"))

payment$year <- factor(payment$year, # Convert to factor
                       levels = c("2003", "2004", "2005", "2006", "2007",
                                  "2008", "2009", "2010", "2011", "2012", 
                                  "2013", "2014", "2015"))

payment$dept.name <- factor(payment$dept.name) # Convert to factor

payment$vendor.name <- factor(payment$vendor.name) # Convert to factor

# Create boolean for whether payment is for contract or DV
payment$pmt.type <- ifelse(payment$cntrct.num != "DV", 
                           "Contract", "Direct voucher")

table(payment$pmt.date)

# Weird stuff going on with check dates. 1996-2002 data is rolled up and
# dated as 2002. Data that is older than two years is also supposed to be
# rolled up into one annual observation by contract, but this is not
# consistent in the data. Perhaps contracts with only one payment are
# dated by the date of the one payment.

pmt <- subset(payment, payment$year != '2002') # Remove 2002 data

############################
#                          #
#  2013-2015 PAYMENT DATA  #
#                          #
############################

pmt.recent <- subset(pmt, pmt$year == '2014' | pmt$year == '2015')

################
#  CLEAN DATA  #
################

pmt.recent$month.year <- paste(pmt.recent$month, pmt.recent$year, sep='-')
pmt.recent$month.year <- factor(pmt.recent$month.year, 
                                levels=c('01-2014', '02-2014', '03-2014', 
                                         '04-2014', '05-2014', '06-2014', 
                                         '07-2014', '08-2014', '09-2014', 
                                         '10-2014', '11-2014', '12-2014',
                                         '01-2015', '02-2015', '03-2015') )

###########################
#                         #
#  PAYMENT-CONTRACT DATA  #
#                         #
###########################

# Restrict payments data to contract data
pmt.cntrct <- subset(pmt, pmt$pmt.type == "Contract")

# https://data.cityofchicago.org/Administration-Finance/Contracts/rsxa-ify5
contract <- fread('Contracts.csv', header=T, stringsAsFactors=F)

################
#  CLEAN DATA  #
################

contract <- rename(contract, # Rename column headers
                   c("Purchase Order Description" = "cntrct.desc",
                   "Purchase Order (Contract) Number" = "cntrct.num",
                   "Revision Number" = "rev.num",
                   "Specification Number" = "spec.num",
                   "Contract Type" = "cntrct.type",
                   "Start Date" = "cntrct.start.date",
                   "End Date" = "cntrct.end.date",
                   "Approval Date" = "cntrct.app.date",
                   "Department" = "department",
                   "Vendor Name" = "vendor.name",
                   "Vendor ID" = "vendor.id",
                   "Address 1" = "addr.1",
                   "Address 2" = "addr.2",
                   "City" = "city",
                   "State" = "state",
                   "Zip" = "zip",
                   "Award Amount" = "cntrct.amt",
                   "Procurement Type" = "proc.type"))

amt <- substr(contract$cntrct.amt, 2, nchar(contract$cntrct.amt)) # Remove '$'
contract$cntrct.amt <- as.numeric(amt) # Convert to numeric

# Find length of contract
end <- strptime(contract$cntrct.end.date, format="%m/%d/%Y %I:%M:%S %p")
start <- strptime(contract$cntrct.start.date, format="%m/%d/%Y %I:%M:%S %p")
contract$cntrct.lngth <- as.numeric(difftime(end, start, units = "week"))

contract$cntrct.num <- factor(contract$cntrct.num)
contract$cntrct.type <- factor(contract$cntrct.type)
contract$proc.type <- factor(contract$proc.type)

cntrct <- merge(as.data.frame(pmt.cntrct), # Merge with pmt
                 as.data.frame(contract),
                 by = "cntrct.num")

cntrct <- data.table(cntrct)

cntrct.recent <- subset(cntrct, cntrct$year == "2014" | cntrct$year == "2015")

cntrct.recent <- data.table(cntrct.recent)


