###########################################################################
#                                                                         #
#  City Lab Finance project                                               #
#  Data exploration I                                                     #
#  Coded by Scarlett Swerdlow                                             #
#  scarlettswerdlow@uchicago.edu                                          #
#  March 31, 2015                                                         #
#                                                                         #
###########################################################################

library(data.table)
library(ggplot2)
library(reshape)
library(RSocrata)

# City payment data
# 
payment <- read.socrata('https://data.cityofchicago.org/Administration-Finance/Payments/s4vu-giwb')
#payment <- read.csv('~/Google Drive/Grad school/Courses/City Lab/data_exploratory/Payments.csv',
#                    header=T, stringsAsFactors = F)


table(payment$Check.Date)

# Weird stuff going on with check dates. 1996-2002 data is rolled up and
# dated as 2002. Data that is older than two years is also supposed to be
# rolled up into one annual observation by contract, but this is not
# consistent in the data. Perhaps contracts with only one payment are
# dated by the date of the one payment.

amt <- substr(payment$AMOUNT, 2, nchar(payment$AMOUNT)) # Remove '$'
payment$AMOUNT <- as.numeric(amt) # Convert to numeric

payment <- within(payment, { # Separate check date into columns
  Month <- ifelse(nchar(Check.Date) == 4, NA, substr(Check.Date, 1, 2))
  Day <- ifelse(nchar(Check.Date) == 4, NA, substr(Check.Date, 4, 5))
  Year <- ifelse(nchar(Check.Date) == 4, Check.Date, substr(Check.Date, 7, 10))
})

payment.clean <- subset(payment, payment$Year != '2002') #1996-2002 rolled up as 2002

pmt <- data.table(payment.clean)

length(pmt$VENDOR.NAME) # Number of observations
length(unique(pmt$VENDOR.NAME)) # Number of unique vendors

# Summary payment; cannot compare before/after 2013 due to roll-up
sum.by.year <- aggregate(pmt$AMOUNT, list(pmt$Year), summary)

# Total contract payment amount boxplot
yr.plt <- ggplot(data = pmt, aes(x = Year, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) +
  # Without outliers
  coord_cartesian(ylim = c(0,750000)) +
  ggtitle("Payment amount by contract, 2003-2015") +
  xlab("Year") +
  ylab("Amount")
  
num.by.year <- aggregate(pmt$CONTRACT.NUMBER, list(pmt$Year), 
                         function(x) length(unique(x))) # Contracts per year

# Number of contracts by year plot
num.yr.plt <- ggplot(num.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of unique contracts, 2003-2015") +
  xlab("Year") +
  ylab("Count")

# Total payment per year
tot.by.year <- aggregate(pmt$AMOUNT, list(pmt$Year), sum, na.rm=T)

# Total payments by year plot
tot.yr.plt <- ggplot(tot.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total payments, 2003-2015") +
  xlab("Year") +
  ylab("Amount")

####################
#  2013-2015 DATA  #
####################

# Data between 2002 and 2012 is 'rolled up' by contract

pmt.recent <- subset(pmt, pmt$Year == '2014' | pmt$Year == '2015')
pmt.recent$Month.Year <- paste(pmt.recent$Month, pmt.recent$Year, sep='-')
pmt.recent$Month.Year <- factor(pmt.recent$Month.Year, 
                                levels=c('01-2014', '02-2014', '03-2014', 
                                         '04-2014', '05-2014', '06-2014', 
                                         '07-2014', '08-2014', '09-2014', 
                                         '10-2014', '11-2014', '12-2014',
                                         '01-2015', '02-2015', '03-2015') )

length(pmt.recent$VENDOR.NAME) # Number of observations
length(unique(pmt.recent$VENDOR.NAME)) # Number of unique vendors

summary(pmt.recent$AMOUNT)

# Payment distribution by month boxplot
mo.plt.recent <- ggplot(data = pmt.recent, aes(x = Month.Year, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) +
  # Without outliers
  coord_cartesian(ylim = c(0,4000)) +
  ggtitle("Payment amount by check by month, 2014-2015") +
  xlab("Month") +
  ylab("Amount")

num.by.mo.recent <- aggregate(pmt.recent$CONTRACT.NUMBER, 
                              list(pmt.recent$Month.Year), 
                              length) # Number of payments per month

# Number of payments by month bar plot
num.mo.plt.recent <- ggplot(num.by.mo.recent, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of payments by month, 2014-2015") +
  xlab("Month") +
  ylab("Count")

# Total payment per year
tot.by.mo.recent <- aggregate(pmt.recent$AMOUNT, 
                              list(pmt.recent$Month.Year), 
                              sum, na.rm=T) # Total payment amt by month

# Total payments by year bar plot
tot.mo.plt.recent <- ggplot(tot.by.mo.recent, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total payment amount by month, 2014-2015") +
  xlab("Month") +
  ylab("Amount")

# Payment distribution by date boxplot
day.plt.recent <- ggplot(data = pmt.recent, aes(x = Day, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) +
  # Without outliers
  coord_cartesian(ylim = c(0,15000)) +
  # Might be helpful to add day of week
  ggtitle("Payment amount by check by date, 2014-2015") +
  xlab("Date") +
  ylab("Amount")

num.by.date.recent <- aggregate(pmt.recent$CONTRACT.NUMBER, 
                                list(pmt.recent$Day), 
                                length) # Number of payments per date

# Number of payments by date bar plot
num.date.plt.recent <- ggplot(num.by.date.recent, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of payments by date, 2014-2015") +
  xlab("Date") +
  ylab("Count")

###############
#  CONTRACTS  #
###############

payment.contract <- subset(payment.clean, 
                       payment.clean$CONTRACT.NUMBER != 'DV') #Just contracts

# City contract data
# https://data.cityofchicago.org/Administration-Finance/Contracts/rsxa-ify5
contract <- read.csv('~/Google Drive/Grad school/Courses/City Lab/data_exploratory/Contracts.csv',
                     header=T, stringsAsFactors = F)

cntrct <- merge(payment.contract, # Merge payments and contracts
                contract[c(1:8,11:18)], 
                by.x = "CONTRACT.NUMBER",
                by.y = "Purchase.Order..Contract..Number")

cntrct <- data.table(cntrct)

cntrct <- rename(cntrct, # Rename column headers
                 c("CONTRACT.NUMBER" = "cntrct.num",
                   "VOUCHER.NUMBER" = "voucher.num",
                   "AMOUNT" = "pmt.amt",
                   "Check.Date" = "pmt.date",
                   "DEPARTMENT.NAME" = "dept.name",
                   "VENDOR.NAME" = "vendor.name",
                   "Year" = "pmt.year",
                   "Day" = "pmt.day",
                   "Month" = "pmt.mo",
                   "Purchase.Order.Description" = "cntrct.desc",
                   "Revision.Number" = "rev.num",
                   "Specification.Number" = "spec.num",
                   "Contract.Type" = "cntrct.type",
                   "Start.Date" = "cntrct.start.date",
                   "End.Date" = "cntrct.end.date",
                   "Approval.Date" = "cntrct.app.date",
                   "Vendor.ID" = "vendor.id",
                   "Address.1" = "addr.1",
                   "Address.2" = "addr.2",
                   "City" = "city",
                   "State" = "state",
                   "Zip" = "zip",
                   "Award.Amount" = "cntrct.amt",
                   "Procurement.Type" = "proc.type"))

amt <- substr(cntrct$cntrct.amt, 2, nchar(cntrct$cntrct.amt)) # Remove '$'
cntrct$cntrct.amt <- as.numeric(amt) # Convert to numeric

# Find length of contract
end <- strptime(cntrct$cntrct.end.date, format="%m/%d/%Y %I:%M:%S %p")
start <- strptime(cntrct$cntrct.start.date, format="%m/%d/%Y %I:%M:%S %p")
cntrct$cntrct.lngth <- difftime(end, start, units = "week")

# Remove obs with contract amts or lengths less than 0 
cntrct.clean <- subset(cntrct, cntrct$cntrct.amt >= 0 &
                       cntrct.lngth >= 0)

num.cntrct.type <- aggregate(cntrct.clean$cntrct.num, # Unique contracts by type
                             list(cntrct.clean$cntrct.type), 
                             function(x) length(unique(x))) 

# Number of contracts by type bar plot
cntrct.type.plt <- ggplot(num.cntrct.type, aes(x = Group.1, y = x)) + 
  geom_bar(stat = "identity") +
  ggtitle("Number of unique contracts by type, 2003-2015") +
  xlab("Type") +
  ylab("Count") +
  coord_flip()

avg.cntrct.type <- aggregate(cntrct.clean$cntrct.amt, # Avg contract amt by type
                             list(cntrct.clean$cntrct.type), mean)

# Average contract amount by type bar plot
cntrct.avg.type.plt <- ggplot(avg.cntrct.type, aes(x = Group.1, y = x)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average contract amount by type, 2003-2015") +
  xlab("Type") +
  ylab("Amount") +
  coord_flip()

# Contract amount versus length scatter plot
cntrct.amt.lngth.scat <- ggplot(cntrct.clean,
                                aes(x = as.numeric(cntrct.lngth),
                                    y = cntrct.amt)) +
  geom_point() +
  coord_cartesian(xlim=c(0,2500), ylim=c(0,600000000)) +
  xlab("Length of contract") +
  ylab("Contract amount")





# cntrct.amt.proc.plt <- ggplot(cntrct.clean, aes(cntrct.amt)) +
#   geom_histogram(binwidth=100000) +
#   facet_grid(proc.type ~.)

# Contract type
# Contract amount
# Procurement type
# Contract length
# How many contracts have been paid more than the award amount

# # Contract amount distribution by type, 2003-2015
# cntrct.amt.type.plt <- ggplot(data = cntrct.clean, 
#                               aes(x = reorder(cntrct.type, mean(cntrct.amt)), 
#                                   y = cntrct.amt)) + 
#   geom_boxplot(outlier.colour=NA) +
#   # Without outliers
#   coord_flip(ylim= c(0,1000000)) +
#   # Might be helpful to add day of week
#   ggtitle("Contract amount by type, 2003-2015") +
#   xlab("Type") +
#   ylab("Amount")
# 
# 
# 
# plot(cntrct.clean$cntrct.lngth, cntrct.clean$cntrct.amt)
# 
# 
# 
# 
# cntrct <- merge(pmt.cntrct, cntrct, by = "CONTRACT.NUMBER")
# 
# length(cntrct$VENDOR.NAME) # Number of contract observations
# length(unique(cntrct$VENDOR.NAME)) # Number of unique contract vendors
# 
# 
# 
# amount.by.contract <- aggregate(contract$AMOUNT, 
#                                 list(contract$CONTRACT.NUMBER),
#                                 sum) # Contract amt by contract
# 
# summary(amount.by.contract$x)
# 
# mean(amount.by.contract$x, na.rm = T) # Mean payment amount
# median(amount.by.contract$x, na.rm = T) # Median payment amount
# quant.con <- quantile(amount.by.contract$x, seq(0,1,.1)) # Quantiles
# 
# hist(amount.by.contract$x[amount.by.contract$x < quant.con[6]]) 
# 

########
#  DV  #
########

# Distribution of payments
# Repeated DV to same vendor

