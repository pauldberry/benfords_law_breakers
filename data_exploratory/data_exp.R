###############################################################################
#                                                                             #
#  City Lab Finance project                                                   #
#  Data exploration I                                                         #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  April 1, 2015                                                              #
#                                                                             #
###############################################################################

library(data.table)
library(ggplot2)
library(reshape)
# library(RSocrata) Can't get this to work

##################
#                #
#  PAYMENT DATA  #
#                #
##################

# https://data.cityofchicago.org/Administration-Finance/Payments/s4vu-giwb
payment <- read.csv('~/Google Drive/Grad school/Courses/City Lab/data_exploratory/Payments.csv',
                    header=T, stringsAsFactors = F)

################
#  CLEAN DATA  #
################

amt <- substr(payment$AMOUNT, 2, nchar(payment$AMOUNT)) # Remove '$'
payment$AMOUNT <- as.numeric(amt) # Convert to numeric

payment <- within(payment, { # Separate check date into columns
  Month <- ifelse(nchar(Check.Date) == 4, NA, substr(Check.Date, 1, 2))
  Day <- ifelse(nchar(Check.Date) == 4, NA, substr(Check.Date, 4, 5))
  Year <- ifelse(nchar(Check.Date) == 4, Check.Date, substr(Check.Date, 7, 10))
})

table(payment$Check.Date)

# Weird stuff going on with check dates. 1996-2002 data is rolled up and
# dated as 2002. Data that is older than two years is also supposed to be
# rolled up into one annual observation by contract, but this is not
# consistent in the data. Perhaps contracts with only one payment are
# dated by the date of the one payment.

payment.sub <- subset(payment, payment$Year != '2002') # Remove 2002 data

pmt <- data.table(payment.sub)

##################
#  EXPLORE DATA  #
##################

length(pmt$VENDOR.NAME) # Number of observations
length(unique(pmt$VENDOR.NAME)) # Number of unique vendors

# Summary stats by year; cannot compare before/after 2013 due to roll-up
sum.by.year <- aggregate(pmt$AMOUNT, list(pmt$Year), summary)

# Plot: Distribution of payment amounts by year
amt.by.yr.plt <- ggplot(data = pmt, aes(x = Year, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) + # Without outliers
  coord_cartesian(ylim = c(0,750000)) +
  ggtitle("Distribution of payment amount by year, 2003-2015") +
  xlab("Year") +
  ylab("Amount")

# Number of unique contracts by year (DVs excluded)
num.con.by.year <- aggregate(pmt$CONTRACT.NUMBER[pmt$CONTRACT.NUMBER != "DV"],
                             list(pmt$Year[pmt$CONTRACT.NUMBER != "DV"]), 
                             function(x) length(unique(x)))

# Plot: Number of contracts by year plot
num.con.by.yr.plt <- ggplot(num.con.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of unique contracts by year, 2003-2015") +
  xlab("Year") +
  ylab("Count")

# Total contract payments per year (DVs excluded)
tot.con.by.year <- aggregate(pmt$AMOUNT[pmt$CONTRACT.NUMBER != "DV"], 
                             list(pmt$Year[pmt$CONTRACT.NUMBER != "DV"]), 
                             sum, na.rm=T)

# Plot: Total contract payments by year plot
tot.con.by.yr.plt <- ggplot(tot.con.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total contract payments by year, 2003-2015") +
  xlab("Year") +
  ylab("Amount")

# Number of DVs by year
# Question: why did DVs increase so much in 2014?
num.dvs.by.year <- aggregate(pmt$CONTRACT.NUMBER[pmt$CONTRACT.NUMBER == "DV"],
                             list(pmt$Year[pmt$CONTRACT.NUMBER == "DV"]), 
                             length)

# Plot: Number of DVs by year plot
num.dvs.by.yr.plt <- ggplot(num.dvs.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of Direct Vouchers by year, 2003-2015") +
  xlab("Year") +
  ylab("Count")

# Total DV payments per year
tot.dvs.by.year <- aggregate(pmt$AMOUNT[pmt$CONTRACT.NUMBER == "DV"], 
                             list(pmt$Year[pmt$CONTRACT.NUMBER == "DV"]), 
                             sum, na.rm=T)

# Plot: Total DV payments by year plot
tot.dvs.by.yr.plt <- ggplot(tot.dvs.by.year, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total Direct Voucher payments by year, 2003-2015") +
  xlab("Year") +
  ylab("Amount")

############################
#                          #
#  2013-2015 PAYMENT DATA  #
#                          #
############################

pmt.recent <- subset(pmt, pmt$Year == '2014' | pmt$Year == '2015')

################
#  CLEAN DATA  #
################

pmt.recent$Month.Year <- paste(pmt.recent$Month, pmt.recent$Year, sep='-')
pmt.recent$Month.Year <- factor(pmt.recent$Month.Year, 
                                levels=c('01-2014', '02-2014', '03-2014', 
                                         '04-2014', '05-2014', '06-2014', 
                                         '07-2014', '08-2014', '09-2014', 
                                         '10-2014', '11-2014', '12-2014',
                                         '01-2015', '02-2015', '03-2015') )

##################
#  EXPLORE DATA  #
##################

length(pmt.recent$VENDOR.NAME) # Number of payments
length(unique(pmt.recent$VENDOR.NAME)) # Number of unique vendors

summary(pmt.recent$AMOUNT) # Summarize payment amount

# Plot: Distribution of payments by month
amt.by.mo.plt <- ggplot(data = pmt.recent, aes(x = Month.Year, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) + # Without outliers
  coord_cartesian(ylim = c(0,4000)) +
  ggtitle("Distribution of payment amount by month, 2014-2015") +
  xlab("Month") +
  ylab("Amount")

# Number of payments by month (contract and DV)
num.pmt.by.mo <- aggregate(pmt.recent$CONTRACT.NUMBER, 
                           list(pmt.recent$Month.Year), 
                           length) 

# Plot: Number of payments by month (contract and DV)
num.pmt.by.mo.plt <- ggplot(num.pmt.by.mo, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of payments by month, 2014-2015") +
  xlab("Month") +
  ylab("Count")

# Total payments per year (contract and DV)
tot.pmt.by.mo <- aggregate(pmt.recent$AMOUNT, 
                           list(pmt.recent$Month.Year), 
                           sum, na.rm=T)

# Plot: Total payments by year bar plot (contract and DV)
tot.pmt.by.mo.plt <- ggplot(tot.pmt.by.mo, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Total payments by month, 2014-2015") +
  xlab("Month") +
  ylab("Amount")

# Plot: Distribution of payments by day of month (contract and DV)
pmt.by.day.plt <- ggplot(data = pmt.recent, aes(x = Day, y = AMOUNT)) + 
  geom_boxplot(outlier.colour=NA) + # Without outliers
  coord_cartesian(ylim = c(0,15000)) +
  # Might be helpful to add day of week
  ggtitle("Distribution of payment amount by day of month, 2014-2015") +
  xlab("Day of month") +
  ylab("Amount")

# Number of payments by day of month (contract and DV)
num.pmt.by.day.plt <- aggregate(pmt.recent$CONTRACT.NUMBER, 
                                list(pmt.recent$Day), 
                                length)

# Plot: Number of payments by date bar plot
num.date.plt.recent <- ggplot(num.pmt.by.day.plt, aes(x = Group.1, y = x)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Number of payments by day of month, 2014-2015") +
  xlab("Date") +
  ylab("Count")

###########################
#                         #
#  PAYMENT-CONTRACT DATA  #
#                         #
###########################

# Restrict payments data to contract data
payment.contract <- subset(payment.sub, payment.sub$CONTRACT.NUMBER != 'DV')

# https://data.cityofchicago.org/Administration-Finance/Contracts/rsxa-ify5
contract <- read.csv('~/Google Drive/Grad school/Courses/City Lab/data_exploratory/Contracts.csv',
                     header=T, stringsAsFactors = F)

cntrct <- merge(payment.contract, # Merge payments and contracts
                contract[c(1:8,11:18)], 
                by.x = "CONTRACT.NUMBER",
                by.y = "Purchase.Order..Contract..Number")

cntrct <- data.table(cntrct)

################
#  CLEAN DATA  #
################

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

##################
#  EXPLORE DATA  #
##################

# Number of unique contracts by type
num.con.by.type <- aggregate(cntrct$cntrct.num, 
                             list(cntrct$cntrct.type), 
                             function(x) length(unique(x))) 

# Plot: Number of unique contracts by type
num.con.by.type.plt <- ggplot(num.con.by.type, aes(x = Group.1, y = x)) + 
  geom_bar(stat = "identity") +
  ggtitle("Number of unique contracts by type, 2003-2015") +
  xlab("Type") +
  ylab("Count") +
  coord_flip()

# Average contract amount by type
avg.con.by.type <- aggregate(cntrct$cntrct.amt,
                             list(cntrct$cntrct.type), 
                             mean)

# Plot: Average contract amount by type
avg.con.by.type.plt <- ggplot(avg.con.by.type, aes(x = Group.1, y = x)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average contract amount by type, 2003-2015") +
  xlab("Type") +
  ylab("Amount") +
  coord_flip()

# Plot: Contract length in weeks versus amount
con.amt.lngth.plt <- ggplot(cntrct,
                            aes(x = as.numeric(cntrct.lngth), y = cntrct.amt)) +
  geom_point() +
  coord_cartesian(xlim=c(0,2500), ylim=c(0,600000000)) +
  xlab("Length of contract") +
  ylab("Contract amount")

###########################
#                         #
#  INTERESTING QUESTIONS  #
#                         #
###########################

# How many contracts have been paid more than their award?
# Appears multiple contracts have multiple awards of different amounts.
# Should we sum award amounts over contracts?

tot.pmt.by.con <- aggregate(cntrct$pmt.amt,
                            list(cntrct$cntrct.num), 
                            sum)

# Should we sum up awards?
tot.cntrct.amt.by.con <- aggregate(cntrct$cntrct.amt,
                                   list(cntrct$cntrct.num),
                                   sum)

tot.by.con <- merge(tot.pmt.by.con, tot.cntrct.amt.by.con, by="Group.1")

tot.by.con <- rename(tot.by.con, c("Group.1" = "cntrct.num",
                                   "x.x" = "tot.pmt.amt",
                                   "x.y" = 'tot.cntrct.amt'))

tot.by.con$comp <- ifelse(tot.by.con$tot.pmt.amt > tot.by.con$tot.cntrct.amt,
                          "more", "less or equal")

prop.table(table(tot.by.con$comp))

# How many unique vendors have the same address as another vendor?

length(unique(cntrct$vendor.id))
length(unique(cntrct$addr.1))

# There are 3976 unique vendors but only 3811 unique addresses. Explore
# vendors with the same addresses.
