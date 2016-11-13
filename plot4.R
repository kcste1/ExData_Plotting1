#this script produces plot4.png as per the specification for the week 1 course  
#project for the Exploratory Data Analysis course on coursera.org

data <- getData()
#note: png produces an image 480x480 pixels as a default
png("plot4.png")
par(mfcol=c(2,2), mar=c(4,4,2,1))
plot(Global_active_power~datetime, data=data, type="l", xlab="", 
     ylab="Global Active Power")
plot(Sub_metering_1~datetime, data=data, type="l", xlab="", 
     ylab="Energy sub metering")
lines(Sub_metering_2~datetime, data=data, type="l", col="red")
lines(Sub_metering_3~datetime, data=data, type="l", col="blue")
legend("topright", lty = 1, col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       bty="n")
plot(Voltage~datetime, data=data, type="l")
plot(Global_reactive_power~datetime, data=data, type="l")
dev.off()

###############################################################################
# Function Name: getData
# 
# Parameters:
#    -nil
# Returns:
#    -data: a dataframe containing power consumption data for 1-Feb-2007  
#           and 2-Feb-2007
#
# Desc: The function assumes that the power consumption file is stored in 
#       the working directory and is called household_power_consumption.txt.
#       It also assumes the file is sorted ascending by date and time and 
#       that there is one row per minute per day (this is how the file was 
#       structured as at 13 Nov 2016)  
#       
#       The function produces a dataframe containing the power consumption data   
#       for 1-Feb-2007 and 2-Feb-2007. It does this by calculating the number of
#       rows to skip from the first date and time (16-Dec-2006 17:24:00) 
#       to 1-Feb-2007, then selecting the rows for 1-Feb-2007 and 
#       2-Feb-2007.
#
#       The function is the same for plot1.R, plot2.R, plot3.R and plot4.R.
###############################################################################
getData <- function() {
    fName <- "household_power_consumption.txt"
    #get the column names for the dataset
    colNm <- read.table(fName, sep=";", nrows=1, stringsAsFactors=FALSE)
    firstDateTime <- as.POSIXct("2006-12-16 17:24:00")
    fromDateTime <- as.POSIXct("2007-02-01 00:00:00")
    toDateTime <- as.POSIXct("2007-02-03 00:00:00")
    #calc the rows (minutes) in the file for 1-Feb + 2-Feb
    nrows <- difftime(toDateTime, fromDateTime, units="mins")
    #calc the rows to skip from the start of the file, which should be the 
    #number of minutes from the first date to 1-Feb-2007 plus 1 for header row
    skip <- difftime(fromDateTime, firstDateTime, units="mins") + 1
    data <- read.table(fName, sep=";", col.names=colNm, nrows=nrows, skip=skip)
    #create a new column in the dataframe that concatenates the Date and Time
    data <- within(data, datetime <- as.POSIXct(paste(Date, Time), 
                                                format="%d/%m/%Y %H:%M:%S"))
}