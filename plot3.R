##### PLOT3.R

# Read Data

`datapower` <- read.table("~/doc/data-science/data-science-track/4-ds-exdata/project1/household_power_consumption.txt", sep=";", quote="\"", header=T)

datapower$Date <- as.Date(datapower$Date, "%d/%m/%Y")

Date1 = as.Date("2007-02-01")
Date2 = as.Date("2007-02-03")

#2 Subset
datapower1 = datapower[datapower$Date %in% Date1:Date2, ]

# Create a new column timestamp = Date + Time
datapower2 = within(datapower1, { timestamp=format(as.POSIXct(paste(Date, Time)), "%Y%m%d %H:%M:%S") })

# Place the new (10th) column in the first column
datapower2 = datapower2[,c(10,1:9)]

# Check whether data are clean. Any missing data as ? or NA?
sum(is.na(datapower2))
# 0 so none; how many ?
unique(grep("\\?", datapower2[,1:10], value = TRUE))
# none

# Define a function
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Convert column values to be plotted from factor to numeric
datapower2$Global_active_power=as.numeric.factor(datapower2$Global_active_power)

# Subset again for 2 days + first minute of the third day (Saturday).
datapower2 = datapower2[1:2881, ]

# Convert timestamp values from chr to POSIXct
datapower2$timestamp=as.POSIXct(datapower2$timestamp)


# Convert Sub_metering_1 to 3 from Factor to Numeric
datapower2$Sub_metering_1=as.numeric.factor(datapower2$Sub_metering_1)

datapower2$Sub_metering_2=as.numeric.factor(datapower2$Sub_metering_2)

datapower2$Sub_metering_2=as.numeric.factor(datapower2$Sub_metering_3)


# Create and save Plot 3
png("./plot3.png")

plot(datapower2$timestamp,datapower2$Sub_metering_1,type="l", ylab="Energy sub metering", width = 480, height = 480, units = "px", xlab=" ",ylim=c(0,38));
par(new=TRUE)
lines(datapower2$timestamp,datapower2$Sub_metering_2,col="red", type="l", ylab="Energy sub metering", width = 480, height = 480, units = "px", xlab=" ",ylim=c(0,38));
par(new=TRUE)
plot(datapower2$timestamp,datapower2$Sub_metering_3,col="blue", type="l", ylab="Energy sub metering", width = 480, height = 480, units = "px", xlab=" ",ylim=c(0,38))
legend("topright", lty, lwd = 1, col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
dev.off()
