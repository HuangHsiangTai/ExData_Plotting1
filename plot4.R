plot4 <- function() {
  #load the data
  data <- read.csv2("household_power_consumption.txt",
                    colClasses = rep("character",9))
  #subset the data
  data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
  #convert varialbe to Time,Date, numeric class
  data$Time <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date,"%d/%m/%Y")
  data[,3:9] <- sapply(data[,3:9],as.numeric)
  
  par(mfrow = c(2,2))
  Sys.setlocale("LC_TIME", "US")
  x.time <- rep(data$Time[1], 3)
  x.time[2]$mday <- x.time[2]$mday + 1
  x.time[3]$mday <- x.time[3]$mday + 2
  #plot (1,1) figure
  plot(as.numeric(data$Time), data$Global_active_power,type = "s", xlab="", 
       ylab = "Global Active Power",xaxt = "n") 
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
   
  #plot (1,2) figure
  plot(as.numeric(data$Time), data$Voltage,type = "s", xlab="datetime ", 
       ylab = "Voltage",xaxt = "n") 
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
  
  #plot (2,1) figure
  plot(as.numeric(data$Time), data$Sub_metering_1, type = "s", xlab="", 
       ylab = "Energy sub metering",xaxt = "n", )
  lines(as.numeric(data$Time), data$Sub_metering_2, type = "s", col = "red")
  lines(as.numeric(data$Time), data$Sub_metering_3, type = "s", col = "blue")
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
  legend("topright", lty=c(1,1), # gives the legend appropriate symbols (lines)
         lwd=c(1,1), col = c("black", "blue", "red"), bty = "n",
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
        cex = 0.75)
  
  #plot (2,2) figure
  plot(as.numeric(data$Time), data$Global_reactive_power,type = "s", 
       xlab="datetime", ylab = "Global_reactive_power",xaxt = "n") 
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
  ## Copy plot to a PNG file
  dev.copy(png, file = "plot4.png")
  dev.off()
  
}  