plot3 <- function() {
  #load the data
  data <- read.csv2("household_power_consumption.txt",
                    colClasses = rep("character",9))
  #subset the data
  data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
  #convert varialbe to Time,Date, numeric class
  data$Time <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date,"%d/%m/%Y")
  data[,3:9] <- sapply(data[,3:9],as.numeric)
  #initialize the par
  par(mfrow = c(1,1))
  #plot lines
  plot(as.numeric(data$Time), data$Sub_metering_1, type = "s", xlab="", 
       ylab = "Energy sub metering",xaxt = "n", )
  lines(as.numeric(data$Time), data$Sub_metering_2, type = "s", col = "red")
  lines(as.numeric(data$Time), data$Sub_metering_3, type = "s", col = "blue")
  Sys.setlocale("LC_TIME", "US") 
  #set the x axis tick
  x.time <- rep(data$Time[1], 3)
  x.time[2]$mday <- x.time[2]$mday + 1
  x.time[3]$mday <- x.time[3]$mday + 2
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
  legend("topright", lty=c(1,1), # gives the legend appropriate symbols (lines)
         lwd=c(1,1), col = c("black", "blue", "red"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  ## Copy plot to a PNG file
  dev.copy(png, file = "plot3.png")
  dev.off()
  
}  
  