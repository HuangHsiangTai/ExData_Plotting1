plot2 <- function() {
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
  par(mar = c(4,4,3,2))
  #plot 
  plot(as.numeric(data$Time), data$Global_active_power,type = "s", xlab="", 
       ylab = "Global Active Power(kilowatts)",xaxt = "n")
  #set the x axis tick
  Sys.setlocale("LC_TIME", "US") 
  x.time <- rep(data$Time[1], 3)
  x.time[2]$mday <- x.time[2]$mday + 1
  x.time[3]$mday <- x.time[3]$mday + 2
  axis(1, as.numeric(x.time), weekdays(as.POSIXct(x.time), TRUE))
  ## Copy plot to a PNG file
  dev.copy(png, file = "plot2.png")
  dev.off()
}