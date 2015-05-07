plot1 <- function() {
  #load the data
  data <- read.csv2("household_power_consumption.txt",
                    colClasses = rep("character",9))
  #subset the data
  data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
  #convert varialbe to Time,Date, numeric class
  data$Time <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date,"%d/%m/%Y")
  data[,3:9] <- sapply(data[,3:9],as.numeric)
  #plot histogram
  par(mfrow = c(1,1))
  par(mar = c(4,4,3,2))
  hist(data$Global_active_power,col = "red", 
       main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
  ## Copy my plot to a PNG file
  dev.copy(png, file = "plot1.png")
  dev.off()
}