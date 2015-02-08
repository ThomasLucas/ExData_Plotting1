############################################################################
#               Exploratory Data Analysis - Course Project 1               #
#                         Plot4 - Multiple Graphs                          #
#                             Thomas LUCAS                                 #
############################################################################

# Function which loads and cleans (formatting and subsetting) data
loadingAndSubsettingData <- function(){
  # Setting local system in English to display English weekdays in graphs
  Sys.setlocale("LC_TIME", "en_US")
  
  # Loading data
  if(!(file.exists("household_power_consumption.txt"))){
    stop("household_power_consumption.txt does not exist in your working directory, you need to download the data before running this script")
  }
  elecPowerData <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")
  
  # Convert Time column into Date/Time class
  elecPowerDataDateTime <- paste(elecPowerData$Date, elecPowerData$Time)
  elecPowerDataDateTime <- strptime(elecPowerDataDateTime, "%d/%m/%Y %H:%M:%S") 
  elecPowerData$Time <- elecPowerDataDateTime
  
  # Convert Date column into Date class
  elecPowerData$Date <- as.Date(elecPowerData$Date, format = "%d/%m/%Y")
  
  # Subsetting
  subset <- elecPowerData$Date == "2007-02-01" 
  elecPowerDataSub1 <- elecPowerData[subset, ]
  subset <- elecPowerData$Date == "2007-02-02"
  elecPowerDataSub2 <- elecPowerData[subset, ]
  elecPowerDataSub <- rbind(elecPowerDataSub1, elecPowerDataSub2)
  
  return(elecPowerDataSub)
}

# Function which generates the first graph (top left corner)
# Similar code in "plot2.R" except the ylab value
createFirstGraph <- function(elecPowerDataSub){
  with(elecPowerDataSub, plot(Time, Global_active_power, type="n", xlab = "", ylab = "Global Active Power"))
  with(elecPowerDataSub, lines(Time, Global_active_power))
}

# Function which generates the second graph (top right corner)
createSecondGraph <- function(elecPowerDataSub){
  with(elecPowerDataSub, plot(Time, Voltage, type="n", xlab = "datetime", ylab = "Voltage"))
  with(elecPowerDataSub, lines(Time, Voltage))
}

# Function which generates the third graph (bottom left corner)
# Similar code in "plot3.R"
createThirdGraph <- function(elecPowerDataSub){
  with(elecPowerDataSub, plot(Time, Sub_metering_1, type="n", xlab = "", ylab = "Energy sub metering"))
  with(elecPowerDataSub, lines(Time, Sub_metering_1))
  with(elecPowerDataSub, lines(Time, Sub_metering_2, col = "red"))
  with(elecPowerDataSub, lines(Time, Sub_metering_3, col = "blue"))
  legend("topright", lwd = 3, col = c("black", "red", "blue" ), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

# Function which generates the fourth graph (bottom right corner)
createFourthGraph <- function(elecPowerDataSub){
  with(elecPowerDataSub, plot(Time, Global_reactive_power, type="n", xlab = "datetime"))
  with(elecPowerDataSub, lines(Time, Global_reactive_power))
}

# Function which generates the file plot4.png
generatePlot4 <- function(elecPowerDataSub){
  # Open PNG device, create "plot4.png" in the working directory
  png(file = "plot4.png", width = 480, height = 480, bg = "transparent")
  
  # Use of par with mfrow = c(2,2) to display 2 graphs on the first row and 2 graphs on the second row
  par(mfrow = c(2, 2))
  
  # Call of the different functions to create each graph
  createFirstGraph(elecPowerDataSub)
  createSecondGraph(elecPowerDataSub)
  createThirdGraph(elecPowerDataSub)
  createFourthGraph(elecPowerDataSub)
  
  # Close the PNG file device
  dev.off()
} 


#### MAIN ####

# Data loading
elecPowerDataSub <- loadingAndSubsettingData()

# Plot generation
generatePlot4(elecPowerDataSub)  
