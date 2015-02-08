############################################################################
#               Exploratory Data Analysis - Course Project 1               #
#                       Plot3 - Multiple Line Plots                        #
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

# Function which generates the file plot3.png
generatePlot3 <- function(elecPowerDataSub){
  # Open PNG device, create "plot3.png" in the working directory
  png(file = "plot3.png", width = 480, height = 480, bg = "transparent")
  
  # Create plot and send to a file
  # type = "n" enables to not display the points
  with(elecPowerDataSub, plot(Time, Sub_metering_1, type="n", xlab = "", ylab = "Energy sub metering"))
  
  # Create a line to join graph points for each different series and choose an appropriate color
  with(elecPowerDataSub, lines(Time, Sub_metering_1))
  with(elecPowerDataSub, lines(Time, Sub_metering_2, col = "red"))
  with(elecPowerDataSub, lines(Time, Sub_metering_3, col = "blue"))
  
  # Create a legend in the top right corner
  legend("topright", lwd = 3, col = c("black", "red", "blue" ), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  # Close the PNG file device
  dev.off()
} 


#### MAIN ####

# Data loading
elecPowerDataSub <- loadingAndSubsettingData()

# Plot generation
generatePlot3(elecPowerDataSub)  
