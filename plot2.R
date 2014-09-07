
plot2 <- function()
{
        
        library(timeDate)
        library(dplyr)
        library(lubridate)
        library(datasets)
        
        Data <- read.csv("household_power_consumption.csv",
                         header=TRUE, sep=";")
        #------- Convert Date ------------------------
        Data$Date <- dmy(as.character(Data$Date))
        
        #--------Make the date tresholds --------------------
        min <-dmy("01/02/2007")
        max <- dmy("02/02/2007")
        #-------------Select Date between the two tresholds -----------
        Data2 <- select(Data,Date,Time,Global_active_power,
                        Global_reactive_power,Voltage,Global_intensity,
                        Sub_metering_1,Sub_metering_2,Sub_metering_3)%.%
                filter(Date  ==min |Date==max)
        #-------------------------convert factors to a character-------------------------------
        Data2$Global_active_power <- as.character(Data2$Global_active_power )
        Data2$Sub_metering_1 <-as.character(Data2$Sub_metering_1)
        Data2$Sub_metering_2 <-as.character(Data2$Sub_metering_2)
        Data2$Sub_metering_3 <-as.character(Data2$Sub_metering_3)
        #-------------Replace NAs-------------------------------------------------------------
        replace(Data2$Global_active_power, Data2$Global_active_power  == "?", NA)
        replace(Data2$Sub_metering_1, Data2$Sub_metering_1  == "?", NA)
        replace(Data2$Sub_metering_2, Data2$Sub_metering_2  == "?", NA)
        replace(Data2$Sub_metering_3, Data2$Sub_metering_3  == "?", NA)
        #-------------------------convert to numeric-----------------------------------------
        Data2$Global_active_power <- as.numeric(Data2$Global_active_power)
        Data2$Sub_metering_1 <- as.numeric(as.character(Data2$Sub_metering_1))
        Data2$Sub_metering_2 <- as.numeric(as.character(Data2$Sub_metering_2))
        Data2$Sub_metering_3 <- as.numeric(as.character(Data2$Sub_metering_3))
        #------------2nd plt----------------------------------------------------------------
        Data2$Datetime <- do.call(paste0, Data2[,c(1, 2)])
        Data2$Datetime<- ymd_hms(Data2$Datetime)
        Data2 <- Data2[complete.cases(Data2),] 
        png("plot2.png",480,480)
        plot(x=Data2$Datetime,y=Data2$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab="")
        dev.off()
    
}
