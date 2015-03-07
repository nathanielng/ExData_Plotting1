get.file <- function(dest, fileUrl) {
  if (!file.exists(dest)) {
    zipfile <- "household_power_consumption.zip"
    if (!file.exists(zipfile)) {
      download.file(fileUrl, zipfile, method="curl")
    }
    unzip(zipfile)
    if (file.exists(dest)) {
      rm(zipfile,fileUrl)
    }
  }
}

read.file.by.blocks <- function(file, rblock = 500000, rmax = 2100000) {
  rstart <- 0
  all.data <- data.frame()
  has.header <- TRUE
  data0 <- read.table(file, header=TRUE, sep=";", stringsAsFactors=FALSE,
                      na.strings="?", skip = 0, nrow = 10)
  while (rstart < rmax) {
    data <- read.table(file, header=has.header, sep=";", stringsAsFactors=FALSE,
                       na.strings="?", skip = rstart, nrow = rblock,
                       colClasses = c(rep("character",2), rep("numeric",7)))
    names(data) <- names(data0)
    data <- data[(data[,1] == "1/2/2007" | data[,1] == "2/2/2007"),]
    if (nrow(data) > 0) {
      all.data <- rbind(all.data, data)
    }
    if (has.header) {
      has.header <- FALSE   #when you change the header to FALSE
      rstart <- rstart + 1  #rstart needs to be increased by one
    }
    rstart = rstart + rblock
  }
  all.data
}

file = "household_power_consumption.txt"
get.file(file, "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
#data  <- read.file.by.blocks(file) #reading by blocks takes 70 seconds, but the next one takes 20 seconds!
data0 <- read.table(file, header=TRUE, sep=";", stringsAsFactors=FALSE,
                    na.strings="?", skip = 0, nrow = 2100000,
                    colClasses = c(rep("character",2), rep("numeric",7)))

Sys.setlocale("LC_TIME", "en_GB")
data <- data0[(data0[,1] == "1/2/2007" | data0[,1] == "2/2/2007"),]
data[, 2] <- as.POSIXct(strptime(paste(data[, 1], data[, 2]), "%d/%m/%Y %T"))
data[, 3:9]  <- sapply(data[, 3:9], as.numeric)
png(filename = "plot2.png", width = 480, height = 480)
with(data, plot(Time, Global_active_power, type="l",
                xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()
