# R Script for exam digit frequency of data in a given XML file
# Author: zwpwjwtz <zwpwjwtz@126.com>
# Licence: GPLv3


# CONSTANTS DEFINITION
EXAMED_FIELD <- 'bo:halfLife'
OUTPUT_PIC_FILE <- 'stat_digit_freq.png'


# Load the packages required to read XML files.
library("XML")
library("methods")


# Read arguments passed by command line
Args <- commandArgs()
dataFile <- Args[6]
examedField <- Args[7]


# Verify the arguments
if (is.na(dataFile)) {
    print('Please specify a data file for analysis!\n')
    quit()
}
if (is.na(examedField)) {
    print(paste('Field name not specified. Use default field \'', EXAMED_FIELD, '\' to extract!\n', sep=''))
    examedField <- EXAMED_FIELD
}


# Parse xml file to a data frame.
xmlData <- xmlParse(dataFile)


# Select interested node set
queryPath <- paste('//*[@dictRef=\'', examedField, '\']', sep='')
dataSet <- getNodeSet(xmlData, queryPath)
dataList <- sapply(dataSet, xmlValue)


# Stat for digit frequency and normalize it
freq <- rep(0, 10)
for (var in dataList) {
    digit <- as.numeric(var) %/% 10 ^ trunc(log10(as.numeric(var)))
    freq[digit] <- freq[digit] + 1 
}
freq <- freq / sum(freq)


# Exam the regression model
x <- 1:9
result <- lm(freq[x] ~ log10(1 + 1 / x))
summary(result)


# Output the stat result
# setwd('~/')
png(file=OUTPUT_PIC_FILE, bg='#FFFFFF')
plot(x, freq[x])


# Output the curve for comparison
x <- seq(0, 10, 0.1)
y <- log10(1 + 1 / x)
lines(x, y, col='#666666', lwd='1')

print(paste('Graphic result saved as ', OUTPUT_PIC_FILE, '.\n', sep=''))
