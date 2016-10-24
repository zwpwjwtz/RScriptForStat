#!/bin/Rscript
#
# R Script for exam digit frequency of data in a given XML file
# Author: zwpwjwtz <zwpwjwtz@126.com>
# Licence: GPLv3


# CONSTANTS DEFINITION
DEFAULT_FILTER <- 'dictRef:bo:halfLife'
OUTPUT_PIC_FILE <- 'stat_digit_freq.png'


# Load the packages required to read XML files.
library("XML")
library("methods")


# Read and parse arguments passed by command line
args <- commandArgs(TRUE)
if(length(args) < 1) {
  args <- c('--help')
}
if('--help' %in% args) {
  cat("
The R Script for exam digit frequency of data in a given XML file
 
    Arguments:
      --digit=INTEGER           - Which digit should we stat (default is 1)
      --output_pic=FILENAME     - Name of output graphic file
      --property=KEY:VALUE      - Use certain property to filter out some nodes
      --source=FILENAME         - Source XML file
      --help                    - print this help text
 
    Example:
      ./stat_digit_freq.R --DIGIT 1 --source=./data.xml\n\n") 
  quit()
}

parseArgs <- function(x) strsplit(sub('^--', '', x), '=')
argDF <- as.data.frame(do.call('rbind', parseArgs(args)))
argList <- as.list(as.character(argDF$V2))
names(argList) <- argDF$V1

digitNum <- argList$digit
outPicFile <- argList$output_pic
filter <- argList$property
dataFile <- argList$source


# Verify the arguments
if (is.null(dataFile)) {
    cat('Please specify a data file for analysis!\n')
    quit()
}
if (is.null(digitNum)) {
    cat('Digit number not specified. Use default value 1.\n')
    digitNum <- 1
}
if (is.null(outPicFile)) {
    cat('Output picture file name not specified. Use default name \'', OUTPUT_PIC_FILE, '\'.\n')
    outPicFile <- OUTPUT_PIC_FILE
}
if (is.null(filter)) {
    cat('Property name not specified. Use default property \'', DEFAULT_FILTER, '\' for filtering!\n')
    filter <- DEFAULT_FILTER
}


# Parse xml file to a data frame.
xmlData <- xmlParse(dataFile)


# Select interested node set
queryPath <- paste('//*[@', sub(':', '=\'', filter), '\']', sep='')
dataSet <- getNodeSet(xmlData, queryPath)
dataList <- as.numeric(sapply(dataSet, xmlValue))


# Stat for digit frequency and normalize it
digitNum <- as.numeric(digitNum) - 1
freq <- rep(0, 10)
for (var in dataList) {
    digit <- (var %/% 10 ^ trunc(log10(var) - digitNum)) %% 10
    if (digit == 0) digit <- 10
    freq[digit] <- freq[digit] + 1 
}
freq <- freq / sum(freq[1:9])


# Exam the regression model
x <- 1:9
result <- lm(freq[x] ~ log10(1 + 1 / x))
summary(result)


# Output the stat result
# setwd('~/')
png(file=outPicFile, bg='#FFFFFF')
plot(x, freq[x])


# Output the curve for comparison
x <- seq(0, 10, 0.1)
y <- log10(1 + 1 / x)
lines(x, y, col='#666666', lwd='1')

cat('Graphic result saved as ', outPicFile, '.\n')
