# R script for calculating data range and
# scaling data according to a given range

calcRangeByMinMax <- function(dataSets, varName)
{
    range <- c(Inf, -Inf)
    for (dataSet in dataSets)
    {
        temp <- min(dataSet[,varName], na.rm = TRUE)
        if (temp < range[1])
            range[1] <- temp
        
        temp <- max(dataSet[,varName], na.rm = TRUE)
        if (temp > range[2])
        range[2] <- temp
    }
    return(range)
}

calcRangeByMeanStd <- function(dataSets, varName, numberOfSigma = 1)
{
    meanValue <- c()
    stdValue <- 0
    for (dataSet in dataSets)
    {
        meanValue <- c(meanValue, mean(dataSet[,varName], na.rm = TRUE))
        
        temp <- abs(sd(dataSet[,varName], na.rm = TRUE))
        if (temp > stdValue)
            stdValue <- temp
    }
    meanValue <- mean(meanValue)
    return(c(meanValue - numberOfSigma * stdValue, 
             meanValue + numberOfSigma * stdValue))
}

normalizeByRange <- function(data, range)
{
    return((data - mean(range)) / (range[2] - range[1]) * 2)
}