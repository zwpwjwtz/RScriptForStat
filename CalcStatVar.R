# R script for calculating statistic values (sum, stat, etc.) 
# of a data frame, over a given range of a given variable.
# All other variables in the data frame are also treated, according
# to the specified statistic methods.

source('DateTimeConv.R')

varToNumeric <- function(var, type='integer', format='')
{
    if (type == 'date')
        return(varToDate(var, format))
    else if (type == 'time')
        return(varToTimeTick(var, format))
    else
        return(as.numeric(var))
}

numericToVar <- function(var, type='integer', format='')
{
    if (type == 'date')
        return(dateToVar(var, format))
    else if (type == 'time')
        return(timeTickToVar(var, format))
    else
        return(var)
}

calcStatVars <- function(dataList, statVarList=list())
{
    result <- list()
    discard <- FALSE
    for (statVar in statVarList)
    {
        # Pick variables (columns) from data frame
        # Then apply the statistic functions on each of them
        if (!is.element(statVar$name, colnames(dataList)))
            next
        tempStat <- mapply(statVar$func,
                           dataList[statVar$name],
                           MoreArgs = statVar$param,
                           SIMPLIFY = FALSE)
        
        # Exit the loop early if user does not want NAs
        if (all(is.na(tempStat)) && statVar$param$na.rm)
        {
            discard <- TRUE
            break
        }
        else
        {
            result <- c(result, tempStat)
        }
    }
    
    if (discard)
        return(list())
    else
        return(result)
}

calcStats <- function(data, 
                      statVar=list(name='y', type='integer', interval=1),
                      varList=list(),
                      classifierList=c())
{
    statVarList <- unique(data[,statVar$name])
    if (length(statVarList) < 1)
        return(data)
    
    varNameList <- c()
    for (i in 1:length(varList))
    {
        if (is.element(varList[[i]]$name, colnames(data)))
            varNameList <- c(varNameList, varList[[i]]$name)
        else
        {
            varList[i] <- NULL
            i <- i - 1
        }
    }
    
    varStatValues <- data.frame()
    for (classifier in classifierList)
    {
        # Deal with the column specified by 'classifier'
        classifiedVarList <- unique(data[,classifier])
        for (classifiedVar in classifiedVarList)
        {
            # Deal with rows that has value 'classifiedVar' in this column
            reducedVarIndexes <- data[,classifier] == classifiedVar
            tempData <- data[reducedVarIndexes,varNameList]
            if (length(tempData) < 1)
                next
            
            # Reduce loop length if the 'statVar' is exactly the 'classifier'
            if (statVar$name == classifier)
                reducedStatVarList <- classifiedVar
            else
                reducedStatVarList <- statVarList
            
            reducedStatVarList <- reducedStatVarList[order(reducedStatVarList)]
            lastStatValue <- reducedStatVarList[1] - statVar$interval
            tempStatValues <- data.frame()
            
            for (currentStatValue in reducedStatVarList)
            {
                # Deal with rows that has value 'stat_var' in the column
                # that is specified by param 'statVar'
                tempStatValues <- 
                    rbind(tempStatValues, 
                          calcStatVars(tempData[tempData[statVar$name] ==
                                                    currentStatValue,],
                                       varList))
                if (currentStatValue >= statVar$interval + lastStatValue)
                {
                    if (nrow(tempStatValues) > 1 & 
                        !all(is.na(tempStatValues)))
                    {
                        tempStatValues <- as.data.frame(
                            calcStatVars(tempStatValues, varList))
                    }
                    tempStatValues[,classifier] <- rep(classifiedVar,
                                                       nrow(tempStatValues))
                    varStatValues <- rbind(varStatValues, tempStatValues)
                    lastStatValue <- currentStatValue
                    tempStatValues <- data.frame()
                }
            }
            if (lastStatValue != currentStatValue)
            {
                tempStatValues <- as.data.frame(
                    calcStatVars(tempStatValues, varList))
                tempStatValues[,classifier] <- rep(classifiedVar,
                                                   nrow(tempStatValues))
                varStatValues <- rbind(varStatValues, tempStatValues)
            }
        }
    }
    return(varStatValues)
}