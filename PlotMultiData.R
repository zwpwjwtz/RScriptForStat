# R script for plotting multiple variables of data set(s)

source('DateTimeConv.R')

# Internal variables for generating colors
rainbow_default_color_ranges <- c(c(0, 0.12), # Red ~ Orange
                                  c(0.35, 0.4), # Green
                                  c(0.55, 0.65), # Blue
                                  c(0.75, 0.85) # Purple
                                 )

setPlotMargins <- function(side, lines)
{
    plotArgs <- list()
    plotArgs$OMA <- par('oma')
    plotArgs$OMA[side] <- lines
    par(oma=plotArgs$OMA)
}

setSubPlotMargins <-function(side, lines)
{
    plotArgs <- list()
    plotArgs$MAR <- par('mar')
    plotArgs$MAR[side] <- lines
    par(mar=plotArgs$MAR)
}

setPlotAxisFontSize <- function(relativeSize)
{
    par(cex.axis=relativeSize)
}

setPlotAxisTitleFontSize <- function(relativeSize)
{
    par(cex.lab=relativeSize)
}

addGraphBorder <- function(side)
{
    if (side == 1 || side == 3)
        range <- par("usr")[1:2]
    else
        range <- par("usr")[3:4]
    axis(side, at=range, labels=c(" "," "), lwd.ticks=0)
}

# Funcion that cut date ranges by natural month
cut.POSIXt.natural <- function(data, breaks='month', count=NA)
{
    if (breaks == 'month')
    {
        breakFormat <- '%m'
        #subBreakFormat <- '%d'
    }
    else if (breaks == 'year')
    {
        breakFormat <- '%Y'
        #subBreakFormat <- '%m'
    }
    else if (breaks == 'day')
    {
        breakFormat <- '%d'
        #subBreakFormat <- '%H'
    }
    else
        return(data)
    
    if (inherits(data,'POSIXlt'))
        data <- as.POSIXct(data, origin='1970-01-01')
    formattedBreakList <- format(data, breakFormat)
    uniqueList <- unique(formattedBreakList)
    breakList <- c()
    
    for (i in seq(1, length(uniqueList)))
    {
        breakList <- c(breakList,
                       data[which(uniqueList[i] == formattedBreakList)[1]])
    }
    breakList <- breakList[!is.na(breakList)]
    breakList <- breakList[order(breakList)]
    breakList <- as.POSIXlt(breakList, origin='1970-01-01')
    
    if (length(breakList) < 2)
        return(breakList)
    
    if (!is.na(count) && count > length(breakList))
    {
        # Adjust the number of breaks
        subBreaksCount <- floor(count / length(breakList))
        intervals <- pretty(data, n = count)
    }
    else
        intervals <- breakList
    
    return(intervals)
}

rainbow.comfortable <- function(number, colorRanges=c())
{
    if (length(colorRanges) < 2)
        colorRanges <- rainbow_default_color_ranges
    dim(colorRanges) <- c(2, length(colorRanges) / 2)
    colorRangesLength <- colorRanges[2,] - colorRanges[1,]
    colorGroups <- ceiling(number * colorRangesLength / sum(colorRangesLength))
    
    colors <- c()
    for (i in seq(1, length(colorGroups)))
    {
        colors <- c(colors, rainbow(colorGroups[i], 
                                    start=colorRanges[1, i],
                                    end=colorRanges[2, i]))
    }
    return(colors)
}

removeInf <- function(var)
{
    # Calculate index masks
    indexes <- (var != Inf) & (var != -Inf)
    
    if (length(indexes[indexes == FALSE]) == length(var))
        return(NULL)
    else
        return(var[indexes])
}

plotAnnualData <- function(data, targetFileName, 
                           resolution=c(1600, 1000),
                           timeVarName='time', varName='y',
                           titleText='',
                           xLabel='time', yLabel='y',
                           xFormat='%m-%d',
                           xTickFormat='month',
                           xLimit=c(), yLimit=c(),
                           plotFormat='b')
{
    # Classify by year
    # Assuming the year is represented by the first four digits
    data$year <- timeTickToVar(data[,timeVarName], '%Y')
    data$time <- varToTime(timeTickToVar(data[,timeVarName], '%m%d %H%M%S'),
                           '%m%d %H%M%S')
    
    yearList <- unique(unlist(data$year))
    timeList <- as.POSIXct(data[,'time'])
    
    png(targetFileName, 
        res = 200, 
        width = resolution[1], 
        height = resolution[2])
    
    setPlotMargins(1, 0)
    setPlotMargins(2, 1)
    
    # Adjust label styles for X and Y Axes
    setPlotAxisFontSize(1.2)
    #setPlotAxisTitleFontSize(1.4)
    
    # Calculate plot boundary
    if (length(xLimit) < 2)
    {
        if (length(xLimit) < 1)
            xLimit <- timeList[which.min(timeList)]
        xLimit <- c(xLimit, timeList[which.max(timeList)])
    }
    if (length(yLimit) < 2)
    {
        if (length(yLimit) < 1)
            yLimit <- min(data[,varName], na.rm = TRUE)
        yLimit = c(yLimit, max(data[,varName], na.rm = TRUE))
    }
    
    # Create canvas and axes
    plot(0, 0, type = "n", axes = FALSE,
         xlab="", ylab="",
         xlim=removeInf(xLimit), ylim=removeInf(yLimit))
    mtext(xLabel, side = 1, line = 3)
    mtext(yLabel, side = 2, line = 3)
    
    if (substring(xTickFormat, 1, 8) == 'natural ')
    {
        # Use customized "natural cut" function
        intervals <- cut.POSIXt.natural(timeList, 
                                        substring(xTickFormat, 9),
                                        count = 9)
    }
    else
        intervals <- cut.POSIXt(timeList, 
                                xTickFormat, 
                                include.lowest = TRUE)
    axis.POSIXct(side=1, tck=0.02, at=intervals, format=xFormat)
    axis(side=2, tck=0.02)
    
    # Plot lines for each year
    colors <- rainbow.comfortable(length(yearList))
    for (i in seq(1, length(yearList)))
    {
        # Select data that corresponds to the given year
        # and remove NAs on the variable field
        indexes <- data[,'year'] == yearList[i]
        indexes <- indexes & !is.na(data[,varName])
        yearData <- data[indexes,]
        
        # Sort data according to "time" field
        yearData <- yearData[order(yearData[,'time']),]
        
        # Draw lines and points
        lines(x=yearData[,'time'], 
              y=yearData[,varName],
              col=colors[i],
              pch=19,
              cex=0.5,
              lwd=1,
              type=plotFormat)
    }
    
    # Add legend for each year
    legend("topright", legend = yearList, col=colors, lwd = 1)
    
    # Add a title
    title(titleText)
    
    # Add borders
    addGraphBorder(1)
    addGraphBorder(2)
    addGraphBorder(3)
    addGraphBorder(4)
    
    dev.off()
}

plotDataSets.default <- function(dataSets, colors, 
                                 xVarNameList, yVarNameList,
                                 xLabel='x', yLabelList=c(),
                                 xLimit=c(), yLimit=c(),
                                 plotFormat='b')
{
    
}

plotDataSets.temporal <- function(dataSets, colors,
                                  xVarNameList, yVarNameList,
                                  xVarFormat='%Y-%m-%d %H:%M:%S',
                                  xTickFormat='month',
                                  xLabel='Time', yLabelList=c(),
                                  xAxis=TRUE,
                                  xFormat='%m-%d',
                                  xLimit=c(), yLimit=c(),
                                  plotFormat='b')
{
    # Create an empty layer
    plot(0, 0, type = 'n', xlab = '', ylab = '', axes = FALSE)
    
    # Adjust bottom margin for X Axis
    if (xAxis)
        setSubPlotMargins(1, 4)
    else
        setSubPlotMargins(1, 0)
    
    # Adjust label styles for X and Y Axes
    setPlotAxisFontSize(1.4)
    
    for (i in seq(1, length(dataSets)))
    {
        data <- dataSets[[i]]
        
        # Remove NAs on the variable field
        data <- data[!is.na(data[,yVarNameList[i]]),]
        
        # Sort data according to "time" field
        timeList <- data[,xVarNameList[i]]
        if (!is.numeric(timeList))
        {
            timeList <- varToTimeTick(timeList, xVarFormat)
        }
        data <- data[order(timeList),]
        
        # The range of X axis (time) is only defined 
        # by the first layer
        if (!exists('timeRange'))
        {
            timeRange <- list()
            timeRange$min <- timeList[which.min(timeList)]
            timeRange$max <- timeList[which.max(timeList)]
        }
        
        # Calculate plot boundaries and verify their validity
        if (length(xLimit) < 2)
        {
            if (length(xLimit) < 1)
                xLimit <- timeRange$min
            xLimit = c(xLimit, timeRange$max)
        }
        xLimit <- removeInf(xLimit)
        yVarLimit <- yLimit
        if (length(yVarLimit) < 2)
        {
            if (length(yVarLimit) < 1)
                yVarLimit <- min(data[,yVarNameList[i]])
            yVarLimit <- c(yVarLimit, max(data[,yVarNameList[i]]))
        }
        yVarLimit <- removeInf(yVarLimit)
        if (is.null(xLimit) | is.null(yVarLimit))
            next
        
        # Create new plot layer
        par(new = TRUE)
        plot(0, 0, type = 'n', axes = FALSE, 
             xlab = '', ylab = '',
             xlim = xLimit, ylim = yVarLimit)
        
        # Add an X axis and label
        if (i == 1 && xAxis)
        {
            # Ugly hack: numeric => POSIXct
            if (!inherits(timeList, 'POSIXt'))
            {
                timeList <- varToTime(timeTickToVar(timeList, xVarFormat),
                                      xVarFormat)
            }
            
            if (substring(xTickFormat, 1, 8) == 'natural ')
            {
                # Use customized "natural cut" function
                intervals <- cut.POSIXt.natural(timeList, 
                                                substring(xTickFormat, 9),
                                                count = 9)
            }
            else
                intervals <- cut.POSIXt(timeList, 
                                        xTickFormat, 
                                        include.lowest = TRUE)
            axis.POSIXct(side=1, tck=0.02, at=intervals, format=xFormat)
                
            mtext(xLabel, side=1, line = 3)
        }
        
        # Add y axes and labels
        if (i == 1)
        {
            yAxisSide <- 2
            yAxisLine <- 4
        }
        else if (i == 2)
        {
            yAxisSide <- 4
            yAxisLine <- 6
        }
        else
            yAxisSide <- 0
        if (yAxisSide > 0)
        {
            axis(side=yAxisSide, las=1, tck=0.02)
            mtext(yLabelList[i], side=yAxisSide, line=yAxisLine)
        }
        
        # Draw lines and points
        lines(x=timeList, 
              y=data[,yVarNameList[i]],
              col=colors[i],
              pch=19,
              cex=0.3,
              lwd=2,
              type=plotFormat)
    }
    
    # Add borders
    addGraphBorder(1)
    addGraphBorder(2)
    addGraphBorder(3)
    addGraphBorder(4)
}

plotDataSets <- function(dataSets=list(), targetFileName, 
                         resolution=c(1600, 1000),
                         colors=c(),
                         xVarType='time',
                         titleText='',
                         yLabelList=c(),
                         yLegendList=c(),
                         ...)
{
    # Create new plot device
    png(targetFileName, 
        res = 200, 
        width = resolution[1], 
        height = resolution[2])
    
    setPlotMargins(2, 3)
    setPlotMargins(4, 4)
    
    # Set palette
    if (length(colors) < 1)
        colors <- rainbow.comfortable(length(dataSets))
    
    # Plot lines for each data set
    if (xVarType == 'time')
        plotDataSets.temporal(dataSets = dataSets, colors = colors,
                              yLabelList = yLabelList, ...)
    else
        plotDataSets.default(dataSets = dataSets, colors = colors,
                             yLabelList = yLabelList, ...)
    
    # Add legend for each year
    if (length(yLegendList) < length(dataSets))
        yLegendList <- c(yLegendList, 
                         yLabelList[seq(length(yLegendList) + 1,
                                        length(yLabelList))])
    if (length(yLegendList) > 1)
        legend("topright", legend = yLegendList, col=colors, lwd = 1)
    
    # Add a title
    title(titleText)
    
    dev.off()
}

plotMultiGraph <- function(dataSetsList, targetFileName, 
                           resolution=c(1600, 1000),
                           graphRatio=c(),
                           xVarNameList, yVarNameList,
                           xVarTypeList=c(),
                           yLimitList=list(),
                           titleTextList=list(), 
                           yLabelList=list(),
                           yLegendList=list(),
                           ...)
{
    # Create new plot device
    png(targetFileName, 
        res = 200, 
        width = resolution[1], 
        height = resolution[2])
    
    setPlotMargins(2, 2)
    setPlotMargins(4, 5)
    setSubPlotMargins(3, 0.5)
    
    # Set up multiple graph layout
    if (length(graphRatio) < 1)
        graphRatio <- rep(1, length(dataSetsList))
    layout(matrix(seq(1, length(dataSetsList)), 
                  length(dataSetsList), 1, byrow = TRUE), 
           heights=graphRatio)
    #par(mfrow=c(length(dataSetsList), 1))
    
    # Count for total number of graphs in this list of data sets 
    graphCount <- 0
    for (dataSets in dataSetsList)
    {
        for (dataSet in dataSets)
            graphCount <- graphCount + 1
    }
    
    # Set palette
    colors <- rainbow.comfortable(graphCount)
    currentColorIndex <- 1
    
    for (i in seq(1, length(dataSetsList)))
    {
        # Set default variable range for plot
        if (length(yLimitList) >= i)
            yLimit <- yLimitList[[i]]
        else
            yLimit <- c()
    
        # Select color range
        colorRange <- colors[seq(currentColorIndex, 
                                 currentColorIndex + 
                                     length(dataSetsList[[i]]) - 1)]
        currentColorIndex <- currentColorIndex + length(dataSetsList[[i]])
        
        # Determine whether the X axis is to be shown
        if (i == length(dataSetsList) ||
            i > 1 && xVarTypeList[i] != xVarTypeList[i - 1])
            showXAxis <- TRUE
        else
            showXAxis <- FALSE
        
        # Plot for each item in the list of data sets
        if (xVarTypeList[i] == 'time')
            plotDataSets.temporal(dataSets = dataSetsList[[i]], 
                                  colors = colorRange,
                                  xVarNameList = xVarNameList[[i]],
                                  yVarNameList = yVarNameList[[i]],
                                  yLimit = yLimit,
                                  yLabelList = yLabelList[[i]],
                                  xAxis = showXAxis,
                                  ...)
        else
            plotDataSets.default(dataSets = dataSetsList[[i]], 
                                 colors = colorRange,
                                 xVarNameList = xVarNameList[[i]],
                                 yVarNameList = yVarNameList[[i]],
                                 yLabelList = yLabelList[[i]], 
                                 ...)
        
        # Add legend for each year
        if (length(yLegendList[[i]]) < length(dataSetsList[[i]]))
            yLegendList[[i]] <- c(yLegendList[[i]],
                                  yLabelList[[i]]
                                        [seq(length(yLabelList[[i]]) + 1,
                                             length(yLabelList[[i]]))])
        if (length(yLegendList[[i]]) > 1)
            legend("topright", 
                   legend = yLegendList[[i]], 
                   col=colorRange, lwd = 1)
        
        # Add a title
        if (length(titleTextList) >= i)
        {
            if (typeof(titleTextList) == 'list')
                title(titleTextList[[i]])
            else
                title(titleTextList[i])
        }
    }
    
    dev.off()
}