# R script for converstion between time/date and their character representation

# Internal parameters used by convertion functions
data_date_origin <- '1970-01-01'

# String => as.numeric(Date)
varToDate <- function(dateValue, dateFormat='%Y-%m-%d')
{
    if (is.numeric(dateValue))
        dateString <- as.character(dateValue)
    else
        dateString <- dateValue
    return(as.numeric(as.Date(dateString, 
                              format=dateFormat, 
                              origin=data_date_origin)))
}

# String => as.numeric(Date)
dateToVar <- function(date, dateFormat='%Y-%m-%d')
{
    return(format(as.Date(date, origin=data_date_origin), 
                  format=dateFormat, 
                  origin=data_date_origin))
}

# String => POSIXlt
varToTime <- function(timeValue, timeFormat='%H:%M:%S')
{
    # Deal with customized time identifiers
    if (!is.null(timeFormat))
    if (timeFormat == '%OH')
    {
        timeFormat <- '%Y-%m-%d %H:%M:%S'
        timeValue <- format(as.POSIXct(timeValue * 3600, 
                                        origin='1900-01-01',
                                        tz = 'UTC'),
                             format = timeFormat)
    }
    if (is.numeric(timeValue))
        timeString <- as.character(timeValue)
    else
        timeString <- timeValue
    return(strptime(timeString, format=timeFormat))
}

# String => POSIXct
varToTimeTick <- function(timeValue, timeFormat='%H:%M:%S')
{
    # Deal with customized time identifiers
    if (!is.null(timeFormat))
        if (timeFormat == '%OH')
    {
        return(as.POSIXct(timeValue * 3600, 
                                     origin='1900-01-01',
                                     tz = 'UTC'))
    }
    if (is.numeric(timeValue))
        timeString <- as.character(timeValue)
    else
        timeString <- timeValue
    return(as.POSIXct(strptime(timeString, format=timeFormat), 
                      origin=data_date_origin))
}

# POSIXlt => String
timeToVar <- function(time, timeFormat='%H:%M:%S')
{
    return(format(time, format=timeFormat))
}

# POSIXct => String
timeTickToVar <- function(time, timeFormat='%H:%M:%S')
{
    if (is.numeric(time))
        time <- as.POSIXct(time, origin=data_date_origin)
    return(format(time, format=timeFormat))
}

# Strings + Strings => as.numeric(POSIXct) / Strings
mergeDateTime <- function(dateVar, dateFormat='%Y-%m-%d',
                          timeVar, timeFormat='%H:%M:%S',
                          resultFormat=c())
{
    dateString <- dateToVar(varToDate(dateVar, dateFormat),'%Y-%m-%d')
    timeString <- timeToVar(varToTime(timeVar, timeFormat), '%H:%M:%S')
    dateTime <- varToTimeTick(paste(dateString, timeString), 
                              '%Y-%m-%d %H:%M:%S')
    if (length(resultFormat) > 0)
    {
        dateTime <- timeTickToVar(dateTime, resultFormat)
    }
    return(dateTime)
}