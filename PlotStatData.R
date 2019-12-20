library('reshape2')
library('ggplot2')

source('CalcDataRange.R')


plotBoxNormalDistrib <- function(dataSets, targetFileName, 
                                 resolution=c(1600, 800),
                                 yVarNameList,
                                 yVarLimits=list(),
                                 titleText=c(), 
                                 xGroupName='x', yGroupName='y',
                                 xLabelList=c(), yLabelList=c(),
                                 yLegendList=c())
{ 
    # Calculate ranges for each Y variable
    # Then normalize them
    for (var in yVarNameList)
    {
        #range <- calcRangeByMinMax(dataSets, var)
        #range <- calcRangeByMeanStd(dataSets, var, 1)
        #for (i in seq(1, length(dataSets)))
        #{
        #    dataSets[[i]][var] <- normalizeByRange(dataSets[[i]][,var], range)
        #}
    }
    
    # Combine all data sets into one data frameplot_group_name
    data <- as.data.frame(matrix(NA, nrow = 0, ncol = length(dataSets)))
    for (i in seq(1, length(dataSets)))
    {
        data <- rbind(data, 
                      cbind(dataSets[[i]][yVarNameList], 
                            rep(xLabelList[i], nrow(dataSets[[i]]))))
    }
    colnames(data) <- c(yVarNameList, 'Group')
    
    # Calculate Y ranges for each variable
    yLimitMin <- c()
    yLimitMax <- c()
    for (limit in yVarLimits)
    {
        yLimitMin <- c(yLimitMin, limit[1])
        yLimitMax <- c(yLimitMax, limit[2])
    }
    
    data_melted <- melt(data, id.var = 'Group')
    p <- ggplot(data = data_melted, aes(x=variable, y=value)) + 
        geom_boxplot(aes(fill=Group),
                     position = position_dodge(1)) + 
        scale_x_discrete(name='') + 
        scale_y_continuous(name=yGroupName) + 
        stat_summary(aes(group=Group), fun.y=mean, 
                     col='red', geom='point',
                     position = position_dodge(1))
    
    yLabel <- function(string)
    {
        return(yLabelList[which(string == yVarNameList)])
    }
    p <- p + facet_wrap( ~ variable, scales="free", 
                         labeller = labeller(.default = yLabel))
    p <- p + theme(panel.background = element_rect(fill = "white", color = "black", size = 1),
                   #plot.background = element_rect(color = "black", size = 1),
                   text = element_text(size=20),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()
    ) 
    p <- p + guides(fill=guide_legend(title=xGroupName))
    p <- p + ggtitle(titleText)
    
    ggsave(targetFileName, 
           plot = p, 
           device = png, 
           dpi = 300,
           width = resolution[1], height = resolution[2],
           limitsize = FALSE)
}