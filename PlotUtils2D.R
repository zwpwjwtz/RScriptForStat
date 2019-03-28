# R script for manipulating 2-D plot

library('ggplot2')


# Add points on given ggplot object
# pointList: list of coordinates of points
# image: a ggplot object to attach to
# pointColor: color of the points
plotPoints <- function(pointList = list(), 
                       image = ggplot(),
                       pointColor = "red")
{
    pointMatrix <- as.data.frame(t(as.data.frame(pointList)))
    colnames(pointMatrix) <- c("dim1", "dim2")
    image <- image + geom_point(data = pointMatrix, 
                                aes(dim1, dim2),
                                color = pointColor)
    
    return(image)
}