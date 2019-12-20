# R script for manipulating 2-D plot

library('ggplot2')


# Add points on given ggplot object
# pointList: list of coordinates of points
# image: a ggplot object to attach to
# pointColor: color of the points
plotPoints <- function(pointList = list(), 
                       image = ggplot(),
                       pointColor = "red",
                       pointFill = "red",
                       pointShape = 21,
                       pointSize = 1,
                       lineWidth = 0.2)
{
    pointMatrix <- as.data.frame(t(as.data.frame(pointList)))
    colnames(pointMatrix) <- c("dim1", "dim2")
    image <- image + 
                geom_point(data = pointMatrix, 
                           aes(dim1, dim2),
                           shape = pointShape,
                           color = pointColor,
                           fill = pointFill,
                           size = pointSize,
                           stroke = lineWidth)
    
    return(image)
}