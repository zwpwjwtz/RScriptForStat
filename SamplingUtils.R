# Script for generating sample points in a rectangular area
# along the sample lines defined by given coordinates


# Helper function for adjusting coordiante that 
# falls out of the given geometry
adjust_boundary <- function(point, geometry)
{
     dimension_count <- length(point)
     
     for (i in seq(1:dimension_count))
     {
         if (point[i] < geometry[i])
             point[i] <- geometry[i]
         if (point[i] > geometry[i + dimension_count])
             point[i] <- geometry[i + dimension_count]
     }
     return(point)
}

# Helper function for generating sequence of given length
# with desired step, i.e. sequence containing possible duplicates
seqWithRep <- function(from, to, step = 1, length = abs((to - from) / step) + 1)
{
    if ((to - from) / step < 0)
        step <- -step
    
    newSeq <- seq(from, to, step)
    if (length(newSeq) == 1)
    {
        # Correct the unexpected behavior of sample() when 
        # the first parameter ("X") has a length of 1
        newSeq <- c(newSeq, newSeq)
    }
    duplicates <- sample(newSeq, length - length(newSeq),replace = TRUE)
    newSeq <- c(newSeq, duplicates)
    
    if (from < to)
        return(newSeq[order(newSeq)])
    else
        return(newSeq[order(newSeq, decreasing = TRUE)])
}

# Helper function for generating "brodening" sequence (vector)
# with given step and duplicity from original sequence
# e.g. By duplicate the sequence c(1,3,5) twice with step 1, 
# we have a new sequence c(0,1,2,2,3,4,4,5,6)
brodeningSeq <- function(var, duplicity = 1, step = 1)
{
    duplicity <- duplicity - 1
    if (duplicity < 1)
        return(var)
    
    newSeq <- c()
    for (value in var)
    {
        newSeq <- c(newSeq, 
                    seq(value - step * duplicity,
                        value + step * duplicity,
                        step))
    }
    return(newSeq)
}

# Helper function for removing outlier points basing on a
# given list of available points (map)
removeOutlierPoints <- function(map, pointList)
{
    indexes <- c()
    for (i in seq(1, length(pointList)))
    {
        filter <- rep(TRUE, nrow(map))
        for (j in seq(1, ncol(map)))
            filter <- filter & (map[,j] == pointList[[i]][j])
        if (length(which(filter)) > 0)
            indexes <- c(indexes, i)
    }
    if (length(indexes) > 1)
        pointList[-indexes] <- NULL
    return(pointList)
}

# Helper function for generating a matrix of discreate coordinates by 
# applying "cross-product" to lists of coordinates of different dimensions,
# and then "melting" the matrix into a simple list of coordinates
meltMultiDimCoordinates <- function(coordinates=list())
{
    dimensionCount <- length(coordinates)
    
    # Deal with empty list
    if (dimensionCount < 1)
        return(data.frame())
    
    # Deal with 1-D coordinates (should be the deepest level of recursion)
    if (dimensionCount < 2)
        return(data.frame(coordinates[[1]]))
    
    # Deal with inferior dimensions recursively
    newCoordinates <- data.frame()
    for (point in coordinates[[1]])
    {
        subCoordinates <- meltMultiDimCoordinates(coordinates[2:dimensionCount])
        newCoordinates <- rbind(newCoordinates,
                                cbind(point, 
                                      subCoordinates, 
                                      deparse.level = 0))
    }
    return(newCoordinates)
}


# Generate coordinates of points for sampling
# area: A vector representing the sampling boundary, e.g. c(0,0,256,256)
# nodeList: List of coordinates of sampling nodes
# accuracy: Spatial resolution of sampling for each dimension
# radius: Broadening for each dimension during sampling
generateSamplePoints <- function(nodeList=list(),
                                 area=c(-Inf,-Inf,Inf,Inf),
                                 accuracy=c(1,1),
                                 radius=c(1,1),
                                 referenceMap=NULL)
{
    # Adjust boundaries for sampling nodes
    adjusted_nodes <- list()
    for (node in nodeList)
    {
        adjusted_nodes <- c(adjusted_nodes,
                            list(adjust_boundary(node, area)))
    }
    
    dimension_count <- as.integer(length(area) / 2)
    if (dimension_count < 1)
        return(list())
    
    if (length(adjusted_nodes) < 2)
        return(adjusted_nodes)
    
    # Set default accuracy and radius for each dimension
    if (length(accuracy) < dimension_count)
        accuracy <- c(accuracy, rep(1, dimension_count - length(accuracy)))
    if (length(radius) < dimension_count)
        radius <- c(radius, rep(1, dimension_count - length(radius)))
    
    # Calculate the duplicity of sampling point for each dimension
    duplicity <- ceiling(radius / accuracy)

    # Generate coordinates for each dimension
    sample_point_matrix <- matrix(nrow = 0, ncol = dimension_count)
    for (i in seq(1, length(adjusted_nodes) - 1))
    {
        lower_bound <- adjusted_nodes[[i]]
        upper_bound <- adjusted_nodes[[i + 1]]
        
        # Calculate the maximum number of points required 
        # along the center line (defined by sampling nodes)
        sampling_count <- max(abs(upper_bound - lower_bound) / accuracy + 1)
        
        # Calculate centroid coordinates for each dimension
        coordinates <- c()
        dimensionOrder <- c() # TRUE = Ascending, FALSE = Decending
        for (j in seq(1, dimension_count))
        {
            # Get coordinates' order of this dimension
            if (lower_bound[j] < upper_bound[j])
                dimensionOrder <- c(dimensionOrder, TRUE)
            else
                dimensionOrder <- c(dimensionOrder, FALSE)
            
            # Generate replicates for each coordinates
            coordinates_1D <- seqWithRep(lower_bound[j], upper_bound[j],
                                         accuracy[j], sampling_count)
            coordinates <- cbind(coordinates, coordinates_1D)
        }
            
        # Calculate bordened coordinates for each centroid coordinate
        for (j in seq(1, sampling_count))
        {
            coordinates_1D <- list()
            for (k in seq(1, dimension_count))
            {
                coordinate <- coordinates[j, k]
                coordinates_1D <- c(coordinates_1D,
                                    list(brodeningSeq(coordinate, 
                                                      duplicity[k],
                                                      accuracy[k])))
            }
            
            # Generate point coordinates of all the dimensions
            sample_point_matrix <- 
                    rbind(sample_point_matrix, 
                          meltMultiDimCoordinates(coordinates_1D))
        }
    }
    
    # Remove duplicated coordinates (lines)
    if (ncol(sample_point_matrix) > 1)
        sample_point_matrix <- unique.matrix(sample_point_matrix, MARGIN = 1)
    else
        sample_point_matrix <- unique(sample_point_matrix)
    colnames(sample_point_matrix) <- paste0("dim", seq(1, dimension_count))

    # Convert matrix to list
    sample_point_list <- list()
    for (i in seq(1, nrow(sample_point_matrix)))
    {
        sample_point_list <- c(sample_point_list,
                               list(unlist(sample_point_matrix[i,])))
    }
    
    if (is.null(referenceMap))
        return(sample_point_list)
    else
        return(removeOutlierPoints(referenceMap, sample_point_list))
}

# Get indexes of points that are present in a given list (map)
# map: a list of coordinates of availabel points
# pointList: a list of coordiantes to search for
getPointIndexes <- function(map, pointList)
{
    indexes <- c()
    for (i in seq(1, length(pointList)))
    {
        filter <- rep(TRUE, nrow(map))
        for (j in seq(1, ncol(map)))
            filter <- filter & (map[,j] == pointList[[i]][j])
        selectedIndexes <- which(filter)
        if (length(selectedIndexes) > 0)
        {
            # Deal with replicates
            # Currently we only take the first one of the same point
            if (length(selectedIndexes) > 1)
                indexes <- c(indexes, selectedIndexes[1])
            else
                indexes <- c(indexes, selectedIndexes)
        }
    }
    return(indexes)
}


# Main entry for test
# 
# area_dimension <- c(0,0,10,10)
# sampling_nodes <- list(c(1,1),c(5,5))
# sampling_accuracy <- c(1,1)
# sample_points <-list()
# generateSamplePoints(area_dimension, sampling_nodes, sampling_accuracy)