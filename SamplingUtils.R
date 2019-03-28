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

# Generate coordinates of points for sampling
# area: A vector representing the sampling boundary, e.g. c(0,0,256,256)
# nodeList: List of coordinates of sampling nodes
# accuracy: Spatial resolution of sampling for each dimension
# radius: Broadening for each dimension during sampling
generateSamplePoints <- function(nodeList=list(),
                                 area=c(-Inf,-Inf,Inf,Inf),
                                 accuracy=c(1,1),
                                 radius=c(1,1))
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
        
        # Calculate coordinate for each dimension
        coordinates <- c()
        for (j in seq(1, dimension_count))
        {
            # Calculate primary coordinates along the center line
            coordinates_1D <- seqWithRep(lower_bound[j], upper_bound[j],
                                         accuracy[j], sampling_count)
            
            # Calculate bordened coordinates
            coordinates_1D <- brodeningSeq(coordinates_1D, 
                                           duplicity[j],
                                           accuracy[j])
            
            # Calculate duplicates - critical for alignate coordinates
            coordinates_1D_duplicates <- 
                        prod(duplicity[seq(1, dimension_count) != j] * 2 - 1)
            coordinates_1D <- rep(coordinates_1D,
                                  each = coordinates_1D_duplicates)
            
            # Append the coordinates of this dimension to the whole matrix
            coordinates <- cbind(coordinates, coordinates_1D)
        }
        
        sample_point_matrix <- rbind(sample_point_matrix, coordinates)
    }
    
    # Remove duplicated coordinates (lines)
    sample_point_matrix <- unique.matrix(sample_point_matrix, MARGIN = 1)
    # colnames(sample_point_matrix) <- paste0("dim", seq(1, dimension_count))
    
    # Convert matrix to list
    sample_point_list <- split(sample_point_matrix, 
                               rep(1:nrow(sample_point_matrix)))
    
    return(sample_point_list)
}

# Main entry for test
# 
# area_dimension <- c(0,0,10,10)
# sampling_nodes <- list(c(1,1),c(5,5))
# sampling_accuracy <- c(1,1)
# sample_points <-list()
# generateSamplePoints(area_dimension, sampling_nodes, sampling_accuracy)