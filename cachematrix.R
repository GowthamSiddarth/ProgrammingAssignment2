## This script includes two functions necessary to create a special matrix which
## aids in caching the inverse of an invertible matrix.

## makeCacheMatrix is a function for creating a special 'matrix', which is a list
## containing functions to
##
## 1. Set the value of a matrix.
## 2. Get the value of the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inv_mat as null
    inv_mat <- NULL
    
    # a function to reinitialize or set new data into the matrix
    setMatrix <- function(new_matrix) {
        x <<- new_matrix
        inv_mat <<- NULL
    }
    
    # a function to obtain the matrix
    getMatrix <- function() x
    
    # a function to set the inverse of the matrix
    # Caution: Use setMatrix and setInvMatrix proportionately to avoid mistakes.
    setInvMatrix <- function(new_inv_matrix) inv_mat <<- new_inv_matrix
    
    # a function to get the value of inv_mat
    getInvMatrix <- function() inv_mat
    
    # return value of this function which is a list to all sub-functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## cacheSolve is a function which accepts parameters of type 'makeCacheMatrix' 
## to imminently calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
    # initialize with a function call returning the inv_mat attribute of x
    inv_mat <- x$getInvMatrix()
    
    # if the initialized value is not null, return the obtained data
    if(!is.null(inv_mat)) {
        message('getting cached inverse of the matrix')
        return(inv_mat)
    }
    
    # otherwise, get the data as a matrix with a function call of x
    data <- x$getMatrix()
    
    # calculate for inverse matrix on the obtained data
    inv_mat <- solve(data, ...)
    
    # set the result to the attribute in x with a function call belonging to x
    x$setInvMatrix(inv_mat)
    
    # return the value of inv_mat
    inv_mat
}
