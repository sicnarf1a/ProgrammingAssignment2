## Overall Description

## The following functions cache the value of the
## inverse matrix so that it can be looked up in the
## cache rather than recomputed when we need it again.


## makeCacheMatrix creates a "matrix", which is really a
## list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse_matrix
## 4. get the value of the inverse_matrix

makeCacheMatrix <- function(x = matrix()) {
    # set initial inverse_matrix as NULL
    inverse_matrix <- NULL
    # set input matrix
    set_matrix <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    # get current given matrix
    get_matrix <- function() x
    # set given inverse matrix
    set_inverse_matrix <- function(matrix) inverse_matrix <<- matrix
    # get invser matrix
    get_inverse_matrix <- function() inverse_matrix
    # output as type of list
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
}


## cacheSolve calulates the inverse matrix of the special
## "matrix" created with the above function.

## However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation.

## Otherwise, it calculates the inverse matrix of the data
## and sets the value of the inverse matrix in the cache 
## via the solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse_matrix()
    ## First check whether inverse matrix has already
    ## been calculated
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    ## If not, calculate the inver matrix of the data
    data <- x$get_matrix()
    inverse_matrix <- solve(data, ...)
    x$set_inverse_matrix(inverse_matrix)
    ## Return output as inverse matrix
    inverse_matrix
}
