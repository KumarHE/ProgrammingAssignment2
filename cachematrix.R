## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The assignment is to write a pair of functions that cache the 
## inverse of a matrix.
##
###############################################################
## Working example :
## > source(("cachematrix.R"))
## > bg <- matrix(c(0,2,1,0),nrow = 2,ncol = 2,byrow = TRUE)
## > mcM <- makeCacheMatrix()
## > mcM$set(bg)
## > cacheSolve(mcM)
## [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## > cacheSolve(mcM)
## getting cached data
## [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## > 
## #############################################################


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ##print(class(x))
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
