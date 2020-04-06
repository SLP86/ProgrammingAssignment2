## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   
        m <- NULL                                           ## define the matrix
        set <- function(y) {                                ## set the value of the matrix
                x <<- y                       
                m <<- NULL
        }
        get <- function() x                                 ## get the value of the matrix
        setInverse <- function(inverse)m <<- inverse        ## set the value of the inverse
        getInverse <- function()m                           ## get the value of the inverse                
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()                                 ## get the value of the inverse matrix from makeCacheMatrix
        if(!is.null(m)) {                                   ## if the inverse is NOT NULL
                message("getting cached data")              ## return message
                return(m)                                   ## return the inverse
        }                                                   ## else if the inverse is NULL
        data <- x$get()                                     ## get the original data
        m <- solve(data, ...)                               ## use solve function to inverse the matrix
        x$setInverse(m)                                     ## set the inverse
        m                                                   ## return the inverse
}
