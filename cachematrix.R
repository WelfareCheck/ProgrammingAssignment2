## Put comments here that give an overall description of what your
## functions do
## Coursera Week 3 Programming Assignment. Matrix inversion is a costly computation and so
## there is benefit to caching the inverse of a matrix, rather than compute repeatedly. The 
## following are a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## Write a short comment describing this function
    ## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    

}


cacheSolve <- function(x, ...) {
    ## Write a short comment describing this function
    ## The cacheSolve function computes the inverse of the special matrix returned by the
    ## makeCacheMatrix function. If the inverse has already been calculated (and the matrix
    ## has not changed), then cacheSolve retrieves the inverse from cache.
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getmean()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}

## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}