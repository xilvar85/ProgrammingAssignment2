## Put comments here that give an overall description of what your
## functions do
#Create a "special" matrix object that has method for caching its inverse. 
#The inverse of matrix will be computed if not already cached previously or if the matrix has changed.

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
#The returned special "matrix" has functions to set and get the matrix 
#and functions to set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    #for storing inverse of matrix
    i <- NULL
    #set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #get the current matrix
    get <- function() x
    #set the inverse of matrix
    setinverse <- function(inverse) i <<- inverse
    #get the inverse of current matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #get the inverse from cache
    i <- x$getinverse()
    #check if inverse has been computed before
    if(!is.null(i)) {
        message("getting cached data")
        #return previously computed inverse
        return(i)
    }
    #get matrix
    data <- x$get()
    #compute inverse
    i <- solve(data, ...)
    #store inverse into cache
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
