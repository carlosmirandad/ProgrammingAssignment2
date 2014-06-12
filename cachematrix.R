## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.  These functions help do just that.

## EXAMPLE:
##
## x1 = matrix(c(1,0,0,0,2,2,3,1,7),3,3)
##
## m1 <- makeCacheMatrix(x1)
##
## cacheSolve(m1)  # calculates the inverse, catches the result, and returns it
## cacheSolve(m1)  # getting cached data
## cacheSolve(m1)  # getting cached data
## cacheSolve(m1)  # getting cached data
## cacheSolve(m1)  # getting cached data
##
## solve(x1)       # just to validate results...


## makeCacheMatrix function: creates a special "matrix" object that can cache the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(minverse) minv <<- minverse
    getinverse <- function() minv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## cacheSolve function: computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function returns the catched result.
cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
