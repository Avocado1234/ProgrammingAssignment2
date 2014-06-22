## These two functions that are used to create a special object that stores a matrix and caches  inverse of that matrix
## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse. The "matrix" object is a list containing a function to :
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x=matrix(numeric(0),0,0)) { 
        inv <- NULL
        set <- function(y) {
                x <<- y
               inv <<- NULL
        }
        get <- function() x
        setinv <- function(inversion) inv <<- inversion
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##The function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above and sets the value of the inverse in the cache via setinv function 
##If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
