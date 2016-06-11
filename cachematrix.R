## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

		## creates a list of functions to cache the inverse of a matrix passed into the function
		## the list contains 4 functions; get, set, getinverse, setinverse
		## result of this function is stored in an object, which is used by the cachedSolve function

		inverse <- matrix(, nrow = nrow(x), ncol=ncol(x))
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## the input to this function is the object created by the makeCacheMatrix function
		## if the inverse was previously calculated, then the cached value is retrieved
		
		inverse <- x$getinverse()
        if(!all(is.na(inverse))) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
		
}
