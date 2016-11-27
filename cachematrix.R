## These functions are used to optimize the performance of otherwise "expensive" inverse matrix computations

## makeCacheMatrix assigns functions to set and retrieve the inverse of a square matrix to an object in the Global Environment that can later be called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve retrieves cached inverse matrices that have already been solved OR computes and puts into the cache matrices that have not yet been solved

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached inverse matrix")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
