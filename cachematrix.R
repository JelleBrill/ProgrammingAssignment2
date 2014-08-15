## Lets a user create a cacheMatrix. Once the cacheSolve function has been run, the inverse of the matrix is cached
## example usuage:
## x <- makeCacheMatrix(1:4, 2, 2)
## cacheSolve(x)  ## calculates the inverse
## cacheSolve(x)  ## retrieves the inverse from the cach, no calculation needed
## set(x)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
	inverse <- NULL  ## upon creation, the inverse is unknown
	set <- function(y){
		x <<- y
		inverse <<- NULL  ##when the data is changed, the inverse becomes unknown
	}
	get <- function() x  ##returns the data
	setinverse <- function(newInverse) inverse <<- newInverse ##update the inverse
	getinverse <- function() inverse 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Returns the inverse of the matrix. If this function has been run earlier, a cached result is returned
## If the matrix has been changed, the cache is cleared and the inverse will have to be computed again

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if (!is.null(inverse)){
		message("getting cached data") ## as in the example. Can be deleted
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
