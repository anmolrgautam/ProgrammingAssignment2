## The overall function is calculating inverse of the matrix and cache it. 
## When there is a need of the inverse matrix again (matrix content is not changed),
## then it doesn't calculate the inverse again, returns the cached inverse matrix. 
## It saves runtime of the program.


## The function makeCacheMatrix() makes a list containing set the value of the matrix, get
## the value of the matrix, set the value of the inverse matrix, get the value of 
## inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL
	set <- function(y) {
		x <<- y
		invmat <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invmat <<- inverse
	getinverse <- function() invmat
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve() calculates the inverse matrix of the list created with the
## above function. However, it first checks to see if the inverse matrix has already been 
## calculated. If so, it gets the cached inverse matrix and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse matrix in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmat <- x$getinverse()
	if(!is.null(invmat)){
		message("getting cached data")
		return(invmat)
	}
	data <- x$get()
	invmat <- solve(data, ...)
	x$setinverse(invmat)
	invmat
}
