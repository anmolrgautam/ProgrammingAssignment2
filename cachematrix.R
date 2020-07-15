## The overall function is calculating inverse of the matrix and cache it. 
## When there is a need of the inverse matrix again (matrix content is not changed),
## then it doesn't calculate the inverse again, returns the cached inverse matrix. 
## It saves runtime of the program.


## The function makeCacheMatrix() makes a list containing set the value of the matrix, get
## the value of the matrix, set the value of the inverse matrix, get the value of 
## inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL			## stores inverse matrix                                                                   
	set <- function(y) {		## function to set the matrix
		x <<- y			## stores matrix in "x" in parent environment
		invmat <<- NULL		## stores NULL in "invmat" in parent environment
	}
	get <- function() x		## function to get the matrix
	setinverse <- function(inverse) invmat <<- inverse		## function to set inverse matrix
	getinverse <- function() invmat					## function to get inverse matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	## cached matrix
}


## The function cacheSolve() calculates the inverse matrix of the list created with the
## above function. However, it first checks to see if the inverse matrix has already been 
## calculated. If so, it gets the cached inverse matrix and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse matrix in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmat <- x$getinverse()		## function to get inverse matrix in "invmat"
	if(!is.null(invmat)){			## condition to check "invmat" is NULL or not 
		message("getting cached data")	## if "invmat" is not NULL then return "invmat" ...
		return(invmat)			## with message
	}
	data <- x$get()				## function to get matrix in "data"
	invmat <- solve(data, ...)		## calculate inverse matrix of the "data"
	x$setinverse(invmat)			## function to set inverse matrix invmat
	invmat					## return inverse matrix
}
